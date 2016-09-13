/**
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *    http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

package djinni

import djinni.ast._
import djinni.generatorTools._
import djinni.meta._
import djinni.writer.IndentWriter

import scala.collection.mutable

class JsGenerator(spec: Spec) extends Generator(spec) {

  val marshal = new CppMarshal(spec)

  def writeJsFile(name: String, origin: String, includes: Iterable[String], f: IndentWriter => Unit) =
    writeJsFileGeneric(spec.cppOutFolder.get, spec.cppNamespace, spec.cppFileIdentStyle)(name, origin, includes, f)

  class CppRefs(name: String) {
    var hpp = mutable.TreeSet[String]()
    var hppFwds = mutable.TreeSet[String]()
    var cpp = mutable.TreeSet[String]()

    def find(ty: TypeRef, forwardDeclareOnly: Boolean) { find(ty.resolved, forwardDeclareOnly) }
    def find(tm: MExpr, forwardDeclareOnly: Boolean) {
      tm.args.foreach((x) => find(x, forwardDeclareOnly))
      find(tm.base, forwardDeclareOnly)
    }
    def find(m: Meta, forwardDeclareOnly : Boolean) = {
      for(r <- marshal.hppReferences(m, name, forwardDeclareOnly)) r match {
        case ImportRef(arg) => hpp.add("#include " + arg)
        case DeclRef(decl, Some(spec.cppNamespace)) => hppFwds.add(decl)
        case DeclRef(_, _) =>
      }
      for(r <- marshal.cppReferences(m, name, forwardDeclareOnly)) r match {
        case ImportRef(arg) => cpp.add("#include " + arg)
        case DeclRef(_, _) =>
      }
    }
  }


  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum) {
    val self = marshal.typename(ident, e)
    val includes = List("#include "+q(spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt))

    writeJsFile(ident, origin, includes, w => {
      w.w(s"EMSCRIPTEN_BINDINGS($self)").braced {
        w.wl(s"emscripten::enum_<$self>(${q(self)})").nested {
          e.options.foreach(o => {
            val name = idCpp.enum(o.ident.name)
            w.wl(s".value(${q(name)}, $self::$name)")
          })
          w.wl(";")
        }
      }
    })
  }

  override def generateRecord(origin: String, ident: Ident, doc: Doc, params: Seq[TypeParam], r: Record) {
    val self = marshal.typename(ident, r)
    val cppName = if (r.ext.cpp) ident.name + "_base" else ident.name
    val actualSelf = marshal.typename(cppName, r)
    val includes = List("#include "+q(spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt))
    val vectors = new mutable.TreeSet()

    r.fields.foreach(p => {
      p.ty.resolved.base match {
        case MList =>
          // TODO(andy): add support for nested types here?
          vectors.add(s"emscripten::register_vector<${ getVectorType(p.ty.resolved.args.head) }>(${q(getVectorName(p.ty.resolved))});")
        case _ =>
          println("nothing")
      }
    })


    writeJsFile(ident, origin, includes, w => {
      w.w(s"EMSCRIPTEN_BINDINGS($actualSelf)").braced {
        vectors.foreach(w.wl)
        w.wl(s"emscripten::class_<$actualSelf>(${q(actualSelf)})").nested {
          val fieldTypeString = r.fields.map(f => marshal.fieldType(f.ty)).mkString(", ")
          w.wl(s".constructor<$fieldTypeString>()")
          r.fields.foreach(f => {
            val fieldName = idCpp.local(f.ident)
            w.wl(s".property(${q(fieldName)}, &$actualSelf::$fieldName)")
          })
          w.wl(";")
        }
      }
    })
  }

  def getVectorName(ty : MExpr): String = {
    ty.base match {
      case MList =>
        "Vector" + (if (ty.args.nonEmpty) getVectorName(ty.args.head) else "")
      case MString =>
        "String"
    }
  }

  def getVectorType(ty : MExpr): String = {
    ty.base match {
      case MString =>
        "std::string"
    }
  }

  override def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface) {
    val refs = new CppRefs(ident.name)
    i.methods.map(m => {
      m.params.map(p => refs.find(p.ty, true))
      m.ret.foreach((x)=>refs.find(x, true))
    })
    i.consts.map(c => {
      refs.find(c.ty, true)
    })

    val self = marshal.typename(ident, i)
    val wrapper = self + "Wrapper"
    val selfHeaderInclude = "#include " + q(spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt)


    writeJsFile(ident, origin, refs.cpp + selfHeaderInclude, w => {
      // write the emscripten interface wrapper
      if (i.ext.js) {
        // TODO(andy): wrap this in an anonymous namespace?
        w.w(s"class $wrapper : public emscripten::wrapper<$self>").bracedSemi {
          w.wl("public:")
          w.wl(s"EMSCRIPTEN_WRAPPER($wrapper);")
          i.methods.foreach(m => {
            val returnType = marshal.returnType(m.ret)
            val name = idCpp.method(m.ident)
            val typedParams = m.params.map(p => marshal.paramType(p.ty) + " " + idCpp.local(p.ident))
            val invokeParams = List(q(name)) ++ m.params.map(p => idCpp.local(p.ident))
            w.w(s"$returnType $name(${typedParams.mkString(",")})").braced {
              w.wl(s"return call<$returnType>(${invokeParams.mkString(",")});")
            }
          })
        }
        w.wl
      }

      // write the bindings
      w.w(s"EMSCRIPTEN_BINDINGS($self)").braced {
        w.wl(s"emscripten::class_<$self>(${q(self)})").nested {
          if (i.ext.js) {
            w.wl(s".allow_subclass<$wrapper, std::shared_ptr<$wrapper>>(${q(wrapper)}, ${q(wrapper + "Ptr")})")
          }
          w.wl(s".smart_ptr<std::shared_ptr<$self>>(${ q(s"shared_ptr<$self>") })")
          i.methods.foreach(m => {
            val name = idCpp.method(m.ident)
            if (m.static)
              w.w(s".class_function(${q(name)}, &$self::$name")
            else
              w.w(s".function(${q(name)}, &$self::$name")
            if (i.ext.js)
              w.w(s", emscripten::pure_virtual()")
            w.wl(")")
          })
          w.wl(";")
        }
      }
    })
  }
}
