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
  val allStlContainerTypes = mutable.Set[MExpr]()

  def writeJsFile(name: String, origin: String, includes: Iterable[String], f: IndentWriter => Unit) =
    writeJsFileGeneric(spec.jsOutFolder.get, spec.cppNamespace, spec.cppFileIdentStyle)(name, origin, includes, f)

  def writeNestedWrappers(m : MExpr, w: IndentWriter) {
    val typeName = marshal.typename(m)
    val argsName = m.args.map(marshal.typename).mkString(",")
    m.base match {
      case MList =>
        w.wl(s"emscripten::register_vector<$argsName>(${q(typeName)});")
      case MMap =>
        w.wl(s"emscripten::register_map<$argsName>(${q(typeName)});")
      case MOptional =>
        w.wl(s"emscripten::class_<$typeName>(${q(typeName)})").nested {
          w.wl(".template constructor<>()")
          w.wl(s".constructor<$argsName>()")
          w.wl(s".function(${q("get")}, &optional_get<$argsName>)")
          w.wl(s".function(${q("is_set")}, &optional_is_set<$argsName>)")
          w.wl(";")
        }
      case _ =>
    }
  }

  def genStlWrappers() {
    val refs = new CppRefs("Stl")
    allStlContainerTypes.foreach(c => refs.find(c, false))

    writeJsFile("ContainerWrappers", "", refs.hpp, w => {
      w.wl("""namespace {
         |    template<typename T>
         |    T& optional_get(boost::optional<T>& opt) {
         |         return opt.get();
         |    }
         |    template<typename T>
         |    bool optional_is_set(boost::optional<T>& opt) {
         |         return static_cast<bool>(opt);
         |    }
         |}  // namespace""".stripMargin)
       w.wl
       w.w("EMSCRIPTEN_BINDINGS(stl_bindings)").braced {
         allStlContainerTypes.foreach((expr : MExpr) => writeNestedWrappers(expr, w))
       }
    })
  }

  override def generate(idl: Seq[TypeDecl]) {
    super.generate(idl)
    genStlWrappers()
  }

  class CppRefs(name: String) {
    var hpp = mutable.TreeSet[String]()
    var stlContainerTypes = mutable.Set[MExpr]()

    def find(ty: TypeRef, forwardDeclareOnly: Boolean) { find(ty.resolved, forwardDeclareOnly) }
    def find(tm: MExpr, forwardDeclareOnly: Boolean) {
      stlContainerTypes.add(tm)
      tm.args.foreach((x) => find(x, forwardDeclareOnly))
      find(tm.base, forwardDeclareOnly)
    }
    def find(m: Meta, forwardDeclareOnly : Boolean) = {
      m match {
        case d: MDef =>
          hpp.add("#include " + q(spec.jsBaseLibIncludePrefix + spec.cppFileIdentStyle(d.name) + "." + spec.cppHeaderExt))
        case _ =>
      }
    }
  }


  override def generateEnum(origin: String, ident: Ident, doc: Doc, e: Enum) {
    val self = marshal.typename(ident, e)
    val includes = List("#include "+q(spec.jsBaseLibIncludePrefix + spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt))

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
    val selfHeaderInclude = "#include "+q(spec.jsBaseLibIncludePrefix + spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt)
    val refs = new CppRefs(ident.name)
    r.fields.foreach(f => refs.find(f.ty, false))
    allStlContainerTypes ++= refs.stlContainerTypes

    writeJsFile(ident, origin, refs.hpp + selfHeaderInclude, w => {
      w.w(s"EMSCRIPTEN_BINDINGS($actualSelf)").braced {
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

  override def generateInterface(origin: String, ident: Ident, doc: Doc, typeParams: Seq[TypeParam], i: Interface) {
    val refs = new CppRefs(ident.name)
    i.methods.map(m => {
      m.params.map(p => refs.find(p.ty, true))
      m.ret.foreach((x)=>refs.find(x, true))
    })
    i.consts.map(c => {
      refs.find(c.ty, true)
    })
    allStlContainerTypes ++= refs.stlContainerTypes

    val self = marshal.typename(ident, i)
    val wrapper = self + "Wrapper"
    val selfHeaderInclude = "#include " + q(spec.jsBaseLibIncludePrefix + spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt)


    writeJsFile(ident, origin, refs.hpp + selfHeaderInclude, w => {
      // write the emscripten interface wrapper
      if (i.ext.js) {
        wrapAnonymousNamespace(w, w => {
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
        })
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
