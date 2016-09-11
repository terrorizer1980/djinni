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
import djinni.writer.IndentWriter

class JsGenerator(spec: Spec) extends Generator(spec) {

  val marshal = new CppMarshal(spec)

  def writeJsFile(name: String, origin: String, includes: Iterable[String], f: IndentWriter => Unit) =
    writeJsFileGeneric(spec.cppOutFolder.get, spec.cppNamespace, spec.cppFileIdentStyle)(name, origin, includes, f)

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

    writeJsFile(ident, origin, includes, w => {
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
    val self = marshal.typename(ident, i)
    val wrapper = self + "Wrapper"
    val includes = List("#include "+q(spec.cppFileIdentStyle(ident) + "." + spec.cppHeaderExt))

    writeJsFile(ident, origin, includes, w => {
      // write the emscripten interface wrapper
      if (i.ext.js) {
        // TODO(andy): wrap this in an anonymous namespace?
        w.w(s"class $wrapper : public emscripten::wrapper<$self>").braced {
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
          if (i.ext.js)
            w.wl(s".allow_subclass<$wrapper>(${q(wrapper)})")
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
