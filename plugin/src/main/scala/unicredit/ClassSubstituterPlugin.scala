package unicredit

import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }
import scala.tools.nsc.transform.{ Transform, TypingTransformers }
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.ast.TreeDSL

import java.nio.file.Files.readAllBytes
import java.nio.file.Paths.get

import scala.collection.mutable
import scala.util.{Try => STry, Success, Failure}

class ClassSubstituterPlugin(val global: Global) extends Plugin {
  import global._

  val name = "class-substituter-plugin"
  val description = "Want to substitute class with alternative implementations"
  val components = List[PluginComponent](ClassSubstituterComponent)

  lazy val config: mutable.Set[(String, String)] =
    (try new String(readAllBytes(get("./class_substitution.config"))).split("\n").toSeq.map(e => {
       val splitted = e.split(" ")
       (splitted(0), splitted(1))
    })
     catch {
       case err: Throwable =>
         println("Class Substituter configuration file is missing")
         Seq()
     }).to[mutable.Set]

  private object ClassSubstituterComponent extends PluginComponent {
    val global = ClassSubstituterPlugin.this.global
    import global._

    override val runsAfter = List("typer")

    val phaseName = "class-substituter"

    override def newPhase(prev: Phase): StdPhase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit) {

        config.foreach { (c: (String, String)) =>
          val originalClass = STry {
            rootMirror.getClassByName((c._1: TypeName))
          }
          val alternativeImpl = STry {
            rootMirror.getClassByName((c._2: TypeName))
          }
          (originalClass, alternativeImpl) match {
            case (Success(originalClassSym), Success(alternativeImplSym)) =>
              //classSym.addAnnotation(annotationSym)
              originalClassSym.overridingSymbol(alternativeImplSym)
              
              originalClassSym.owner
              alternativeImplSym.owner_= originalClassSym.owner
              alternativeImplSym.name_= alternativeImplSym 
              unit.warning(null, s"substituting ${c._1} with ${c._2}")
            case _ =>
              unit.warning(null, s"CLASS SUBSTITUTER ERROR: ${c._1} or ${c._2} not found")
          }
        }
      }
    }
  }
}
