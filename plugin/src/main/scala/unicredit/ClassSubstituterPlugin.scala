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
import scala.util.{ Try => STry, Success, Failure }

class ClassSubstituterPlugin(val global: Global) extends Plugin {
  self =>
  import global._

  val name = "class-substituter-plugin"
  val description = "Want to substitute class with alternative implementations"
  val components = List[PluginComponent](ClassRegisterComponent, ClassSubstituterComponent)

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

  val registered: mutable.HashMap[Any, Any] = new mutable.HashMap()

  private object ClassRegisterComponent extends PluginComponent with Transform with TreeDSL {
    val global = ClassSubstituterPlugin.this.global
    import global._
    import global.definitions._

    override val runsAfter = List("typer")

    val phaseName = "class-registerer"

    def newTransformer(unit: CompilationUnit) =
      new AggregateRegisterTransformer(unit)

    class AggregateRegisterTransformer(unit: CompilationUnit) extends Transformer {

      val classSubstitution =
        config.flatMap { (c: (String, String)) =>
          val originalClass = STry {
            rootMirror.getClassByName((c._1: TypeName))
          }
          val alternativeImpl = STry {
            rootMirror.getClassByName((c._2: TypeName))
          }
          (originalClass, alternativeImpl) match {
            case (Success(originalClassSym), Success(alternativeImplSym)) =>
              Some(new RegisterTransformer(unit, originalClassSym, alternativeImplSym))
            case _ =>
              None
          }
        }

      class RegisterTransformer(unit: CompilationUnit, val originalClassSym: ClassSymbol, val alternativeImplSym: ClassSymbol) {

        def check(tree: Tree): Boolean = {
          tree match {
            case cd @ ClassDef(Modifiers(flags, privateWithin, annotations), name, params, typeDef) 
              if (alternativeImplSym == cd.symbol) =>
              true
            case any =>
              false
          }
        }
      }

      override def transform(tree: Tree): Tree = {
        val iter = classSubstitution.iterator
        var last: Option[RegisterTransformer] = None
        var found: Option[RegisterTransformer] = None
        
        if (iter.hasNext)
          last = Some(iter.next)
        
        while (last.isDefined && found.isEmpty) {
          if (last.get.check(tree))
            found = last

          if (iter.hasNext)
            last = Some(iter.next)
          else
            last = None
        }
        if (found.isEmpty)
          super.transform(tree)
        else {
          self.registered += 
            (found.get.originalClassSym -> tree)
          unit.warning(tree.pos, s"Registering class tree of ${found.get.alternativeImplSym}")
          Literal(Constant(())) setType UnitTpe
        }
      }
    }
  }
  
  
  private object ClassSubstituterComponent extends PluginComponent with Transform with TreeDSL {
    val global = ClassSubstituterPlugin.this.global
    import global._
    import global.definitions._

    override val runsAfter = List("class-registerer")

    val phaseName = "class-substituter"

    def newTransformer(unit: CompilationUnit) =
      new AggregateSubstituterTransformer(unit)
  
  class AggregateSubstituterTransformer(unit: CompilationUnit) extends Transformer {

      override def transform(tree: Tree): Tree = {
        tree match {
          case cd @ ClassDef(Modifiers(flags, privateWithin, annotations), name, params, typeDef)
              if (registered.contains(cd.symbol)) =>
                
             registered(cd.symbol).asInstanceOf[ClassDef] match {
               case fixed @ ClassDef(fixedMods, _, fixedParams, fixedTypeDef) =>
                 val hacked = ClassDef(fixedMods, cd.name, fixedParams, fixedTypeDef)
                 hacked.setPos(tree.pos)
                 hacked.setSymbol(cd.symbol)
                 hacked
               case _ => 
                 unit.error(tree.pos, "must not happens")
                 super.transform(tree)
             }
          case _ =>
            super.transform(tree)
        }
      }
    }
  }
  
}
