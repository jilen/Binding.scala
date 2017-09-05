/*
The MIT License (MIT)

Copyright (c) 2016 Yang Bo & REA Group Ltd.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

package com.thoughtworks.binding

import Binding.{BindingSeq, Constants, MultiMountPoint, SingleMountPoint}
import dom.Runtime.NodeSeqMountPoint
import com.thoughtworks.Extractor._
import com.thoughtworks.binding.XmlExtractor.{PrefixedName, UnprefixedName}
import com.thoughtworks.sde.core.Preprocessor
import macrocompat.bundle
import org.scalajs.dom.raw._

import scala.annotation.{StaticAnnotation, compileTimeOnly, tailrec}
import scala.collection.GenSeq
import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import scalatags.JsDom
import scalatags.jsdom
import org.scalajs.dom.document

import scala.collection.immutable.Queue
import scala.scalajs.runtime.AnonFunction1

/**
* Enable scalatag dom constructor for Binding
*
*/
@compileTimeOnly("enable macro paradise to expand macro annotations")
class scalatag extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro scalatag.Macros.macroTransform
}

object scalatag {


  @bundle
  private[scalatag] final class Macros(context: whitebox.Context) extends Preprocessor(context) with XmlExtractor {

    import c.universe._

    def macroTransform(annottees: Tree*): Tree = {


      val transformer = new ComprehensionTransformer {

        private def transformModifier(tree: Tree): (Queue[ValDef], Tree) = tree match {
          case transformedWithValDefs.extract(queue, tree) =>
            (queue, tree)
          case transformed.extract(transformedTree) =>
            Queue.empty -> transformedTree
          case _ =>
            Queue.empty -> super.transform(tree)
        }

        private def nodeSeq(children: Seq[Tree]): (Queue[ValDef], Tree) = {
          children match {
            case Seq() =>
              Queue.empty -> q"""_root_.com.thoughtworks.binding.Binding.Constants.empty"""
            case Seq(child) =>
              val (valDefs, transformedChild) = transformModifier(child)
              valDefs -> atPos(child.pos) {
                q"""_root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq($transformedChild)"""
              }
            case _ =>
              val transformedPairs = (for {
                child <- children
              } yield {
                val (valDefs, transformedChild) = transformModifier(child)
                valDefs -> atPos(child.pos) {
                  q"""
                    _root_.com.thoughtworks.binding.Binding.apply {
                      _root_.com.thoughtworks.binding.dom.Runtime.domBindingSeq($transformedChild)
                    }
                  """
                }
              }) (collection.breakOut(Queue.canBuildFrom))
              val (valDefs, transformedChildren) = transformedPairs.unzip
              valDefs.flatten -> q"""_root_.com.thoughtworks.binding.Binding.Constants(..$transformedChildren).flatMapBinding(_root_.scala.Predef.locally _)"""
          }
        }

        private def transformed: PartialFunction[Tree, Tree] = {
          case Block(stats, expr) =>
            super.transform(Block(stats.flatMap {
              case transformedWithValDefs.extract((valDefs, transformedTree)) =>
                valDefs.enqueue(transformedTree)
              case stat =>
                Seq(stat)
            }, expr))
        }

        private def isModifier(tree: Tree) = {
          c.typecheck(tree).tpe <:< typeOf[scalatags.generic.Modifier[_]]
        }

        def transformedWithValDefs: PartialFunction[Tree, (Queue[ValDef], Tree)] = {
          case tree@q"$tag(..$children)" if isModifier(tag) =>
            println(showRaw(tree))
            val elementName =  TermName(c.freshName("newValue"))
            val (valDefs, transformedChild) = children match {
              case Seq() => Queue.empty -> Nil
              case _ =>
                val (valDefs, transformedChildren) = nodeSeq(children)
                valDefs -> List(atPos(tree.pos) {
                  q"""
                  _root_.com.thoughtworks.sde.core.MonadicFactory.Instructions.each[
                    _root_.com.thoughtworks.binding.Binding,
                    _root_.scala.Unit
                  ](
                    new _root_.com.thoughtworks.binding.dom.Runtime.NodeSeqMountPoint(
                      $elementName,
                      $transformedChildren
                    )
                  )
                  """
                })
            }

            val elementDef = q"val $elementName = $tag.render"
            valDefs -> q"""
                  $elementDef
                  ..$transformedChild
                  $elementName
                """

        }
        override def transform(tree: Tree): Tree = {
          tree match {
            case transformedWithValDefs.extract((valDefs, transformedTree)) =>
              q"""
                ..$valDefs
                $transformedTree
              """
            case transformed.extract(transformedTree) =>
              transformedTree
            case _ =>
              super.transform(tree)
          }
        }
      }



      import transformer.transform
      replaceDefBody(annottees, { body =>
        q"""_root_.com.thoughtworks.binding.Binding.apply{
          import _root_.com.thoughtworks.binding.dom.AutoImports.{
            != => _,
            ## => _,
            == => _,
            eq => _,
            equals => _,
            getClass => _,
            hashCode => _,
            ne => _,
            notify => _,
            notifyAll => _,
            synchronized => _,
            toString => _,
            wait => _,
            _
          }
          ${transform(body)}
        }"""
      })
    }

  }

}
