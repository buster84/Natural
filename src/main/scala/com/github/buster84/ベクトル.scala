package com.github.buster84
import scala.language.experimental.macros

import scala.annotation.tailrec
import scala.language.dynamics
import scala.reflect.macros.whitebox.Context
// import scala.reflect.macros.blackbox.Context

sealed trait ベクトル集合[個数 <: 自然数] {
  def +( vector: ベクトル集合[個数] ): ベクトル集合[個数]
  def toString: String
  def numStr: String
  def append[Size <: 自然数]( vector: ベクトル集合[Size] ): ベクトル集合[個数#足す[Size]]
}

case object 零次元ベクトル extends ベクトル集合[零] {
  def +( vector: ベクトル集合[零] ): ベクトル集合[零] = 零次元ベクトル
  override def toString: String = "零次元ベクトル"
  def numStr: String = ""
  def append[Size <: 自然数]( vector: ベクトル集合[Size] ): ベクトル集合[零#足す[Size]] = vector
}


case class ベクトル[残り個数 <: 自然数]( 最初: Int, 残り: ベクトル集合[残り個数]) extends ベクトル集合[後者[残り個数]] {
  def +( vector: ベクトル集合[後者[残り個数]] ): ベクトル集合[後者[残り個数]] = {
    vector match {
      case ベクトル( head, tail ) => ベクトル[残り個数]( 最初 + head, 残り + tail)
    }
  }
  override def toString: String = s"ベクトル(${numStr})"

  def numStr: String = {
    残り match {
      case _ :零次元ベクトル.type => 最初.toString
      case _ => s"${最初.toString}, ${残り.numStr}"
    }
  }
  def append[Size <: 自然数]( vector: ベクトル集合[Size] ): ベクトル集合[後者[残り個数]#足す[Size]] = 
    new ベクトル[残り個数#足す[Size]]( 最初, 残り.append( vector ) )
}

object ベクトル extends Dynamic {

  type _0 = 零
  type _1 = 後者[零]



  def apply[個数 <: 自然数]( numbers: Int* ): ベクトル集合[個数] = macro ベクトルマクロ実装[個数]

  def ベクトルマクロ実装[個数 <: 自然数]( c: Context )(numbers: c.Expr[Int]*) = {
    import c.universe._

    val succTpe = typeOf[後者[_]].typeConstructor
    val _0Tpe = typeOf[_0]

    if ( numbers.isEmpty ) c.abort( c.enclosingPosition, "Need more than or equl to 1 Integer!!!" )
    (numbers.foldRight((_0Tpe,q"""_root_.com.github.buster84.零次元ベクトル""": Tree)){
      case ( literal, (numTpe, accTree )) =>
        (appliedType(succTpe, numTpe), q"""new _root_.com.github.buster84.ベクトル[${numTpe}]( ${literal},  ${accTree})""")
    })._2
  }


  def applyDynamic(method: String)(numbers: Int*): Any = macro forwardToApply

  def forwardToApply(c: Context)(method: c.Expr[String])(numbers: c.Expr[Int]*) = {
    import c.universe._
    val methodName = method.tree match {
      case Literal(Constant(name: String)) => name
      case _ => c.abort( c.enclosingPosition, "method should be String!!")
    }
    if(!methodName.startsWith("create")) c.abort(c.enclosingPosition, s"Method name should start with 'create'")

    val size = methodName.drop("create".size).toInt

    if(size != numbers.size) c.abort(c.enclosingPosition, s"Argument size[${numbers.size}] must be same as ${methodName}")

    val succSym = typeOf[後者[_]].typeConstructor.typeSymbol
    val _0Sym = typeOf[_0].typeSymbol

    @tailrec
    def loop(i: Int, acc: Tree): Tree = {
      if(i == 0) acc
      else loop(i-1, AppliedTypeTree(Ident(succSym), List(acc)))
    }

    val numTypeTree = loop(size, Ident(_0Sym))
    val numbersTree = numbers.map(_.tree).toList
    Apply(TypeApply(Select(Ident(TermName("ベクトル")), TermName("apply")), List(numTypeTree)), numbersTree)
  }
}
