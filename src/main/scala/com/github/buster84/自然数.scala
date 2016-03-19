package com.github.buster84
import scala.language.experimental.macros
import scala.language.dynamics
import scala.reflect.macros.whitebox.Context
import scala.annotation.tailrec

sealed trait 自然数 {
  type 足す[N <: 自然数] <: 自然数
}

trait 零 extends 自然数 {
  override type 足す[N <: 自然数] = N
}

trait 後者[N<:自然数] extends 自然数 {
  override type 足す[O <: 自然数] = 後者[N#足す[O]]
}

object Nat {
  def toNat(i: Int): 自然数 = macro materializeWidened

  def materializeWidened(c: Context)(i: c.Expr[Int]) = {
    import c.universe._
    i match {
      case Literal(Constant(n: Int)) => { 
        def mkNatTpt(i: Int): Tree = {
          val succSym = typeOf[後者[_]].typeConstructor.typeSymbol
          val _0Sym = typeOf[零].typeSymbol

          @tailrec
          def loop(i: Int, acc: Tree): Tree = {
            if(i == 0) acc
            else loop(i-1, AppliedTypeTree(Ident(succSym), List(acc)))
          }

          loop(i, Ident(_0Sym))
        }
        q""" new ${mkNatTpt(n)} """
      }
      case _ =>
        c.abort(c.enclosingPosition, s"Expression $i does not evaluate to a non-negative Int literal")
    }
  }
}
