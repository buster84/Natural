package com.github.buster84

sealed trait 自然数 {
  type 足す[N <: 自然数] <: 自然数
}

trait 零 extends 自然数 {
  override type 足す[N <: 自然数] = N
}

trait 後者[N<:自然数] extends 自然数 {
  override type 足す[O <: 自然数] = 後者[N#足す[O]]
}
