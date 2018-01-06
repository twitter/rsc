// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).

package java {
  package io {
    class Serializable()
  }

  package lang {
    class ArrayIndexOutOfBoundsException(message: String)
    object Character {
      def charCount(x: Int): Int
      def codePointAt(x: Int): Int
      def codePointBefore(x: Int): Int
      def isLowerCase(x: Char): Boolean
      def isUpperCase(x: Char): Boolean
      def toChars(x: Int): Array[Char]
    }
    class CharSequence() {
      def charAt(x: Int): Char
      def length(): Int
      def subSequence(x: Int, y: Int): CharSequence
      def toString(): String
    }
    class IllegalArgumentException(message: String)
    class IllegalStateException(message: String)
    class IndexOutOfBoundsException(message: String)
    object Integer {
      def valueOf(x: String, y: Int): Int
      def toHexString(x: Int): String
    }
    class NullPointerException(message: String)
    class RuntimeException()
    class String() {
      def apply(x: Int): Char
      def charAt(x: Int): Char
      def codePointAt(x: Int): Int
      def codePointCount(x: Int, y: Int): Int
      def getBytes(): Array[Byte]
      def indexOf(x: Char): Int
      def isEmpty(): Boolean
      def length(): Int
      def startsWith(x: String): Boolean
      def substring(x: Int, y: Int): String
    }
    object String {
      def valueOf(x: Char): String
    }
    class StringBuffer() {
      def append(x: Any): StringBuffer
    }
    class StringBuilder() {
      def append(x: Any): StringBuilder
      def appendCodePoint(x: Int): StringBuilder
      def indexOf(x: String, y: Int): Int
      def length(): Int
    }
    object System {
      def arraycopy(
          src: Any,
          srcPos: Int,
          dest: Any,
          destPos: Int,
          length: Int): Unit
    }
  }

  package util {
    object Arrays {
      def fill(x: Any, y: Any): Unit
    }
    class ArrayList[T]() {
      def add(x: T): Boolean
      def clear(): Unit
      def get(x: Int): T
      def isEmpty(): Boolean
      def iterator(): Iterator[T]
      def remove(x: Any): Boolean
      def removeRange(x: Int, y: Int): Unit
      def set(x: Int, y: T): Unit
      def size(): Int
      def subList(x: Int, y: Int): ArrayList[T]
      def toArray(x: Any): Array[T]
    }
    class HashMap[T, U]() {
      def get(x: T): U
      def put(x: T, y: U): U
    }
    class Iterator[T]() {
      def hasNext(): Boolean
      def next(): T
    }
    class List[T]() {
      def add(x: T): Boolean
    }
    class Map[T, U]() {
      def get(x: T): U
      def put(x: T, y: U): U
    }
  }
}

package scala {
  class Any() {
    def +(x: Any): String
    def ==(x: Any): Boolean
    def !=(x: Any): Boolean
    def equals(x: Any): Boolean
    def toString(): String
  }
  class AnyVal() extends Any()
  class AnyRef() extends Any() {
    def synchronized[T](x: T): T
  }
  class Array[T]() {
    val length: Int
    def apply(x: Int): T
    def update(x: Int, y: Int): Unit
  }
  object Array {
    def apply[T](x: T*): Array[T]
  }
  class Boolean() extends AnyVal() {
    def apply(): Boolean
    def !(): Boolean
    def &&(x: Boolean): Boolean
    def ||(x: Boolean): Boolean
  }
  class Byte() extends AnyVal() {
    def &(x: Any): Byte
  }
  class Char() extends AnyVal() {
    def <(x: Any): Boolean
    def <=(x: Any): Boolean
    def >(x: Any): Boolean
    def >=(x: Any): Boolean
    def -(x: Any): Char
    def +(x: Any): Char
  }
  class Float() extends AnyVal()
  class Double() extends AnyVal()
  class Function0[+R]() {
    def apply(): R
  }
  class Function1[-T1, +R]() {
    def apply(x: T1): R
  }
  class Function2[-T1, -T2, +R]()
  class Function3[-T1, -T2, -T3, +R]()
  class Function4[-T1, -T2, -T3, -T4, +R]()
  class Int() extends Any() {
    def ~(x: Any): Int
    def &(x: Any): Int
    def |(x: Any): Int
    def ^(x: Any): Int
    def <<(x: Any): Int
    def >>(x: Any): Int
    def <(x: Any): Boolean
    def <=(x: Any): Boolean
    def >(x: Any): Boolean
    def >=(x: Any): Boolean
    def +(x: Any): Int
    def -(x: Any): Int
    def *(x: Any): Int
    def /(x: Any): Int
    def %(x: Any): Int
    def toChar(): Int
    def toShort(): Short
  }
  class Nothing()
  class Long() extends AnyVal()
  class Seq[T]()
  class Short() extends AnyVal()
  class Tuple1[+T1]()
  class Tuple2[+T1, +T2]()
  class Tuple3[+T1, +T2, +T3]()
  class Tuple4[+T1, +T2, +T3, +T4]()
  class Unit() extends AnyVal()
  object Predef {
    type String = _root_.java.lang.String
  }
}
