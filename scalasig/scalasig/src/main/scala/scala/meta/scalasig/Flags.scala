// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/scala.
package scala.meta.scalasig

// NOTE: There is no specification for this aspect of ScalaSignatures.
// The best that we have is the Scala compiler source code:
// * https://github.com/scala/scala/blob/v2.11.12/src/reflect/scala/reflect/internal/Flags.scala
// * https://github.com/scala/scala/blob/v2.12.6/src/reflect/scala/reflect/internal/Flags.scala

trait Flags extends FlagNames with FlagValues

trait FlagValues {
  final val IMPLICIT = 1L << 0
  final val FINAL = 1L << 1
  final val PRIVATE = 1L << 2
  final val PROTECTED = 1L << 3
  final val SEALED = 1L << 4
  final val OVERRIDE = 1L << 5
  final val CASE = 1L << 6
  final val ABSTRACT = 1L << 7
  final val DEFERRED = 1L << 8
  final val METHOD = 1L << 9
  final val MODULE = 1L << 10
  final val INTERFACE = 1L << 11
  final val MUTABLE = 1L << 12
  final val PARAM = 1L << 13
  final val PACKAGE = 1L << 14
  final val MACRO = 1L << 15
  final val BYNAMEPARAM = 1L << 16
  final val CAPTURED = 1L << 16
  final val COVARIANT = 1L << 16
  final val CONTRAVARIANT = 1L << 17
  final val INCONSTRUCTOR = 1L << 17
  final val LABEL = 1L << 17
  final val ABSOVERRIDE = 1L << 18
  final val LOCAL = 1L << 19
  final val JAVA = 1L << 20
  final val SYNTHETIC = 1L << 21
  final val STABLE = 1L << 22
  final val STATIC = 1L << 23
  final val CASEACCESSOR = 1L << 24
  final val DEFAULTPARAM = 1L << 25
  final val TRAIT = 1L << 25
  final val BRIDGE = 1L << 26
  final val ACCESSOR = 1L << 27
  final val SUPERACCESSOR = 1L << 28
  final val PARAMACCESSOR = 1L << 29
  final val MODULEVAR = 1L << 30
  final val LAZY = 1L << 31
  final val IS_ERROR = 1L << 32
  final val OVERLOADED = 1L << 33
  final val LIFTED = 1L << 34
  final val EXISTENTIAL = 1L << 35
  final val MIXEDIN = 1L << 35
  final val EXPANDEDNAME = 1L << 36
  final val IMPLCLASS = 1L << 37
  final val PRESUPER = 1L << 37
  final val TRANS_FLAG = 1L << 38
  final val LOCKED = 1L << 39
  final val SPECIALIZED = 1L << 40
  final val DEFAULTINIT = 1L << 41
  final val VBRIDGE = 1L << 42
  final val VARARGS = 1L << 43
  final val TRIEDCOOKING = 1L << 44
  final val SYNCHRONIZED = 1L << 45
  final val ARTIFACT = 1L << 46
  final val JAVA_DEFAULT_METHOD = 1L << 47
  final val JAVA_ENUM = 1L << 48
  final val JAVA_ANNOTATION = 1L << 49
  final val SYNTHESIZE_IMPL_IN_SUBCLASS = 1L << 50
  final val notPROTECTED = 1L << 56
  final val notPRIVATE = 1L << 58
}

trait Flagged {
  def flags: Long
  def isImplicit = (flags & IMPLICIT) != 0
  def isFinal = (flags & FINAL) != 0
  def isPrivate = (flags & PRIVATE) != 0
  def isProtected = (flags & PROTECTED) != 0
  def isSealed = (flags & SEALED) != 0
  def isOverride = (flags & OVERRIDE) != 0
  def isCase = (flags & CASE) != 0
  def isAbstract = (flags & ABSTRACT) != 0
  def isDeferred = (flags & DEFERRED) != 0
  def isMethod = (flags & METHOD) != 0
  def isModule = (flags & MODULE) != 0
  def isInterface = (flags & INTERFACE) != 0
  def isMutable = (flags & MUTABLE) != 0
  def isParam = (flags & PARAM) != 0
  def isPackage = (flags & PACKAGE) != 0
  def isMacro = (flags & MACRO) != 0
  def isByNameParam = (flags & BYNAMEPARAM) != 0
  def isCaptured = (flags & CAPTURED) != 0
  def isCovariant = (flags & COVARIANT) != 0
  def isContravariant = (flags & CONTRAVARIANT) != 0
  def isInConstructor = (flags & INCONSTRUCTOR) != 0
  def isLabel = (flags & LABEL) != 0
  def isAbsOverride = (flags & ABSOVERRIDE) != 0
  def isLocal = (flags & LOCAL) != 0
  def isJava = (flags & JAVA) != 0
  def isSynthetic = (flags & SYNTHETIC) != 0
  def isStable = (flags & STABLE) != 0
  def isStatic = (flags & STATIC) != 0
  def isCaseAccessor = (flags & CASEACCESSOR) != 0
  def isDefaultParam = (flags & DEFAULTPARAM) != 0
  def isTrait = (flags & TRAIT) != 0
  def isBridge = (flags & BRIDGE) != 0
  def isAccessor = (flags & ACCESSOR) != 0
  def isSuperAccessor = (flags & SUPERACCESSOR) != 0
  def isParamAccessor = (flags & PARAMACCESSOR) != 0
  def isModuleVar = (flags & MODULEVAR) != 0
  def isLazy = (flags & LAZY) != 0
  def isError = (flags & IS_ERROR) != 0
  def isOverloaded = (flags & OVERLOADED) != 0
  def isLifted = (flags & LIFTED) != 0
  def isExistential = (flags & EXISTENTIAL) != 0
  def isMixedin = (flags & MIXEDIN) != 0
  def isExpandedName = (flags & EXPANDEDNAME) != 0
  def isImplClass = (flags & IMPLCLASS) != 0
  def isPresuper = (flags & PRESUPER) != 0
  def isTransFlag = (flags & TRANS_FLAG) != 0
  def isLocked = (flags & LOCKED) != 0
  def isSpecialized = (flags & SPECIALIZED) != 0
  def isDefaultInit = (flags & DEFAULTINIT) != 0
  def isVbridge = (flags & VBRIDGE) != 0
  def isVarargs = (flags & VARARGS) != 0
  def isTriedCooking = (flags & TRIEDCOOKING) != 0
  def isSynchronized = (flags & SYNCHRONIZED) != 0
  def isArtifact = (flags & ARTIFACT) != 0
  def isJavaDefaultMethod = (flags & JAVA_DEFAULT_METHOD) != 0
  def isJavaEnum = (flags & JAVA_ENUM) != 0
  def isJavaAnnotation = (flags & JAVA_ANNOTATION) != 0
  def isSynthesizeImplInSubclass = (flags & SYNTHESIZE_IMPL_IN_SUBCLASS) != 0
  def isNotProtected = (flags & notPROTECTED) != 0
  def isNotprivate = (flags & notPRIVATE) != 0
}

trait FlagNames extends FlagValues {
  val flagNames = scala.collection.mutable.Map[Long, String]()
  flagNames(IMPLICIT) = "IMPLICIT"
  flagNames(FINAL) = "FINAL"
  flagNames(PRIVATE) = "PRIVATE"
  flagNames(PROTECTED) = "PROTECTED"
  flagNames(SEALED) = "SEALED"
  flagNames(OVERRIDE) = "OVERRIDE"
  flagNames(CASE) = "CASE"
  flagNames(ABSTRACT) = "ABSTRACT"
  flagNames(DEFERRED) = "DEFERRED"
  flagNames(METHOD) = "METHOD"
  flagNames(MODULE) = "MODULE"
  flagNames(INTERFACE) = "INTERFACE"
  flagNames(MUTABLE) = "MUTABLE"
  flagNames(PARAM) = "PARAM"
  flagNames(PACKAGE) = "PACKAGE"
  flagNames(MACRO) = "MACRO"
  flagNames(1L << 16) = "BYNAMEPARAM/CAPTURED/COVARIANT"
  flagNames(1L << 17) = "CONTRAVARIANT/INCONSTRUCTOR/LABEL"
  flagNames(ABSOVERRIDE) = "ABSOVERRIDE"
  flagNames(LOCAL) = "LOCAL"
  flagNames(JAVA) = "JAVA"
  flagNames(SYNTHETIC) = "SYNTHETIC"
  flagNames(STABLE) = "STABLE"
  flagNames(STATIC) = "STATIC"
  flagNames(CASEACCESSOR) = "CASEACCESSOR"
  flagNames(1L << 25) = "DEFAULTPARAM/TRAIT"
  flagNames(BRIDGE) = "BRIDGE"
  flagNames(ACCESSOR) = "ACCESSOR"
  flagNames(SUPERACCESSOR) = "SUPERACCESSOR"
  flagNames(PARAMACCESSOR) = "PARAMACCESSOR"
  flagNames(MODULEVAR) = "MODULEVAR"
  flagNames(LAZY) = "LAZY"
  flagNames(IS_ERROR) = "IS_ERROR"
  flagNames(OVERLOADED) = "OVERLOADED"
  flagNames(LIFTED) = "LIFTED"
  flagNames(1L << 35) = "EXISTENTIAL/MIXEDIN"
  flagNames(EXPANDEDNAME) = "EXPANDEDNAME"
  flagNames(1L << 37) = "IMPLCLASS"
  flagNames(PRESUPER) = "PRESUPER"
  flagNames(TRANS_FLAG) = "TRANS_FLAG"
  flagNames(LOCKED) = "LOCKED"
  flagNames(SPECIALIZED) = "SPECIALIZED"
  flagNames(DEFAULTINIT) = "DEFAULTINIT"
  flagNames(VBRIDGE) = "VBRIDGE"
  flagNames(VARARGS) = "VARARGS"
  flagNames(TRIEDCOOKING) = "TRIEDCOOKING"
  flagNames(SYNCHRONIZED) = "SYNCHRONIZED"
  flagNames(ARTIFACT) = "ARTIFACT"
  flagNames(JAVA_DEFAULT_METHOD) = "JAVA_DEFAULT_METHOD"
  flagNames(JAVA_ENUM) = "JAVA_ENUM"
  flagNames(JAVA_ANNOTATION) = "JAVA_ANNOTATION"
  flagNames(SYNTHESIZE_IMPL_IN_SUBCLASS) = "SYNTHESIZE_IMPL_IN_SUBCLASS"
  flagNames(notPROTECTED) = "notPROTECTED"
  flagNames(notPRIVATE) = "notPRIVATE"
}
