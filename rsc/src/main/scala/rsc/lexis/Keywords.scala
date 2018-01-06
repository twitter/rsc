// Copyright (c) 2017-2018 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.lexis

import java.util.HashMap

trait Keywords {
  val keywords = {
    val results = new HashMap[String, Token]
    results.put("abstract", ABSTRACT)
    results.put("case", CASE)
    results.put("catch", CATCH)
    results.put("class", CLASS)
    results.put("def", DEF)
    results.put("do", DO)
    results.put("else", ELSE)
    results.put("extends", EXTENDS)
    results.put("false", FALSE)
    results.put("final", FINAL)
    results.put("finally", FINALLY)
    results.put("for", FOR)
    results.put("forSome", FORSOME)
    results.put("if", IF)
    results.put("implicit", IMPLICIT)
    results.put("import", IMPORT)
    results.put("lazy", LAZY)
    results.put("match", MATCH)
    results.put("new", NEW)
    results.put("null", NULL)
    results.put("object", OBJECT)
    results.put("override", OVERRIDE)
    results.put("package", PACKAGE)
    results.put("private", PRIVATE)
    results.put("protected", PROTECTED)
    results.put("return", RETURN)
    results.put("sealed", SEALED)
    results.put("super", SUPER)
    results.put("this", THIS)
    results.put("throw", THROW)
    results.put("trait", TRAIT)
    results.put("try", TRY)
    results.put("true", TRUE)
    results.put("type", TYPE)
    results.put("val", VAL)
    results.put("var", VAR)
    results.put("while", WHILE)
    results.put("with", WITH)
    results.put("yield", YIELD)
    results.put("_", USCORE)
    results.put(":", COLON)
    results.put("=", EQUALS)
    results.put("=>", ARROW)
    results.put("⇒", ARROW)
    results.put("<-", LARROW)
    results.put("←", LARROW)
    results.put("<:", SUBTYPE)
    results.put("<%", CATCH)
    results.put(">:", SUPERTYPE)
    results.put("#", HASH)
    results.put("@", AT)
    results
  }
}
