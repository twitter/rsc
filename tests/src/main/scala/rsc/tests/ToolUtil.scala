// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.tests

import java.nio.file._
import rsc.checkbase._
import rsc.pretty._
import scala.sys.process._
import scala.util._

trait ToolUtil extends rsc.checkbase.ToolUtil {
  def xxd(bytes: Array[Byte]): ToolResult[String] = {
    val temp = Files.createTempFile("xxd", ".bin")
    Files.write(temp, bytes)
    val buf = new StringBuilder
    val logger = ProcessLogger(line => buf.append(line + EOL))
    val exitcode = s"xxd $temp".!(logger)
    val output = buf.toString
    if (exitcode == 0) Right(output)
    else Left(List(output))
  }
}
