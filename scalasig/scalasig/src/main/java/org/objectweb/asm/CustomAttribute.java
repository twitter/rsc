// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
// NOTE: This file has been partially copy/pasted from scala/asm.
package org.objectweb.asm;

import org.objectweb.asm.Attribute;

/**
 * A subclass of ASM's Attribute for the sole purpose of accessing a protected field there.
 */
public class CustomAttribute extends Attribute {
  public CustomAttribute(final String type, final byte[] value) {
    super(type);
    super.value = value;
  }
}
