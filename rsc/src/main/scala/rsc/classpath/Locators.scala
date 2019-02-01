// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.classpath

trait Locators {
  // NOTE: Relative path within a class directory or a jar.
  //  * /foo/bar/ for package foo.bar
  //  * /foo/Bar.class for top-level class foo.Bar
  //  * /foo/Bar$.class for top-level object foo.Bar
  //  * etc etc for inner classes / objects / Java classes
  type Locator = String
}
