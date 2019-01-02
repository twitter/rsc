// Copyright (c) 2017-2019 Twitter, Inc.
// Licensed under the Apache License, Version 2.0 (see LICENSE.md).
package rsc.settings

sealed trait Artifact
case object ArtifactSemanticdb extends Artifact
case object ArtifactScalasig extends Artifact
