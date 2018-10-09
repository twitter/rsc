/*
 * Copyright (C) 2017 Lightbend Inc. <http://www.lightbenc.com>
 */
// NOTE: This file has been partially copy/pasted from scala/jardiff.

package scala.tools.jardiff

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.lib.AnyObjectId
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.treewalk.CanonicalTreeParser

object JGitUtil {
  def getCanonicalTreeParser(git: Git, commitId: AnyObjectId) = {
    val walk = new RevWalk(git.getRepository)
    try {
      val commit = walk.parseCommit(commitId)
      val treeId = commit.getTree.getId
      val reader = git.getRepository.newObjectReader
      try {
        new CanonicalTreeParser(null, reader, treeId)
      } finally {
        reader.close()
      }
    } finally walk.close()
  }


}
