/* This file is part of scalambda.
*
* See the NOTICE file distributed with this work for copyright information.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package gnieh.lambda

import org.kiama.util.JLineConsole
import jline._

import util._

/**
 * 
 * This object represents the lambda console object.
 * It is meant as an abstraction to add new commands and completors. 
 * 
 * @author Lucas Satabin
 *
 */
object LambdaConsole {

  import JLineConsole.reader

  def init {
    // initialize the completors
    reader.addCompletor(new MultiCompletor(completors))
  }

  def completors: Array[Completor] =
    Array(
      new SimpleCompletor(
        Array(":help", ":quit", ":normal-order", ":call-by-name",
          ":call-by-value", ":show-steps", ":hide-steps", ":env",
          ":show-aliases", ":hide-aliases", ":save", ":de-bruijn",
          ":enable-typing", ":disable-typing")),
      new ArgumentCompletor(Array[Completor](
        new SimpleCompletor(":rm"),
        EnvCompletor)),
      new ArgumentCompletor(Array[Completor](
        new SimpleCompletor(":load"),
        LibCompletor)))

  object EnvCompletor extends SimpleCompletor(environment.names.toArray) {
    override def complete(buffer: String, cursor: Int, candidates: java.util.List[_]) = {
      val newnames = environment.names
      if (newnames.size != getCandidates.size)
        setCandidateStrings(newnames.toArray)
      super.complete(buffer, cursor, candidates)
    }
  }

  object LibCompletor extends FileNameCompletor {
    override def complete(buffer: String, cursor: Int, candidates: java.util.List[_]) = {

      val translated = "defs" + java.io.File.separator + (if (buffer != null) buffer else "")
      val bufferLength = if(buffer == null) 0 else buffer.length
      
      super.complete(translated, cursor, candidates) - 5
    }
  }

}