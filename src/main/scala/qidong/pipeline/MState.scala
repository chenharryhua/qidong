/*
 * Copyright 2017 Chen Hua (Harry)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package qidong.pipeline
import java.time.LocalDateTime

final case class Timing(start: LocalDateTime, endat: LocalDateTime)

sealed abstract class MTraceNode(val name: String)

sealed trait MCompleted { def name: String }

case object MRoot extends MTraceNode("root")
final case class MGroupNode(override val name: String) extends MTraceNode(name)
final case class MNotRunNode(override val name: String) extends MTraceNode(name)
final case class MSuccNode(override val name: String, timing: Timing) extends MTraceNode(name) with MCompleted
final case class MFailNode(override val name: String, ex: Throwable, timing: Timing) extends MTraceNode(name) with MCompleted

object MTraceNode {
  implicit def showStatusTree = new scalaz.Show[MTraceNode] {
    override def shows(f: MTraceNode): String = f.toString
  }
}
