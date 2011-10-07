package variational

import java.io.FileWriter

/**
 * Operations to export a value into a dot graph.
 *
 * <p>In the graph, sharing is retained. The graph can be further
 * processed with GraphViz.
 *
 * @author Tillmann Rendel
 */
class GraphViz extends Memoize[Any, String] {

  def escape(text : String) =
    text.replaceAllLiterally("\"", "\\\"")

  var next = 0

  val builder = new StringBuilder()

  def result : String = builder.result

  def process(value : Any) = {
    val node = "n" + next
    next += 1

    value match {
      case c : Choice[_] => {
        builder ++= "  " + node + " [label=\"" + c.condition + "?\", shape=diamond];\n"

        val thenBranchNode = apply(c.thenBranch)
        builder ++= "  " + node + " -> " + thenBranchNode + ";\n"

        val elseBranchNode = apply(c.elseBranch)
        builder ++= "  " + node + " -> " + elseBranchNode + ";\n"
      }

      case l : Leaf[_] => {
        builder ++= "  " + node + " [label=\"" + escape(l.toString) + "\", shape=box];\n"
      }

      case s : Simple[_,  _] => {
        builder ++= "  " + node + " [label=\"" + s.prefix + "\", shape=box];\n"
        val childNode = apply(s.value)
        builder ++= "  " + node + " -> " + childNode + ";\n"
      }

      case s : StructureLike[_] => {
        builder ++= "  " + node + " [label=\"" + s.prefix + "\", shape=box];\n"

        for (child <- s.children) {
          val childNode = apply(child)
          builder ++= "  " + node + " -> " + childNode + ";\n"
        }
      }

      case any => {
        builder ++= "  " + node + " [label=\"" + value + "\"];\n"
      }
    }

    node
  }
}

object GraphViz {
  def asString(value : Any) : String = {
    val graphViz = new GraphViz
    graphViz.process(value)
    "digraph variational {\n" + graphViz.result + "}\n"
  }

  def asFile(value : Any, filename : String) = {
    val fw = new FileWriter(filename);
    fw.write(asString(value));
    fw.close()
  }
}