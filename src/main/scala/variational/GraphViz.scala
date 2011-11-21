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
class GraphViz(val variables : Array[String] = Array.empty) extends Memoize[Any, String] {

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
        val condition =
          if (c.condition < variables.length)
            variables(c.condition)
          else c.condition.toString
        builder ++= "  " + node + " [label=\"" + condition + "?\", shape=diamond];\n"

        val thenBranchNode = apply(c.thenBranch)
        builder ++= "  " + node + " -> " + thenBranchNode + "[label = \"yes\"];\n"

        val elseBranchNode = apply(c.elseBranch)
        builder ++= "  " + node + " -> " + elseBranchNode + "[label = \"no\"];\n"
      }

      case s : StructureLike[_] => {
        builder ++= "  " + node + " [label=\"" + s.prefix + "\", shape=box];\n"

        var i = 0
        for (child <- s.children) {
          val childNode = apply(child)
          builder ++= "  " + node + " -> " + childNode + " [label= \"" + i + "\"];\n"
          i += 1
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
  def asString(value : Any, variables : Array[String] = Array.empty) : String = {
    val graphViz = new GraphViz(variables)
    graphViz.process(value)
    "digraph variational {\n" + graphViz.result + "}\n"
  }

  def asFile(value : Any, filename : String, variables : Array[String] = Array.empty) = {
    val fw = new FileWriter(filename);
    fw.write(asString(value));
    fw.close()
  }
}