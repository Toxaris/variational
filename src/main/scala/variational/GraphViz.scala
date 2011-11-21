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

  val choiceFillColor = "#FF6700"
  val choiceColor = "#CD5200"
  val choiceFontName = "Calibri"
  val choiceFontSize = "12"

  val structureFillColor = "#909465" // "#94C600"
  val structureColor = "#5F6142"
  val structureFontName = "Calibri"
  val structureFontSize = "12"

  val thenEdgeFontName = "Calibri"
  val thenEdgeFontSize = "12"
  val thenEdgeColor = "black"
  val thenEdgeStyle = "solid"

  val elseEdgeFontName = "Calibri"
  val elseEdgeFontSize = "12"
  val elseEdgeColor = "black"
  val elseEdgeStyle = "dotted"

  val anyFontName = "Calibri"
  val anyFontSize = "12"

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

        // the node
        builder ++= "  " + node + " ["
        builder ++= "label=\"" + condition + "\", "
        builder ++= "shape=\"box\", "
        builder ++= "color=\"" + choiceColor + "\", "
        builder ++= "fillcolor=\"" + choiceFillColor + "\", "
        builder ++= "style=\"filled\","
        builder ++= "fontname=\"" + choiceFontName + "\", "
        builder ++= "fontsize=\"" + choiceFontSize + "\""
        builder ++= "];\n"

        val thenBranchNode = apply(c.thenBranch)
        builder ++= "  " + node + " -> " + thenBranchNode + "["
        builder ++= "style=\"" + thenEdgeStyle + "\","
        builder ++= "color=\"" + thenEdgeColor + "\", "
        builder ++= "fontname=\"" + thenEdgeFontName + "\", "
        builder ++= "fontsize=\"" + thenEdgeFontSize + "\""
        builder ++= "];\n"

        val elseBranchNode = apply(c.elseBranch)
        builder ++= "  " + node + " -> " + elseBranchNode + "["
        builder ++= "style=\"" + elseEdgeStyle + "\","
        builder ++= "color=\"" + elseEdgeColor + "\", "
        builder ++= "fontname=\"" + elseEdgeFontName + "\", "
        builder ++= "fontsize=\"" + elseEdgeFontSize + "\""
        builder ++= "];\n"
      }

      case s : StructureLike[_] => {
        builder ++= "  " + node
        builder ++= " [label=\""
        builder ++= s.prefix
        builder ++= "\", "
        builder ++= "color=\"" + structureColor + "\", "
        builder ++= "fillcolor=\"" + structureFillColor + "\", "
        builder ++= "fontname=\"" + structureFontName + "\", "
        builder ++= "fontsize=\"" + structureFontSize + "\", "
        builder ++= "style=\"filled\", "
        builder ++= "shape=\"box\""
        builder ++= "];\n"

        var i = 0
        for (child <- s.children) {
          val childNode = apply(child)
          builder ++= "  " + node + " -> " + childNode + " [label= \"" + i + "\"];\n"
          i += 1
        }
      }

      case any => {
        builder ++= "  " + node + " ["
        builder ++= "label=\"" + value + "\","
        builder ++= "fontname=\"" + anyFontName + "\", "
        builder ++= "fontsize=\"" + anyFontSize + "\""
        builder ++= "];\n"
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