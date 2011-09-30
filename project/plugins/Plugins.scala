import sbt._

class Plugins(info: ProjectInfo) extends PluginDefinition(info) {
    val sbtIdeaRepo = "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"
    val sbtIdea = "com.github.mpeltonen" % "sbt-idea-plugin" % "0.4.0"
    // val junitXmlRepo = "Christoph's Maven Repo" at "http://maven.henkelmann.eu/"
    // val junitXml = "eu.henkelmann" % "junit_xml_listener" % "0.2"
}
