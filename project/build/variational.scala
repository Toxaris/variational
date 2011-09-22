import sbt._
import eu.henkelmann.sbt.JUnitXmlTestsListener

class variational(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test->default"
    val scalacheck = "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9" % "test->default"
    def junitXmlListener: TestReportListener = new JUnitXmlTestsListener(outputPath.toString)
    override def testListeners: Seq[TestReportListener] = super.testListeners ++ Seq(junitXmlListener)
    
    override def javaCompileOptions = super.javaCompileOptions ++ javaCompileOptions("-source", "1.5", "-Xlint:unchecked")
    override def compileOptions = super.compileOptions ++ Seq(Unchecked,
        Deprecation, ExplainTypes, Optimize)
}
