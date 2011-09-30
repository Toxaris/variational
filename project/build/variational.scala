import sbt._

class variational(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {
    val junit = "junit" % "junit" % "4.8.1" % "test"
    val scalatest = "org.scalatest" % "scalatest_2.9.1" % "1.6.1" % "test"
    val junitInterface = "com.novocode" % "junit-interface" % "0.6" % "test"
    // val scalacheck = "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9" % "test"
    // def junitXmlListener: TestReportListener = new JUnitXmlTestsListener(outputPath.toString)
    // override def testListeners: Seq[TestReportListener] = super.testListeners ++ Seq(junitXmlListener)
    
    // override def javaCompileOptions = super.javaCompileOptions ++ javaCompileOptions("-source", "1.5", "-Xlint:unchecked")
    override def compileOptions = super.compileOptions ++ Seq(Unchecked,
        Deprecation, ExplainTypes, Optimize)
}
