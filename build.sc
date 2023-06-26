import mill._, scalalib._, publish._

object utils extends ScalaModule with PublishModule {
  def scalaVersion = "3.3.0"
  def publishVersion = "0.0.1"

  def pomSettings = PomSettings(
    description = "Basic utilities I like to use in Scala, especially implicits for working with Option and Try.",
    organization = "github.com/aholland",
    url = "https://github.com/aholland/aholland-scala-utils.git",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("aholland", "aholland-scala-utils"),
    developers = Seq(
      Developer("aholland", "Anthony Holland", "https://github.com/aholland")
    )
  )
}
