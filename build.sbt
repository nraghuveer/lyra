import org.scalajs.linker.interface.ModuleSplitStyle

val publicDev = taskKey[String]("output directory for `npm run dev`")
val publicProd = taskKey[String]("output directory for `npm run build`")

lazy val lyra = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    scalaVersion := "3.2.1",
    scalacOptions ++= Seq(
      "-encoding",
      "utf-8",
      "-deprecation",
      "-feature"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("lyra"))
        )
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.2.0",
      "io.circe" %%% "circe-core" % "0.14.3",
      "io.circe" %%% "circe-parser" % "0.14.3",
      "io.circe" %%% "circe-generic" % "0.14.3",
    ) ++ Seq(("org.scala-js" %%% "scalajs-java-securerandom" % "1.0.0").cross(CrossVersion.for3Use2_13)),
    publicDev := linkerOutputDirectory((Compile / fastLinkJS).value)
      .getAbsolutePath,
    publicProd := linkerOutputDirectory((Compile / fullLinkJS).value)
      .getAbsolutePath
  )

def linkerOutputDirectory(
    v: Attributed[org.scalajs.linker.interface.Report]
): File = {
  v.get(scalaJSLinkerOutputDirectory.key).getOrElse {
    throw new MessageOnlyException(
      "Linking report was not attributed with output directory. " +
        "Please report this as a Scala.js bug."
    )
  }
}
