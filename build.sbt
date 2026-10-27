val Scala3Version = "3.3.6"
val Http4sVersion = "0.23.30"
val CirceVersion = "0.14.14"
val MunitVersion = "1.1.1"
val MunitCatsEffectVersion = "2.1.0"
val LogbackVersion = "1.5.18"

lazy val root = project
  .in(file("."))
  .settings(
    organization := "io.quantharvest",
    name := "schwab-mock",
    version := "0.1.0",
    scalaVersion := Scala3Version,
    Compile / run / mainClass := Some("io.quantharvest.schwabmock.SchwabMockApp"),
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.http4s"     %% "http4s-ember-server" % Http4sVersion,
      "org.http4s"     %% "http4s-ember-client" % Http4sVersion,
      "org.http4s"     %% "http4s-circe"        % Http4sVersion,
      "org.http4s"     %% "http4s-dsl"          % Http4sVersion,
      "io.circe"       %% "circe-parser"        % CirceVersion,
      "org.scalameta"  %% "munit"               % MunitVersion           % Test,
      "org.typelevel"  %% "munit-cats-effect"   % MunitCatsEffectVersion % Test,
      "ch.qos.logback" %  "logback-classic"     % LogbackVersion         % Runtime
    ),
    assembly / assemblyMergeStrategy := {
      case "module-info.class" => MergeStrategy.discard
      case x if x.endsWith("module-info.class") => MergeStrategy.discard
      case PathList("META-INF", "versions", xs @ _*) => MergeStrategy.first
      case x => (assembly / assemblyMergeStrategy).value.apply(x)
    }
  )
