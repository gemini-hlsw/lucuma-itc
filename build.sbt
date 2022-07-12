val coulombVersion              = "0.6.0-M3"
val catsEffectVersion           = "3.3.13"
val catsTestkitScalaTestVersion = "2.1.5"
val catsVersion                 = "2.8.0"
val catsScalacheckVersion       = "0.3.1"
val catsTimeVersion             = "0.3.4"
val circeVersion                = "0.14.2"
val cirisVersion                = "2.3.2"
val clueVersion                 = "0.23.1"
val http4sVersion               = "0.23.13"
val http4sJdkHttpClientVersion  = "0.5.0"
val fs2Version                  = "3.2.10"
val kindProjectorVersion        = "0.13.2"
val lucumaCoreVersion           = "0.45-e482287-SNAPSHOT"
val lucumaRefinedVersion        = "0.1.0"
val slf4jVersion                = "1.7.36"
val log4catsVersion             = "2.3.2"
val monocleVersion              = "3.1.0"
val munitCatsEffectVersion      = "1.0.7"
val refinedVersion              = "0.10.1"
val grackleVersion              = "0.1.16"
val natcchezHttp4sVersion       = "0.3.2"
val natchezVersion              = "0.1.6"
val munitVersion                = "0.7.29"
val disciplineMunitVersion      = "1.0.9"
val gatlingVersion              = "3.7.6"
val spireVersion                = "0.18.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion       := "3.1.3"
ThisBuild / crossScalaVersions := Seq("3.1.3")

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; Test/scalafix OrganizeImports; scalafmtAll"
)

lazy val commonSettings = lucumaGlobalSettings ++ Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-testkit"           % catsVersion                 % "test",
    "org.typelevel" %% "cats-testkit-scalatest" % catsTestkitScalaTestVersion % "test"
  ),
  Test / parallelExecution := false, // tests run fine in parallel but output is nicer this way
  scalacOptions ++= Seq(
    "-language:implicitConversions"
  )
)

lazy val itc = project
  .in(file("modules/itc"))
  .settings(commonSettings)
  .settings(lucumaGlobalSettings)
  .settings(
    name := "lucuma-itc",
    libraryDependencies ++= Seq(
      "edu.gemini"     %% "lucuma-core"         % lucumaCoreVersion,
      "edu.gemini"    %%% "lucuma-refined"      % lucumaRefinedVersion,
      "org.typelevel"  %% "cats-core"           % catsVersion,
      "org.typelevel"  %% "cats-effect"         % catsEffectVersion,
      "org.http4s"     %% "http4s-ember-client" % http4sVersion,
      "org.http4s"     %% "http4s-circe"        % http4sVersion,
      "org.http4s"     %% "http4s-dsl"          % http4sVersion,
      "io.circe"       %% "circe-literal"       % circeVersion,
      "edu.gemini"     %% "clue-model"          % clueVersion,
      "io.circe"       %% "circe-generic"       % circeVersion,
      "org.tpolecat"   %% "natchez-http4s"      % natcchezHttp4sVersion,
      "org.typelevel"  %% "log4cats-slf4j"      % log4catsVersion,
      "com.manyangled" %% "coulomb-core"        % coulombVersion,
      "com.manyangled" %% "coulomb-spire"       % coulombVersion,
      "com.manyangled" %% "coulomb-units"       % coulombVersion,
      "org.typelevel" %%% "spire"               % spireVersion,
      "org.typelevel" %%% "spire-extras"        % spireVersion
    )
  )

lazy val service = project
  .in(file("modules/service"))
  .dependsOn(itc)
  .settings(commonSettings)
  .settings(
    name              := "lucuma-itc-service",
    scalacOptions -= "-Vtype-diffs",
    reStart / envVars := Map("ITC_URL" -> "https://gemini-new-itc.herokuapp.com/json"),
    libraryDependencies ++= Seq(
      "edu.gemini"    %% "gsp-graphql-core"    % grackleVersion,
      "edu.gemini"    %% "gsp-graphql-generic" % grackleVersion,
      "edu.gemini"    %% "gsp-graphql-circe"   % grackleVersion,
      "org.tpolecat"  %% "natchez-honeycomb"   % natchezVersion,
      "org.tpolecat"  %% "natchez-log"         % natchezVersion,
      "org.tpolecat"  %% "natchez-http4s"      % natcchezHttp4sVersion,
      "co.fs2"        %% "fs2-core"            % fs2Version,
      "edu.gemini"    %% "lucuma-core"         % lucumaCoreVersion,
      "org.typelevel" %% "cats-core"           % catsVersion,
      "org.typelevel" %% "cats-effect"         % catsEffectVersion,
      "is.cir"        %% "ciris"               % cirisVersion,
      "org.typelevel" %% "log4cats-slf4j"      % log4catsVersion,
      "org.slf4j"      % "slf4j-simple"        % slf4jVersion,
      "org.http4s"    %% "http4s-core"         % http4sVersion,
      "org.http4s"    %% "http4s-ember-server" % http4sVersion,
      "eu.timepit"    %% "refined"             % refinedVersion,
      "eu.timepit"    %% "refined-cats"        % refinedVersion,
      "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion % Test
    ),
    buildInfoKeys     := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "herokuSourceVersion" -> sys.env.get("SOURCE_VERSION"),
      "buildDateTime"       -> System.currentTimeMillis()
    )
  )
  .enablePlugins(JavaAppPackaging, BuildInfoPlugin)

// lazy val benchmark = project
//   .in(file("modules/benchmarks"))
//   .enablePlugins(GatlingPlugin)
//   .settings(
//     libraryDependencies ++= Seq(
//       // "io.circe"             %% "circe-core"                % circeVersion,
//       // "io.gatling.highcharts" % "gatling-charts-highcharts" % gatlingVersion % Test,
//       "io.gatling" % "gatling-test-framework_2.13" % gatlingVersion
//     )
//   )
//   // .dependsOn(service)
