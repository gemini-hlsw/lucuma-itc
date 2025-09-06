import NativePackagerHelper._

val boopickleVersion            = "1.5.0"
val catsVersion                 = "2.13.0"
val catsEffectVersion           = "3.6.3"
val catsTestkitScalaTestVersion = "2.1.5"
val catsTimeVersion             = "0.3.4"
val catsScalacheckVersion       = "0.3.2"
val circeVersion                = "0.14.14"
val circeRefinedVersion         = "0.15.1"
val cirisVersion                = "3.10.0"
val clueVersion                 = "0.47.0"
val disciplineMunitVersion      = "2.0.0"
val fs2Version                  = "3.12.2"
val grackleVersion              = "0.25.0"
val graphQLRoutesVersion        = "0.11.1"
val http4sVersion               = "0.23.30"
val http4sJdkHttpClientVersion  = "0.10.0"
val jmhVersion                  = "1.37"
val keySemaphoreVersion         = "0.3.0-M1"
val kittensVersion              = "3.5.0"
val log4catsVersion             = "2.7.1"
val lucumaCoreVersion           = "0.142.0"
val lucumaRefinedVersion        = "0.1.4"
val monocleVersion              = "3.3.0"
val munitVersion                = "1.1.1"
val munitCatsEffectVersion      = "2.1.0"
val natchezVersion              = "0.3.8"
val natchezHttp4sVersion        = "0.6.1"
val pprintVersion               = "0.9.3"
val redis4CatsVersion           = "2.0.1"
val refinedVersion              = "0.11.3"
val slf4jVersion                = "2.0.17"
val spireVersion                = "0.18.0"
val dropwizardVersion           = "4.2.36"

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion        := "3.7.2"
ThisBuild / crossScalaVersions  := Seq("3.7.2")
ThisBuild / tlBaseVersion       := "0.45"
ThisBuild / tlCiReleaseBranches := Seq("main")
ThisBuild / scalacOptions ++= Seq("-Xmax-inlines", "50") // Hash derivation fails with default of 32

val herokuToken = "HEROKU_API_KEY" -> "${{ secrets.HEROKU_API_KEY }}"

ThisBuild / githubWorkflowSbtCommand := "sbt -v -J-Xmx6g"
ThisBuild / githubWorkflowEnv += herokuToken

Global / onChangedBuildSource := ReloadOnSourceChanges

enablePlugins(NoPublishPlugin)

addCommandAlias(
  "fixImports",
  "; scalafix OrganizeImports; Test/scalafix OrganizeImports; scalafmtAll"
)

lazy val commonSettings = lucumaGlobalSettings ++ Seq(
  Test / parallelExecution := false // tests run fine in parallel but output is nicer this way
)

lazy val CheckoutFullWithLfs: WorkflowStep =
  WorkflowStep.Use(
    UseRef.Public("actions", "checkout", "v4"),
    name = Some("Checkout current branch (full)"),
    params = Map("fetch-depth" -> "0", "lfs" -> "true")
  )

ThisBuild / githubWorkflowJobSetup := {
  List(CheckoutFullWithLfs) :::
    WorkflowStep.SetupSbt ::
    WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList) :::
    githubWorkflowGeneratedCacheSteps.value.toList
}

lazy val sbtDockerPublishLocal =
  WorkflowStep.Sbt(
    List("clean", "deploy/docker:publishLocal"),
    name = Some("Build and Publish Docker image")
  )

val environments = List("dev", "staging", "production")

lazy val herokuRelease =
  WorkflowStep.Run(
    List(
      "npm install -g heroku",
      "heroku container:login"
    ) ++
      environments.flatMap(env =>
        List(
          s"docker tag noirlab/gpp-itc registry.heroku.com/$${{ vars.HEROKU_APP_NAME || 'itc' }}-${env}/web:$${{ github.sha }}",
          s"docker push registry.heroku.com/$${{ vars.HEROKU_APP_NAME || 'itc' }}-${env}/web:$${{ github.sha }}"
        )
      ) ++ List( // Retag for easy release to dev
        s"docker tag noirlab/gpp-itc registry.heroku.com/$${{ vars.HEROKU_APP_NAME || 'itc' }}-dev/web",
        s"docker push registry.heroku.com/$${{ vars.HEROKU_APP_NAME || 'itc' }}-dev/web",
        s"heroku container:release web -a $${{ vars.HEROKU_APP_NAME || 'itc' }}-dev -v"
      ),
    name = Some("Deploy and release app in Heroku")
  )

lazy val retrieveDockerImageSha = WorkflowStep.Run(
  List(
    "# Get Docker image SHA",
    """echo "DOCKER_IMAGE_SHA=$(docker inspect registry.heroku.com/${{ vars.HEROKU_APP_NAME || 'itc' }}-dev/web:${{ github.sha }} --format={{.Id}})" >> $GITHUB_ENV"""
  ),
  name = Some("Get Docker image SHA")
)

lazy val recordDeploymentMetadata = WorkflowStep.Run(
  List(
    "# Create a deployment record with commit SHA and image SHA for tracking",
    """echo "Recording deployment: ${{ github.sha }} to ${{ github.repository }}"""",
    """curl -X POST https://api.github.com/repos/${{ github.repository }}/deployments -H "Authorization: Bearer ${{ secrets.GITHUB_TOKEN }}" -H "Accept: application/vnd.github+json" -d '{ "ref": "${{ github.sha }}", "environment": "development", "description": "ITC deployment to dev", "auto_merge": false, "required_contexts": [], "payload": { "docker_image_sha": "${{ env.DOCKER_IMAGE_SHA }}" } }' """
  ),
  name = Some("Record deployment gha")
)

val mainCond                 = "github.ref == 'refs/heads/main'"
val geminiRepoCond           = "startsWith(github.repository, 'gemini')"
def allConds(conds: String*) = conds.mkString("(", " && ", ")")

ThisBuild / watchOnTermination := { (action, cmd, times, state) =>
  val projNames = cmd
    .split(";")
    .flatMap(
      Some(_)
        .filter(_.contains("/reStart"))
        .flatMap(_.trim.split("/reStart") match {
          case Array(projName) => Some(projName)
          case _               => None
        })
    )
  projNames.foldLeft(state) { (acc, projName) =>
    val projRef = ProjectRef((ThisBuild / baseDirectory).value, projName)
    Project.extract(state).runTask(projRef / reStop, state)._1
  }
}

ThisBuild / githubWorkflowBuildPreamble +=
  WorkflowStep.Use(
    UseRef.Public("kamilkisiela", "graphql-inspector", "master"),
    name = Some("Validate GraphQL schema changes"),
    params = Map(
      "schema"        -> "main:modules/service/src/main/resources/graphql/itc.graphql",
      "approve-label" -> "expected-breaking-change"
    ),
    cond = Some("github.event_name == 'pull_request'")
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    "deploy",
    "Build and publish Docker image / Deploy to Heroku",
    githubWorkflowJobSetup.value.toList :::
      sbtDockerPublishLocal ::
      herokuRelease ::
      retrieveDockerImageSha ::
      recordDeploymentMetadata ::
      Nil,
    scalas = List(scalaVersion.value),
    javas = githubWorkflowJavaVersions.value.toList.take(1),
    cond = Some(allConds(mainCond, geminiRepoCond))
  )

// Basic model classes
lazy val model = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/model"))
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc",
    libraryDependencies ++= Seq(
      "io.circe"      %%% "circe-generic" % circeVersion,
      "io.circe"      %%% "circe-refined" % circeRefinedVersion,
      "org.typelevel" %%% "cats-core"     % catsVersion,
      "edu.gemini"    %%% "lucuma-core"   % lucumaCoreVersion,
      "eu.timepit"    %%% "refined"       % refinedVersion,
      "eu.timepit"    %%% "refined-cats"  % refinedVersion,
      "org.typelevel" %%% "kittens"       % kittensVersion
    )
  )

lazy val ocslibHash = taskKey[String]("hash of ocslib")
ThisBuild / ocslibHash / fileInputs += (service / baseDirectory).value.toGlob / "ocslib" / "*.jar"
ThisBuild / ocslibHash := {
  val hashes = ocslibHash.inputFiles.sorted.map(_.toFile).map(Hash(_))
  Hash.toHex(Hash(hashes.toArray.flatten))
}

// Contains the grackle server
lazy val service = project
  .in(file("modules/service"))
  .dependsOn(model.jvm)
  .settings(commonSettings)
  .settings(
    name                  := "lucuma-itc-service",
    scalacOptions -= "-Vtype-diffs",
    reStart / javaOptions := Seq(
      "-Dcats.effect.stackTracing=DISABLED",
      "-Dcats.effect.tracing.mode=none"
    ),
    reStart / envVars     := Map(
      "ODB_BASE_URL" -> "https://lucuma-postgres-odb-dev.herokuapp.com"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel"        %% "grackle-core"          % grackleVersion,
      "org.typelevel"        %% "grackle-generic"       % grackleVersion,
      "org.typelevel"        %% "grackle-circe"         % grackleVersion,
      "edu.gemini"           %% "lucuma-graphql-routes" % graphQLRoutesVersion,
      "org.tpolecat"         %% "natchez-honeycomb"     % natchezVersion,
      "org.tpolecat"         %% "natchez-log"           % natchezVersion,
      "org.tpolecat"         %% "natchez-http4s"        % natchezHttp4sVersion,
      "co.fs2"               %% "fs2-core"              % fs2Version,
      "edu.gemini"           %% "lucuma-core"           % lucumaCoreVersion,
      "org.typelevel"        %% "cats-core"             % catsVersion,
      "org.typelevel"        %% "cats-effect"           % catsEffectVersion,
      "is.cir"               %% "ciris"                 % cirisVersion,
      "org.typelevel"        %% "log4cats-slf4j"        % log4catsVersion,
      "org.slf4j"             % "slf4j-simple"          % slf4jVersion,
      "org.http4s"           %% "http4s-core"           % http4sVersion,
      "org.http4s"           %% "http4s-ember-server"   % http4sVersion,
      "org.http4s"           %% "http4s-ember-client"   % http4sVersion,
      "eu.timepit"           %% "refined"               % refinedVersion,
      "eu.timepit"           %% "refined-cats"          % refinedVersion,
      "dev.profunktor"       %% "redis4cats-effects"    % redis4CatsVersion,
      "dev.profunktor"       %% "redis4cats-log4cats"   % redis4CatsVersion,
      "com.lihaoyi"          %% "pprint"                % pprintVersion,
      "io.suzaku"            %% "boopickle"             % boopickleVersion,
      "io.chrisdavenport"    %% "keysemaphore"          % keySemaphoreVersion,
      "io.dropwizard.metrics" % "metrics-core"          % dropwizardVersion,
      "io.dropwizard.metrics" % "metrics-jvm"           % dropwizardVersion,
      "io.dropwizard.metrics" % "metrics-graphite"      % dropwizardVersion,
      "org.typelevel"        %% "munit-cats-effect"     % munitCatsEffectVersion % Test
    ),
    buildInfoKeys         := Seq[BuildInfoKey](
      scalaVersion,
      sbtVersion,
      git.gitHeadCommit,
      "herokuSourceVersion" -> sys.env.get("SOURCE_VERSION"),
      "buildDateTime"       -> System.currentTimeMillis(),
      ocslibHash
    )
  )
  .enablePlugins(BuildInfoPlugin)

/**
 * Project for the ITC server app for development
 */
lazy val deploy = project
  .in(file("deploy"))
  .enablePlugins(NoPublishPlugin)
  .enablePlugins(LucumaDockerPlugin)
  .enablePlugins(JavaServerAppPackaging)
  .enablePlugins(GitBranchPrompt)
  .dependsOn(service)
  .settings(
    description              := "ITC Server",
    Docker / packageName     := "gpp-itc",
    description              := "ITC Server",
    // Main class for launching
    Compile / mainClass      := Some("lucuma.itc.service.Main"),
    // Name of the launch script
    executableScriptName     := "itc-service",
    dockerExposedPorts ++= Seq(6060),
    // Add the ocslib jars to the distribution
    Universal / mappings ++= {
      val dir = (service / baseDirectory).value / "ocslib"
      (dir ** AllPassFilter).pair(relativeTo(dir.getParentFile))
    },
    // The heap needs to be a lot smaller than the dyno size. This may be
    // because the JVM tricks to load the 367M of old itc jar files increases the
    // `metaspace` size by that amount. It's a nice theory, at least.
    lucumaDockerHeapSubtract := 400
  )

lazy val client = crossProject(JVMPlatform, JSPlatform)
  .in(file("modules/client"))
  .dependsOn(model)
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc-client",
    libraryDependencies ++= Seq(
      "edu.gemini"    %%% "lucuma-core"       % lucumaCoreVersion,
      "edu.gemini"    %%% "lucuma-refined"    % lucumaRefinedVersion,
      "org.typelevel" %%% "cats-core"         % catsVersion,
      "org.typelevel" %%% "cats-effect"       % catsEffectVersion,
      "org.http4s"    %%% "http4s-circe"      % http4sVersion,
      "org.http4s"    %%% "http4s-dsl"        % http4sVersion,
      "io.circe"      %%% "circe-literal"     % circeVersion,
      "edu.gemini"    %%% "clue-model"        % clueVersion,
      "edu.gemini"    %%% "clue-http4s"       % clueVersion,
      "edu.gemini"    %%% "clue-core"         % clueVersion,
      "io.circe"      %%% "circe-generic"     % circeVersion,
      "org.tpolecat"  %%% "natchez-http4s"    % natchezHttp4sVersion,
      "org.typelevel" %%% "spire"             % spireVersion,
      "org.typelevel" %%% "spire-extras"      % spireVersion,
      "org.typelevel" %%% "kittens"           % kittensVersion,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % Test,
      "com.lihaoyi"   %%% "pprint"            % pprintVersion          % Test
    )
  )

lazy val benchmark = project
  .in(file("modules/benchmarks"))
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.openjdk.jmh" % "jmh-core"                 % jmhVersion,
      "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion
    ),
    // Add project root system property for benchmarks
    fork       := false,
    Jmh / fork := false,
    javaOptions += s"-Dproject.root=${(ThisBuild / baseDirectory).value}",
    Jmh / javaOptions += s"-Dproject.root=${(ThisBuild / baseDirectory).value}"
  )
  .dependsOn(service)

val MUnitFramework = new TestFramework("munit.Framework")

lazy val testkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/testkit"))
  .dependsOn(client)
  .settings(commonSettings)
  .settings(
    name := "lucuma-itc-testkit",
    libraryDependencies ++= Seq(
      "edu.gemini"        %%% "lucuma-core-testkit" % lucumaCoreVersion,
      "org.typelevel"     %%% "cats-testkit"        % catsVersion,
      "dev.optics"        %%% "monocle-law"         % monocleVersion,
      "org.typelevel"     %%% "spire-laws"          % spireVersion,
      "eu.timepit"        %%% "refined-scalacheck"  % refinedVersion,
      "io.circe"          %%% "circe-testing"       % circeVersion,
      "io.chrisdavenport" %%% "cats-scalacheck"     % catsScalacheckVersion
    )
  )

lazy val tests = project
  .in(file("modules/tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(service, client.jvm, testkit.jvm)
  .settings(
    name := "lucuma-itc-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "munit-cats-effect"      % munitCatsEffectVersion     % Test,
      "com.lihaoyi"   %%% "pprint"                 % pprintVersion              % Test,
      "org.http4s"     %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion % Test,
      "org.typelevel" %%% "log4cats-slf4j"         % log4catsVersion            % Test,
      "org.scalameta" %%% "munit"                  % munitVersion               % Test,
      "org.typelevel" %%% "discipline-munit"       % disciplineMunitVersion     % Test
    ),
    testFrameworks += MUnitFramework
  )

lazy val legacyTests = project
  .in(file("modules/legacy-tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(service, client.jvm, testkit.jvm)
  .settings(
    name := "lucuma-itc-legacy-tests",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "munit-cats-effect"      % munitCatsEffectVersion     % Test,
      "com.lihaoyi"   %%% "pprint"                 % pprintVersion              % Test,
      "org.http4s"     %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion % Test,
      "org.typelevel" %%% "log4cats-slf4j"         % log4catsVersion            % Test,
      "org.scalameta" %%% "munit"                  % munitVersion               % Test,
      "org.typelevel" %%% "discipline-munit"       % disciplineMunitVersion     % Test
    ),
    testFrameworks += MUnitFramework
  )
