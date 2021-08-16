import com.typesafe.sbt.packager.docker.Cmd

// Plugins
enablePlugins(JavaAppPackaging, DockerPlugin)

// Project Settings
name                 := "itc-server"
scalaVersion         := "2.11.11"
scalacOptions        += "-Yrangepos"
libraryDependencies ++= Seq(
  "org.eclipse.jetty" %  "jetty-server"  % "9.4.15.v20190215",
  "org.eclipse.jetty" %  "jetty-servlet" % "9.4.15.v20190215"
)

// Docker Settings
Docker / packageName := "web"
dockerRepository     := Some("registry.heroku.com")
dockerUsername       := Some("gemini-new-itc")
Docker / name        := "web"
dockerAlias          := DockerAlias(
  dockerRepository.value,
  dockerUsername.value,
  (Docker / name).value,
  None
)
dockerCommands   := """
  FROM openjdk:8
  WORKDIR /opt/docker
  ADD --chown=daemon:daemon opt /opt
  USER daemon
  CMD ["/opt/docker/bin/itc-server"]
""".lines.map(_.trim).map(Cmd(_)).toSeq
