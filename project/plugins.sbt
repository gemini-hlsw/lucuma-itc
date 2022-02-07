resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += "sonatype-s01-snapshots".at(
  "https://s01.oss.sonatype.org/content/repositories/snapshots"
)

addSbtPlugin("edu.gemini"     % "sbt-lucuma-app"      % "0.6-cde3ac9-SNAPSHOT")
addSbtPlugin("io.spray"       % "sbt-revolver"        % "0.9.1")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.7")
addSbtPlugin("io.gatling"     % "gatling-sbt"         % "4.1.2")
