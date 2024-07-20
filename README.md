# Scala Development

A couple of projects based on Scala and Java stack.

## SBT Build Tips

If you use 

```scala
	groupID %% artifactID % revision 
```
	
instead of 

```scala
	groupID % artifactID % revision 
```
	
(note `%%` vs `%` after groupID), SBT will add your project’s Scala version to the artifact name. 
This is just a shortcut.

## SBT Templates

- foundweekends
  - foundweekends/giter8.g8 (A template for Giter8 templates)

- Scala
  - scala/scala-seed.g8 (Seed template for Scala)
  - scala/hello-world.g8 (A minimal Scala application)

- Dotty
    - lampepfl/dotty.g8 (simple dotty-compiled sbt project template)
    - lampepfl/dotty-cross.g8 (Dotty and Scala 2 cross-compiled sbt template)

- Akka
    - akka/akka-scala-seed.g8 (A minimal seed template for an Akka with Scala build )
    - akka/akka-java-seed.g8 (A minimal seed template for an Akka in Java )
    - akka/akka-http-scala-seed.g8 (A Giter8 template for Akka HTTP in Scala)

- Play
    - playframework/play-scala-seed.g8 (Play Scala Seed Template)
    - playframework/play-java-seed.g8 (Play Java Seed template)

- Lagom
    - lagom/lagom-scala.g8 (A Lagom Scala seed template for sbt)
    - lagom/lagom-java.g8 (A Lagom Java seed template for sbt)

- Scala Native
    - scala-native/scala-native.g8 (Scala Native)
    - portable-scala/sbt-crossproject.g8 (sbt-crossproject)

- Scalafix
    - olafurpg/scalafix.g8 (Template to start with scalafix rewrite development)

- http4s
    - http4s/http4s.g8 (http4s services)

- Unfiltered
    - unfiltered/unfiltered.g8 (Unfiltered application)
    - unfiltered/unfiltered-netty.g8 (Unfiltered with Netty)
    - unfiltered/unfiltered-war.g8 (Unfiltered template to use with servlet containers)
    - unfiltered/unfiltered-scalate.g8 (Unfiltered with Scalate and Jetty)
    - unfiltered/unfiltered-gae.g8 (Unfiltered with Google App Engine‎)
    - unfiltered/unfiltered-oauth-server.g8 (Unfiltered OAuth server)
    - unfiltered/unfiltered-oauth2-server.g8 (Unfiltered OAuth2 server)

- Scalatra
    - scalatra/scalatra.g8 (Basic Scalatra project template)

- Spark
    - holdenk/sparkProjectTemplate.g8 (Template for Scala Apache Spark project).

- Spark Job Server
    - spark-jobserver/spark-jobserver.g8 (Template for Spark Jobserver)

- lift-ng
    - joescii/lift-ng.g8 (A template for getting off the ground with Scala, Lift, and AngularJS)

- ScalaFX
    - scalafx/scalafx.g8 (Creates a ScalaFX project with build support of sbt and dependencies.)

- Kiama
    - inkytonik/kiama.g8 (Kiama-based Scala projects)

- sbt-typescript
    - joost-de-vries/play-angular-typescript.g8 (Play Typescript Angular2 application)
    - joost-de-vries/play-reactjs-typescript.g8 (Play Typescript ReactJs Typescript)

- Udash
    - https://github.com/UdashFramework/udash.g8 (Udash with configured Scala.js, Spring, Jetty and sbt-native-packager)

### Third-party and/or old templates

Giter8 has been around since 2010, so some templates are fairly new while others are outdated. We've put last updated year on some of them (e.g. 2012).

- sbt plugin
    - sbt/sbt-autoplugin.g8 (sbt 0.13.5+ AutoPlugin)
    - akiomik/sbt-plugin.g8 (2013. A template for sbt plugins)

- Play
    - ict-group/ict-play-template.g8 (A Play, Scalikejdbc, Swagger REST CRUD template)
    - xuwei-k/play.g8 (Play project template for Scala users)
    - guardian/scala-play.g8 (Play g8 template)
    - lloydmeta/ctdi-play.g8 (Compile-time DI Play template. Includes test harnesses.)
    - lloydmeta/slim-play.g8 (A slim Play app that is almost Sinatra-like.)
    - sameergarg/scala-play-macwire-reactivemongo.g8 (2014. Play 2.3, Scala, macwire and reactive mongo)
    - tysonjh/playJavaBootstrap.g8 (2014. Play 2.2, Java, with Twitter Bootstrap)
    - mbseid/play-mongo-securesocial.g8 (2013. Play 2.1 application. Comes rolled with Salat driver for MongoDB, and SecureSocial for authentication.)

- Akka
    - orhanbalci/akka-http-microservice.g8 (Akka Http Microservice template)
    - silvaren/akka-http.g8 (Akka HTTP microservice with easy start/stop controls)
    - ScalaWilliam/akka-stream-kafka-template.g8 (Akka Streams and Kafka)
    - shigemk2/minimal-akka-scala.g8 (very simple and minimal akka-scala template)
    - mhamrah/sbt.g8 (A slimmer version of ymasory's sbt project with typesafe config, logback, scalatest and akka. Features a spray option via g8 mhamrah/sbt -b spray.)
    - cfeduke/akka-scala-sbt.g8 (2014. Akka 2.2, Scala)
    - yannick-cw/elm-akka-http.g8 (Elm running on Akka-Http)
    - liquidarmour/akka-http-docker.g8 (An Akka HTTP application ready to release and run in Docker)
    - innFactory/bootstrap-akka-http.g8 (2017 A docker ready akka HTTP template with slick, in-memory postgres for tests, swagger-doc, swagger-ui, hickaricp, Flyway Migration and AWS Cognito Auth)
    - innFactory/bootstrap-akka-graphql.g8 (2017 Like the http microservice template but with sangria graphql support)

- Fast data / Spark / Flink
    - imarios/frameless.g8 (A simple frameless template to start with more expressive types for Spark)
    - tillrohrmann/flink-project.g8 (Flink project.)
    - nttdata-oss/basic-spark-project.g8 (Spark basic project.)

- Scala.js
    - Daxten/bay-scalajs.g8 (A Scala.js template with scaffolding, postgres, scalajs-react and play-framework in the backen)
    - vmunier/play-scalajs.g8 (Template to get started with Play and Scala.js)
    - vmunier/akka-http-scalajs.g8 (Template to get started with Akka HTTP and Scala.js)
    - VEINHORN/chrome-extension.g8 (Template to get started with Chrome API and Scala.js)
    - dborisenko/scalajs-react-play-material-ui.g8 (Client-server application template with Scala.js, React, Material-UI and Play)
    - dborisenko/scalajs-react-play-elemental-ui.g8 (Client-server application template with Scala.js, React, Elemental-UI and Play)

- Android / mobile
    - taisukeoe/android-pfn-app.g8 (Android project in Scala with pfn/android-sdk-plugin)
    - aafa/rest-android-scala.g8 (sbt project for Android REST client based on Retrofit with Macroid and Robolectric for testing)
    - ajhager/libgdx-sbt-project.g8 (2014. sbt project for developing Scala games using libGDX)
    - chototsu/mmstemplandroid.g8 (2013. Template for MikMikuStudio(Android version).)
    - gseitz/android-sbt-project.g8 (2011. sbt project for Android)

- Unfiltered
    - xuwei-k/unfiltered-heroku.g8 (Unfiltered heroku)
    - davececere/unfiltered-squeryl-war.g8 (2013. Full persistent REST api of a single resource. SBT, Scala, Unfiltered, Squeryl, .war build artifact)
    - akollegger/unfiltered-neo4j-on-heroku.g8 (2012. Unfiltered, dispatch to Neo4j, host on Heroku)
    - mindcandy/unfiltered-rest-gatling.g8 (2012 .sbt project for Unfiltered with Netty, Gatling load testing, and Specs2)
    - softprops/unfiltered.g8 (2012. sbt project for Unfiltered with Jetty)
    - chrislewis/unfiltered-gae.g8 (2011. sbt project for Unfiltered on Google App Engine)

- Scalatra
    - takezoe/scalatra-twirl.g8 (A web application using Scalatra with Twirl template)
    - takezoe/scalatra-scalajs.g8 (A application using Scalatra and Scala.js)
    - dkrieg/scalatra-angularjs-seed.g8 (2013. Template for web app with Scalatra, AngularJS, AngularUI-Boostrap, CoffeeScript, Less, Jasmine, Scala 2.10.0, sbt 0.12.2.)
    - JanxSpirit/scalatra-mongodb.g8 (2012. A web application using Scalatra, Casbah and MongoDB - builds with SBT 0.11.x)
    - jrevault/scalatra-squeryl.g8 (2012. sbt, Scalatra, Squeryl with usage samples, includes several plugins)

- Lift
    - eltimn/lift-mongo.g8 (Template to get a Lift-MongoDB webapp up and running quickly.)
    - temon/griya-basic.g8 (A template for start your Lift project with Spec2)
    - mads379/lift-blank.g8 (2012. Lift minimalistic project)

- Base templates / ScalaTest / Specs2
    - aafa/bintray-sbt-template.g8 (A Bintray sbt project)
    - anvie/multiproject.g8 (A multi project sbt)
    - anvie/sbt-simple-project.g8 (A simple sbt build with a single project.)
    - chriscoffey/multi-project.g8 (A multi-project build with packaging and dependencies)
    - chrislewis/basic-project.g8 (Bootstrapped sbt project with simple configuration)
    - djspiewak/base.g8 (My cool template)
    - fayimora/basic-scala-project.g8 (A simple Scala project with ScalaTest.)
    - ferhtaydn/sbt-skeleton.g8 (A simple sbt Scala project with ScalaTest and ScalaCheck.)
    - harshad-deo/sbtbase.g8 (sbt project with linting, code formatting and a few useful plugins)
    - j5ik2o/sbt-with-scalafmt.g8 (sbt with Scalafmt)
    - kurochan/java-seed.g8 (A simple hello world app in Java.)
    - lewismj/sbt-project.g8 (Modern Scala 2.12 project template, incorporates tut, microsites and default layout, 'core', 'tests', 'doc' etc...)
    - lewismj/sbt-template.g8 (Multi-module project with application (Scala 2.12, no use of Build.scala))
    - lloydmeta/seed-scala.g8 (Scala skeleton project w/ Scalatest, DocTest, Scalafmt, Wartremover, Coursier and scalac options configured)
    - pmandera/basic-scala-project.g8 (Simple scala project. Fork of fayimora/basic-scala-project.g8 using specs2 instead of ScalaTest.)
    - rlazoti/scala-sbt.g8 (An app with ScalaTest, Scala 2.10.2, sbt 0.12.3, sbt-eclipse and sbt-package-dist plugins.)
    - adinapoli/sbt-revolver.g8 (2012. Generic sbt project with sbt-revolver)
    - akiomik/scala-migrations.g8 (2013. A template for sbt projects using scala-migrations.)
    - ctranxuan/sbt-multi-modules.g8 (2013. A template for sbt multi-modules project)
    - joescii/scala-oss.g8 (2014. A template for creating an open-source Scala project.)
    - ymasory/sbt-code-quality.g8 (2013. CheckStyle and PMD for Java projects)
    - ymasory/sbt.g8 (2013. Generic sbt project for projects using GitHub)

- Scala Native
    - jokade/scalanative-cocoa-seed.g8 (Template for Cocoa projects with scala-native)

- Others
    - adinapoli/scalaz-revolver.g8 (Generic sbt project with scalaz 7 and sbt-revolver)
    - bneil/finch-skeleton.g8 (A project that centers on finagle/finch using circe, scalaz, scalacheck and shapeless)
    - bsamaripa/http4s.g8 (A simple http4s template)
    - edinhodzic/kyriakos-rest-micro-service-spray.g8 (REST CRUD microservice based on Sbt, Spray, MongoDb, Kamon metrics)
    - guardian/scala-aws-lambda.g8 (Scala AWS lambda g8 template)
    - jimschubert/finatra.g8 (A simple Finatra 2.5 template with sbt-revolver and sbt-native-packager)
    - joescii/presentera.g8 (A template for making awesome presentations with Presentera)
    - jmhofer/scalatron-bot.g8 (A template for Scalatron bots.)
    - julien-truffaut/presentation.g8 (A template using tut and remark.js)
    - mefellows/respite-sbt.g8 (sbt project for the Respite REST micro-framework)
    - polymorphic/gatling-simulation-template.g8 (Gatling simulations)
    - ripla/vaadin-scala.g8 (Template to easily get started with a Scala Vaadin project using the Scaladin add-on.)
    - anvie/spray-rest-sbt-0.11.2.g8 (2012. sbt template for Spray Rest sbt 0.11.2 included eclipse and idea plugins.)
    - chototsu/mmstempl.g8 (2013. Template for MikMikuStudio.)
    - codesolid/cassandra.g8 (2014. giter8 project featuring Cassandra in a simple ScalaTest project, with TypeSafe configuration)
    - exu/phalcon.g8.git (2012. CPhalcon basic template)
    - gfrison/proto-app.g8 (2013. Archetype of any Java or Groovy standalone application based on Spring (Grails DSL) and Gradle)
    - giTorto/template-openRefine-extension.g8 (2014. Template to easily get started with an OpenRefine extension.)
    - jackywyz/sbtweb-app.g8 (2014. sbt web project basic template)
    - jeffling/sbt-finagle.g8 (2013. Finagle Thrift template (with scrooge) with subprojects for the interface and server for easy deploying and publishing.)
    - jimschubert/finatra-app.g8 (2014. A template for a Finatra 1.5.3 Application using the bower/Bootstrap template provided by Finatra)
    - jrudolph/scalac-plugin.g8 (2012. sbt project for Scala Compiler Plugins)
    - marceloemanoel/gradle-plugin-template.g8 (2013. Gradle plugin template with info for deploying to Maven Central.)
    - matlockx/simple-gradle-scala.g8 (2013. A Scala project with Gradle and Heroku support.)
    - mesosphere/scala-sbt-mesos-framework.g8 (2014. A bare-bones Apache Mesos framework project in Scala.)
    - n8han/giter8.g8 (2014. sbt project for new giter8 templates)
    - non/caliper.g8 (2013. Template for writing micro-benchmarks using Caliper.)
    - omphe/ansible_project.g8 (2014. A template for Ansible projects according to best-practice directory layouts)
    - omphe/cr_scala_cli.g8 (2014. Simple Scala CLI app template with Scallop option handling)
    - philcali/lwjgl-project.g8 (2012. sbt project with LWJGL integration)
    - semberal/slick-sbt-project.g8 (2014. A template for Slick database access library with BoneCP connection pool and Typesafe config)
    - shadaj/scala-minecraft.g8 (2013. Bukkit plugins using Scala for Minecraft)
    - softprops/chrome-plugin.g8 (2012. Google Chrome Plugin g8 template)
    - tboloo/scala-jna-project.g8 (2014. A sample scala project for accessing native libraries with JNA)
    - tysonjh/scalaMacro.g8 (2014. A template for Scala Macros using the Macro Paradise plugin.)
    - tysonjh/sprayTwirlTemplate.g8 (2014. A template for spray-can with twirl, templating, assembly, revolver (Scala))
    - spotify/scio.g8 (2017. A template for Scio)
    - yarenty/h2o-spark-seed.g8 (2017. Starting template for Sparkling-Water(H2O) Machine Learning framework on Spark (Scala))
    - yarenty/tensorflow-scala-seed.g8 (2017. Template for production like project of Tensorflow using scala.
