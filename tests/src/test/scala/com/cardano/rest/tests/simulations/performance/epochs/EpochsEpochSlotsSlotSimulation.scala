package com.cardano.rest.tests.simulations.performance.epochs

import java.net.URL
import java.util.Properties

import com.cardano.rest.tests.DataStore
import io.gatling.core.Predef._
import io.gatling.core.structure.{ChainBuilder, ScenarioBuilder}
import io.gatling.http.Predef._
import io.gatling.http.protocol.HttpProtocolBuilder

import scala.concurrent.duration.DurationInt
import scala.io.Source


class EpochsEpochSlotsSlotSimulation extends Simulation {

  var properties : Properties = _
  val url: URL = getClass.getResource("/config.properties")
  if (url != null) {
    val source = Source.fromURL(url)

    properties = new Properties()
    properties.load(source.bufferedReader())
  }

  val host: String = properties.getProperty("host")
  val pauseBetweenTests: Int = properties.getProperty("pauseBetweenTests").toInt
  val pauseBetweenRequests: Int = properties.getProperty("pauseBetweenRequests").toInt
  val startingUsers: Int = properties.getProperty("startingUsers").toInt
  val maximumUsers: Int = properties.getProperty("maximumUsers").toInt
  val timeFrameToIncreaseUsers: Int = properties.getProperty("timeFrameToIncreaseUsers").toInt
  val maxTestDuration: Int = properties.getProperty("maxTestDuration").toInt


  val dataStore = new DataStore

  val httpConf: HttpProtocolBuilder = http.baseUrl(host)
    .header("Accept", "application/json")

  def getEpochsEpochSlotsSlot: ChainBuilder = {
    exec (
      http("Get epochs/{epoch}/slots/{slot}")
        .get(String.format("epochs/%s/slots/%s", dataStore.getEpoch, dataStore.getSlot))
        .check(status.is(200))
    )
  }

  val scn: ScenarioBuilder = scenario("performance test: epochs/{epoch}/slots/{slot}")
    .forever(
      exec(getEpochsEpochSlotsSlot)
        .pause(pauseBetweenRequests seconds)
    )

  setUp(
    scn.inject(
      nothingFor(pauseBetweenTests seconds),
      atOnceUsers(startingUsers),
      rampUsers(maximumUsers) during (timeFrameToIncreaseUsers seconds)
    ).protocols(httpConf)
  ).maxDuration(maxTestDuration seconds)
}
