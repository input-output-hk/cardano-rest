package com.cardano.rest.tests.simulations.performance.addresses


import java.net.URL
import java.util.Properties

import com.cardano.rest.tests.DataStore
import io.gatling.core.Predef._
import io.gatling.core.structure.{ChainBuilder, ScenarioBuilder}
import io.gatling.http.Predef._
import io.gatling.http.protocol.HttpProtocolBuilder

import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.language.postfixOps


class AddressesSummaryAddressSimulation extends Simulation {

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

  // Set-up the URL
  val httpConf: HttpProtocolBuilder = http.baseUrl(host)
    .header("Accept", "application/json")

  // Call the URL and set assertions
  def getAddressesSummaryAddress: ChainBuilder = {
    exec (
      http("Get addresses/summary/{address}")
        .get(String.format("addresses/summary/%s", dataStore.getAddressHash))
        .check(status.is(200))
    )
  }

  // Create the scenario
  val scn: ScenarioBuilder = scenario("performance test: addresses/summary/{address}")
    .forever(
      exec(getAddressesSummaryAddress)
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
