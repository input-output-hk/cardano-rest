package com.cardano.rest.tests

import java.net.URL
import java.util.Properties

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

class DataStore {

  var properties : Properties = _
  val url: URL = getClass.getResource("/config.properties")
  if (url != null) {
    val source = Source.fromURL(url)

    properties = new Properties()
    properties.load(source.bufferedReader())
  }

  val network: String = properties.getProperty("network")

  var addresses: ListBuffer[String] = _
  var blocks: ListBuffer[String] = _
  var blocksAndAddresses: ListBuffer[String] = _
  var transactions: ListBuffer[String] = _

  def getEpoch: String = {
    val r: Random.type = scala.util.Random
    r.nextInt(78).toString
  }

  def getSlot: String = {
    val r: Random.type = scala.util.Random
    r.nextInt(21599).toString
  }

  def getAddressHash: String = {
    var lines: List[String] = null

    if (network == "testnet") {
      if (addresses != null) {
        lines = addresses.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/testnet/addresses.txt"))
        lines = linesSource.getLines().toList
        addresses = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else if (network == "mainnet") {
      if (addresses != null) {
        lines = addresses.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/mainnet/addresses.txt"))
        lines = linesSource.getLines().toList
        addresses = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else {
      throw new Exception("unknown network configuration value")
    }

    lines(Random.nextInt(lines.size))
  }

  def getBlockHash: String = {
    var lines: List[String] = null

    if (network == "testnet") {
      if (blocks != null) {
        lines = blocks.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/testnet/blocks.txt"))
        lines = linesSource.getLines().toList
        blocks = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else if (network == "mainnet") {
      if (blocks != null) {
        lines = blocks.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/mainnet/blocks.txt"))
        lines = linesSource.getLines().toList
        blocks = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else {
      throw new Exception("unknown network configuration value")
    }

    lines(Random.nextInt(lines.size))
  }

  def getBlockHashAndAddress: String = {
    var lines: List[String] = null

    if (network == "testnet") {
      if (blocksAndAddresses != null) {
        lines = blocksAndAddresses.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/testnet/blocks_and_addresses.txt"))
        lines = linesSource.getLines().toList
        blocksAndAddresses = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else if (network == "mainnet") {
      if (blocksAndAddresses != null) {
        lines = blocksAndAddresses.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/mainnet/blocks_and_addresses.txt"))
        lines = linesSource.getLines().toList
        blocksAndAddresses = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else {
      throw new Exception("unknown network configuration value")
    }

    lines(Random.nextInt(lines.size))
  }

  def getTransactionHash: String = {
    var lines: List[String] = null

    if (network == "testnet") {
      if (transactions != null) {
        lines = transactions.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/testnet/transactions.txt"))
        lines = linesSource.getLines().toList
        transactions = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else if (network == "mainnet") {
      if (transactions != null) {
        lines = transactions.to[List]
      }
      else {
        val linesSource = Source.fromInputStream(getClass.getResourceAsStream("/data/mainnet/transactions.txt"))
        lines = linesSource.getLines().toList
        transactions = lines.to[ListBuffer]
        linesSource.close()
      }
    }
    else {
      throw new Exception("unknown network configuration value")
    }

    lines(Random.nextInt(lines.size))
  }
}
