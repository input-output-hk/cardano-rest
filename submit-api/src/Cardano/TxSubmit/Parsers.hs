{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TxSubmit.Parsers
  ( opts
  , pTxSubmitNodeParams
  , pConfigFile
  , pNetworkId
  , pProtocol
  , pSocketPath
  , pWebPort
  ) where

import           Prelude (read)
import           Cardano.Prelude

import           Options.Applicative (Parser, ParserInfo, (<**>))
import qualified Options.Applicative as Opt

import           Cardano.Api.Protocol (Protocol (..))
import           Cardano.Api.Typed (NetworkId (..), NetworkMagic (..))

import           Cardano.Chain.Slotting (EpochSlots (..))

import           Cardano.TxSubmit.Node (ConfigFile (..), SocketPath (..),
                    TxSubmitNodeParams (..))
import           Cardano.TxSubmit.Types (TxSubmitPort (..))

import           Ouroboros.Consensus.Cardano (SecurityParam (..))

opts :: ParserInfo TxSubmitNodeParams
opts =
  Opt.info (pTxSubmitNodeParams <**> Opt.helper)
    ( Opt.fullDesc
    <> Opt.progDesc "Cardano transaction submission web API."
    )

pTxSubmitNodeParams :: Parser TxSubmitNodeParams
pTxSubmitNodeParams =
  TxSubmitNodeParams
    <$> pConfigFile
    <*> pProtocol
    <*> pNetworkId
    <*> pSocketPath
    <*> pWebPort

pConfigFile :: Parser ConfigFile
pConfigFile =
  ConfigFile <$> Opt.strOption
    ( Opt.long "config"
    <> Opt.help "Path to the tx-submit web API configuration file"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

-- TODO: This was ripped from `cardano-cli` because, unfortunately, it's not
-- exported. Once we export this parser from the appropriate module and update
-- our `cardano-cli` dependency, we should remove this and import the parser
-- from there.
pNetworkId :: Parser NetworkId
pNetworkId =
  pMainnet <|> fmap Testnet pTestnetMagic
 where
   pMainnet :: Parser NetworkId
   pMainnet =
    Opt.flag' Mainnet
      (  Opt.long "mainnet"
      <> Opt.help "Use the mainnet magic id."
      )

   pTestnetMagic :: Parser NetworkMagic
   pTestnetMagic =
     NetworkMagic <$>
       Opt.option Opt.auto
         (  Opt.long "testnet-magic"
         <> Opt.metavar "NATURAL"
         <> Opt.help "Specify a testnet magic id."
         )


-- TODO: This was ripped from `cardano-cli` because, unfortunately, it's not
-- exported. Once we export this parser from the appropriate module and update
-- our `cardano-cli` dependency, we should remove this and import the parser
-- from there.
pProtocol :: Parser Protocol
pProtocol =
    (  Opt.flag' ()
        (  Opt.long "shelley-mode"
        <> Opt.help "For talking to a node running in Shelley-only mode (default)."
        )
    *> pShelley
    )
  <|>
    (  Opt.flag' ()
        (  Opt.long "byron-mode"
        <> Opt.help "For talking to a node running in Byron-only mode."
        )
    *> pByron
    )
  <|>
    (  Opt.flag' ()
        (  Opt.long "cardano-mode"
        <> Opt.help "For talking to a node running in full Cardano mode."
        )
    *> pCardano
    )
  <|>
    -- Default to the Shelley protocol for now, due to the testnet.
    pure ShelleyProtocol
  where
    pByron :: Parser Protocol
    pByron = ByronProtocol <$> pEpochSlots <*> pSecurityParam

    pShelley :: Parser Protocol
    pShelley = pure ShelleyProtocol

    pCardano :: Parser Protocol
    pCardano = CardanoProtocol <$> pEpochSlots <*> pSecurityParam

    pEpochSlots :: Parser EpochSlots
    pEpochSlots =
      EpochSlots <$>
        ( Opt.option Opt.auto
            (  Opt.long "epoch-slots"
            <> Opt.metavar "NATURAL"
            <> Opt.help "The number of slots per epoch (default is 21600)."
            )
        <|>
          -- Default to the mainnet value.
          pure 21600
        )

    pSecurityParam :: Parser SecurityParam
    pSecurityParam =
      SecurityParam <$>
        ( Opt.option Opt.auto
            (  Opt.long "security-param"
            <> Opt.metavar "NATURAL"
            <> Opt.help "The security parameter (default is 2160)."
            )
        <|>
          -- Default to the mainnet value.
          pure 2160
        )

pSocketPath :: Parser SocketPath
pSocketPath =
  SocketPath <$> Opt.strOption
    ( Opt.long "socket-path"
    <> Opt.help "Path to a cardano-node socket"
    <> Opt.completer (Opt.bashCompleter "file")
    <> Opt.metavar "FILEPATH"
    )

pWebPort :: Parser TxSubmitPort
pWebPort =
  TxSubmitPort . read <$> Opt.strOption
    ( Opt.long "port"
    <> Opt.help "The port the web API should listen on"
    <> Opt.metavar "PORT"
    )
