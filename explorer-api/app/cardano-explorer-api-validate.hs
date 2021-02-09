
import           Explorer.Web.Validate

import           Options.Applicative (Parser, ParserInfo, ParserPrefs)
import qualified Options.Applicative as Opt


main :: IO ()
main = do
    Opt.customExecParser p opts >>= runCommand
  where
    opts :: ParserInfo Command
    opts = Opt.info (Opt.helper <*> pVersion <*> pCommand)
      ( Opt.fullDesc
      <> Opt.header "cardano-webapi-validate - Run validations on the webapi"
      )

    p :: ParserPrefs
    p = Opt.prefs Opt.showHelpOnEmpty

-- -----------------------------------------------------------------------------

newtype Command
  = Validate Word

runCommand :: Command -> IO ()
runCommand cmd = do
  case cmd of
    Validate count -> runValidation count

pVersion :: Parser (a -> a)
pVersion =
  Opt.infoOption "cardano-webapi-validate version 0.1.0.0"
    (  Opt.long "version"
    <> Opt.short 'v'
    <> Opt.help "Print the version and exit"
    )

pCommand :: Parser Command
pCommand =
  Opt.subparser
    ( Opt.command "validate"
        ( Opt.info pValidate
          $ Opt.progDesc "Run validation checks against the webapi."
          )
    )
  where
    pValidate :: Parser Command
    pValidate =
      Validate <$> pCount

    pCount :: Parser Word
    pCount =
      Opt.option Opt.auto
        (  Opt.long "count"
        <> Opt.help "The number of validations to run."
        <> Opt.value 10
        <> Opt.showDefault
        )
