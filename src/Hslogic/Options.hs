module Hslogic.Options where

import Options.Applicative (Parser, ParserInfo, ParserResult (..), auto, defaultPrefs, execParserPure, help, helper, info, long, many, metavar, option, progDesc, short, (<**>), strOption)

data Options = Load [FilePath]
  deriving (Eq, Show)

parseArgs :: [String] -> Either String Options
parseArgs args = case execParserPure defaultPrefs parseOptions args of
  Success opts -> Right opts
  Failure err -> Left $ show err
  _ -> Left "Unsupported completion request"

parseOptions :: ParserInfo Options
parseOptions =
  info
    (optionsParser <**> helper)
    ( progDesc $
        unlines
          [ "hslogic - A mini-Prolog interpreter"
          ]
    )

optionsParser :: Parser Options
optionsParser = Load <$> many loadFileParser

loadFileParser :: Parser FilePath
loadFileParser =
  strOption
    ( long "load"
        <> short 'l'
        <> metavar "FILE"
        <> help "Load and interpret given file as a minilog program"
    )
