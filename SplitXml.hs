{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Options.Applicative
import           Text.XML.Unresolved
import qualified Text.XML.Unresolved       as U


data SplitOn = SplitText
             | SplitPage
             | SplitSection
             | SplitSpeaker
             deriving (Show)

main :: IO ()
main = do
    execParser opts >>= print
    where opts = info (helper <*> splitXml)
                      (  fullDesc
                      <> progDesc "Splits an XML file into parts based on the input options."
                      <> header "split-xml"
                      )

data SplitXml = SplitXml
              { splitOn   :: SplitOn
              , inputFile :: String
              , outputDir :: String
              , chunkSize :: Int
              , overlap   :: Int
              } deriving (Show)

splitXml :: Parser SplitXml
splitXml =   SplitXml
         <$> nullOption (  short 's' <> long "split-on" <> metavar "STRUCTURE" <> reader parseSplitOn
                        <> help "The structure to split on. One of 'text', 'page', 'section', 'speaker'."
                        )
        <*> strOption   (  short 'i' <> long "input" <> metavar "FILENAME"
                        <> help "The input file to split."
                        )
        <*> strOption   (  short 'o' <> long "output" <> metavar "DIRECTORY"
                        <> help "The directory to put the output chunks into."
                        )
        <*> option      (  short 'c' <> long "chunk-size" <> metavar "TOKEN_COUNT" <> value 500
                        <> help "The size of the chunks. Default is 500."
                        )
        <*> option      (  short 'l' <> long "overlap" <> metavar "TOKEN_OVERLAP" <> value 250
                        <> help "The amount chunks should overlap. Default is 250."
                        )

parseSplitOn :: Monad m => String -> m SplitOn
parseSplitOn "text"    = return SplitText
parseSplitOn "t"       = return SplitText
parseSplitOn "page"    = return SplitPage
parseSplitOn "p"       = return SplitPage
parseSplitOn "section" = return SplitSection
parseSplitOn "s"       = return SplitSection
parseSplitOn "speaker" = return SplitSpeaker
parseSplitOn "sp"      = return SplitSpeaker

