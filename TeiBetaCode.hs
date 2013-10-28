{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -Wall #-}


module Main where


import           Control.Monad
import           Data.Monoid
import           Data.String               (fromString)
import           Prelude                   hiding (FilePath)

import           Control.Lens              hiding (children)
import qualified Data.Text                 as T
import           Data.XML.Types
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative       hiding ((&))
import           Shelly
import           System.FilePath.Glob
import           Text.XML.Unresolved
import qualified Text.XML.Unresolved       as U

import           Text.BetaCode

default (T.Text)


-- XML handling

-- There are much nicer interfaces available if all the entities can be
-- resolved. Unfortunately, I can't do that, so I'm stuck rolling my own with
-- pattern matching.

-- Some lenses to make things easier.

root :: Lens' Document Element
root = lens documentRoot $ \d e -> d { documentRoot = e }

doctype :: Lens' Document (Maybe Doctype)
doctype = lens (prologueDoctype . documentPrologue) $ \doc dtd ->
    let p = documentPrologue doc
    in  doc { documentPrologue = p { prologueDoctype = dtd } }

_elementNodes :: Lens' Element [Node]
_elementNodes = lens elementNodes $ \e n -> e { elementNodes = n }

-- Some of these should be traversals or prisms or somesuch.

children :: Lens' Element [Node]
children = lens elementNodes $ \e ns -> e { elementNodes = ns }

-- And the transformations

transformDoc :: Document -> Document
transformDoc doc = doc & doctype .~ Nothing
                       & root    %~ transformElement

mapChildren :: (Node -> Node) -> Element -> Element
mapChildren f = over (children . traverse) f

transformElement :: Element -> Element
transformElement el@(Element "text" _ _) = mapChildren transformGreekNode el
transformElement el                      = mapChildren transformNode el

transformNode :: Node -> Node
transformNode (NodeElement el) = NodeElement $ transformElement el
transformNode n                = n

transformGreekNode :: Node -> Node
transformGreekNode (NodeElement el)              = NodeElement $ transformGreekElement el
transformGreekNode (NodeContent (ContentText c)) = NodeContent . ContentText $ fromBetaIgnore c
transformGreekNode n                             = n

transformGreekElement :: Element -> Element
transformGreekElement e@(Element "bibl" _ _) = e
transformGreekElement el                     = mapChildren transformGreekNode el

-- Program stuff

main :: IO ()
main = do
    config <- execParser opts
    let inputDir  = (fromString $ cfgInputDir config  :: FilePath)
        outputDir = (fromString $ cfgOutputDir config :: FilePath)
        isGkFile  = return
                  . match (compile (cfgGreekGlob config))
                  . FS.encodeString
                  . FS.filename
    shelly $ verbosely $ do
        gkFiles <- findWhen isGkFile inputDir
        forM_ gkFiles $ \inFile -> do
            outFile <- canonic $ outputDir </> inFile
            echo $ toTextIgnore inFile <> "\t=>\t" <> toTextIgnore outFile
            mkdir_p $ FS.directory outFile
            liftIO $   U.writeFile (def { rsPretty = True }) outFile
                   =<< transformDoc
                   <$> U.readFile def inFile
    where opts' =   TeiBetaCode
                <$> strOption (  short 'i' <> long "input-dir" <> value "./Classics/" <> metavar "INPUT_DIR"
                              <> help "The input directory to walk for files matching GREEK_GLOB.")
                <*> strOption (  short 'g' <> long "greek-glob" <> value "*_gk.xml" <> metavar "GREEK_GLOB"
                              <> help "The file name pattern to match for files to process.")
                <*> strOption (  short 'o' <> long "output-dir" <> value "./gk/" <> metavar "OUTPUT_DIR"
                              <> help "The directory to place the output files into.")
          opts  = info (helper <*> opts') (  fullDesc
                                          <> progDesc "Converts the beta code in a directory of TEI documents to Unicode.")

data TeiBetaCode = TeiBetaCode
                 { cfgInputDir  :: String
                 , cfgGreekGlob :: String
                 , cfgOutputDir :: String
                 }

