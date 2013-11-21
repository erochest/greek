{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module Main where


import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                 as T
import           Data.XML.Types
import           Filesystem
import           Filesystem.Path.CurrentOS
import qualified Filesystem.Path.CurrentOS as FS
import           Options.Applicative       hiding ((&))
import qualified Options.Applicative       as O
import           Prelude                   hiding (FilePath)
import           Text.XML.Unresolved
import qualified Text.XML.Unresolved       as U

{-

doc <- Text.XML.Unresolved.readFile Text.XML.Unresolved.def "gk/Classics/Plato/opensource/plat.tet1_gk.xml"
let texts =  getTexts doc

-}


-- Data Types

data SplitOn = SplitText
             | SplitPage
             | SplitSection
             | SplitSpeaker
             deriving (Show)


-- Lenses

_NodeElement :: Prism' Node Element
_NodeElement = prism' NodeElement get
    where get (NodeElement e) = Just e
          get _               = Nothing

_NodeInstruction :: Prism' Node Instruction
_NodeInstruction = prism' NodeInstruction get
    where get (NodeInstruction i) = Just i
          get _                   = Nothing

_NodeContent :: Prism' Node Content
_NodeContent = prism' NodeContent get
    where get (NodeContent c) = Just c
          get _               = Nothing

_NodeComment :: Prism' Node T.Text
_NodeComment = prism' NodeComment get
    where get (NodeComment c) = Just c
          get _               = Nothing

_elementName :: Lens' Element Name
_elementName = lens elementName $ \e n -> e { elementName = n }

_named :: forall (f :: * -> *). Applicative f
       => Name -> (Element -> f Element) -> Element -> f Element
_named name =
    _elementNodes . traverse . _NodeElement . filtered ((== name) . view _elementName)

_elementAttributes :: Lens' Element [(Name, [Content])]
_elementAttributes = lens elementAttributes $ \e a -> e { elementAttributes = a }

_elementAttribute :: (Applicative f, Indexable Int p, Contravariant f)
                  => Name -> p [Content] (f [Content]) -> Element -> f Element
_elementAttribute name =
    _elementAttributes . folded . filtered ((== name) . view _1) . _2

_elementNodes :: Lens' Element [Node]
_elementNodes = lens elementNodes $ \e n -> e { elementNodes = n }

_ContentText :: Prism' Content T.Text
_ContentText = prism' ContentText get
    where get (ContentText t) = Just t
          get _               = Nothing

_ContentEntity :: Prism' Content T.Text
_ContentEntity = prism' ContentEntity get
    where get (ContentEntity t) = Just t
          get _                 = Nothing

_nameLocalName :: Lens' Name T.Text
_nameLocalName = lens nameLocalName $ \n ln -> n { nameLocalName = ln }

_nameNamespace :: Lens' Name (Maybe T.Text)
_nameNamespace = lens nameNamespace $ \n ns -> n { nameNamespace = ns }

_namePrefix :: Lens' Name (Maybe T.Text)
_namePrefix = lens namePrefix $ \n p -> n { namePrefix = p }

-- Some HOFs Related to Lenses.

mapElementContent :: (Content -> Content) -> Element -> Element
mapElementContent f el = el & _elementAttributes . traverse . _2 . traverse %~ f
                            & _elementNodes . traverse %~ mapNodeContent f

mapNodeContent :: (Content -> Content) -> Node -> Node
mapNodeContent f (NodeElement el) = NodeElement $ mapElementContent f el
mapNodeContent f (NodeContent c)  = NodeContent $ f c
mapNodeContent _ n                = n

-- Replacing the Entities

entity :: Content -> Content
entity   (ContentEntity "dagger") = ContentText $ T.singleton '†'
entity   (ContentEntity "dash")   = ContentText $ T.singleton '-'
entity   (ContentEntity "ldquo")  = ContentText $ T.singleton '“'
entity   (ContentEntity "lpar")   = ContentText $ T.singleton '('
entity   (ContentEntity "lsqb")   = ContentText $ T.singleton '['
entity   (ContentEntity "lsquo")  = ContentText $ T.singleton '‘'
entity   (ContentEntity "mdash")  = ContentText $ T.singleton '—'
entity   (ContentEntity "rdquo")  = ContentText $ T.singleton '”'
entity   (ContentEntity "rpar")   = ContentText $ T.singleton ')'
entity   (ContentEntity "rsqb")   = ContentText $ T.singleton ']'
entity   (ContentEntity "rsquo")  = ContentText $ T.singleton '’'
entity   (ContentEntity name)     = ContentText $ "&" <> name <> ";"
entity c@(ContentText _)          = c

-- Processing the Output

prepareOutputDir :: FilePath -> IO ()
prepareOutputDir outDirname = do
    exists <- isDirectory outDirname
    when exists $ removeTree outDirname
    createTree outDirname


-- Working with Texts and Files


getTexts :: Document -> [Element]
getTexts d =   elementChildren (documentRoot d) >>= isNamed "text"
           >>= elementChildren >>= isNamed "group"
           >>= elementChildren >>= isNamed "text" >>= hasAttribute "n"

getN :: Element -> Maybe T.Text
getN el = listToMaybe [ n | T.null n ]
    where n = mconcat $ el ^.. _elementAttribute "n" . traverse . _ContentText

makeOutputName :: FilePath -> T.Text -> FilePath
makeOutputName dir n = dir FS.</> n' FS.<.> "xml"
    where n' = fromText $ T.map clean n
          clean c | isAlphaNum c = c
                  | otherwise    = '_'


wrapDoc :: Element -> Document
wrapDoc el = Document (Prologue [] Nothing []) el []

-- Main and CLI

main :: IO ()
main = do
    config <- execParser opts

    let outDir = decodeString $ outputDir config
    prepareOutputDir outDir

    -- TODO: Could clean this next line up with arrows
    texts <-  map (\t -> (makeOutputName outDir <$> getN t, t))
          .   map (mapElementContent entity)
          .   getTexts
          <$> (U.readFile def
          .   decodeString
          $   inputFile config)

    forM_ texts $ \(outFile, text) ->
        maybe (return ()) (flip (U.writeFile writeOpts) (wrapDoc text)) outFile

    where opts = info (helper <*> splitXml)
                      (  fullDesc
                      <> progDesc "Splits an XML file into parts based on the input options."
                      <> header "split-xml"
                      )
          writeOpts = def { rsPretty = True }

-- TODO: Not chunking here, so remove chunk size, overlap, and split on.
data SplitXml = SplitXml
              { splitOn   :: SplitOn
              , inputFile :: String
              , outputDir :: String
              , chunkSize :: Int
              , overlap   :: Int
              } deriving (Show)

splitXml :: O.Parser SplitXml
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
        <*> O.option    (  short 'c' <> long "chunk-size" <> metavar "TOKEN_COUNT" <> value 500
                        <> help "The size of the chunks. Default is 500."
                        )
        <*> O.option    (  short 'l' <> long "overlap" <> metavar "TOKEN_OVERLAP" <> value 250
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
parseSplitOn x         = fail $ "Unknown SplitOn value: " <> x

