{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}


module Main where


import           Control.Arrow
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

-- This particular list of entities was generated with this unholy shell command:
-- egrep '&[^;]*;' gk/Classics/Plato/opensource/plat.tet* | sed 's/.*\(&[^;]*;\).*/\1/g' | sort | uniq | pbcopy
entity :: Content -> Content
entity   (ContentEntity "dagger") = contentChar '†'
entity   (ContentEntity "dash")   = contentChar '-'
entity   (ContentEntity "ldquo")  = contentChar '“'
entity   (ContentEntity "lpar")   = contentChar '('
entity   (ContentEntity "lsqb")   = contentChar '['
entity   (ContentEntity "lsquo")  = contentChar '‘'
entity   (ContentEntity "mdash")  = contentChar '—'
entity   (ContentEntity "rdquo")  = contentChar '”'
entity   (ContentEntity "rpar")   = contentChar ')'
entity   (ContentEntity "rsqb")   = contentChar ']'
entity   (ContentEntity "rsquo")  = contentChar '’'
entity   (ContentEntity name)     = ContentText $ "&" <> name <> ";"
entity c@(ContentText _)          = c

contentChar :: Char -> Content
contentChar = ContentText . T.singleton

-- Processing the Output

prepareOutputDir :: FilePath -> IO ()
prepareOutputDir = createTree


-- Working with Texts and Files


getTexts :: Document -> [Element]
getTexts d =   elementChildren (documentRoot d) >>= isNamed "text"
           >>= elementChildren >>= isNamed "group"
           >>= elementChildren >>= isNamed "text" >>= hasAttribute "n"

getN :: Element -> Maybe T.Text
getN el = listToMaybe [ n | not (T.null n) ]
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

    let outDir  = decodeString $ outputDir config
        makeOut = fmap (makeOutputName outDir) . getN
    prepareOutputDir outDir

    logS ">>>" $ inputFile config
    texts <-   map ((makeOut &&& arr id) . mapElementContent entity) . getTexts
          <$> (U.readFile def . decodeString $ inputFile config)

    forM_ texts $ \(outFile, text) ->
        maybe (return ())
              (void . liftPair . (flip (U.writeFile writeOpts) (wrapDoc text) &&& logF "<<<"))
              outFile

    where opts = info (helper <*> splitXml)
                      (  fullDesc
                      <> progDesc "Splits an XML file into parts based on the input options."
                      <> header "split-xml"
                      )
          writeOpts = def { rsPretty = True }
          logS pref = putStrLn . (pref <>) . (' ' :)
          logF pref = logS pref . encodeString
          liftPair pair = (,) <$> fst pair <*> snd pair

data SplitXml = SplitXml
              { inputFile :: String
              , outputDir :: String
              } deriving (Show)

splitXml :: O.Parser SplitXml
splitXml =   SplitXml
         <$> strOption   (  short 'i' <> long "input" <> metavar "FILENAME"
                         <> help "The input file to split."
                         )
         <*> strOption   (  short 'o' <> long "output" <> metavar "DIRECTORY"
                         <> help "The directory to put the output chunks into."
                         )
 
