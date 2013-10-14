{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -Wall #-}


module Main where


import           Control.Monad
import           Data.Monoid

import           Control.Lens              hiding (children)
import qualified Data.Text                 as T
import           Data.XML.Types
-- import qualified Data.XML.Types as XT
import qualified Filesystem.Path.CurrentOS as FS
import           Shelly
-- import           Text.XML
-- import qualified Text.XML as X
-- import           Text.XML.Cursor
import           Text.XML.Unresolved
import qualified Text.XML.Unresolved       as U

default (T.Text)


-- Parameters

inputDir :: FS.FilePath
inputDir = "Classics"

outputDir :: FS.FilePath
outputDir = "gk"

-- BetaCode conversion

fromBeta :: T.Text -> T.Text
fromBeta = undefined

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

emptyNode :: Node
emptyNode = NodeComment mempty

_nodeElement :: Lens' Node (Maybe Element)
_nodeElement = let ne (NodeElement e) = Just e
                   ne _               = Nothing
               in  lens ne $ const $ maybe emptyNode NodeElement

_nodeInstruction :: Lens' Node (Maybe Instruction)
_nodeInstruction = let ni (NodeInstruction i) = Just i
                       ni _                   = Nothing
                   in  lens ni $ const $ maybe emptyNode NodeInstruction

_nodeContent :: Lens' Node (Maybe Content)
_nodeContent = let nc (NodeContent c) = Just c
                   nc _               = Nothing
               in  lens nc $ const $ maybe emptyNode NodeContent

_nodeComment :: Lens' Node (Maybe T.Text)
_nodeComment = let nc (NodeComment c) = Just c
                   nc _               = Nothing
               in  lens nc $ const $ maybe emptyNode NodeComment

_elementName :: Lens' Element Name
_elementName = lens elementName $ \e n -> e { elementName = n }

_elementAttributes :: Lens' Element [(Name, [Content])]
_elementAttributes = lens elementAttributes $ \e a -> e { elementAttributes = a }

_elementNodes :: Lens' Element [Node]
_elementNodes = lens elementNodes $ \e n -> e { elementNodes = n }

emptyContent :: Content
emptyContent = ContentText mempty

_contentText :: Lens' Content (Maybe T.Text)
_contentText = let ct (ContentText t) = Just t
                   ct _               = Nothing
               in  lens ct $ const $ maybe emptyContent ContentText

_contentEntity :: Lens' Content (Maybe T.Text)
_contentEntity = let ce (ContentEntity e) = Just e
                     ce _                 = Nothing
                 in  lens ce $ const $ maybe emptyContent ContentEntity

_nameLocalName :: Lens' Name T.Text
_nameLocalName = lens nameLocalName $ \n ln -> n { nameLocalName = ln }

_nameNamespace :: Lens' Name (Maybe T.Text)
_nameNamespace = lens nameNamespace $ \n ns -> n { nameNamespace = ns }

_namePrefix :: Lens' Name (Maybe T.Text)
_namePrefix = lens namePrefix $ \n p -> n { namePrefix = p }

children :: Lens' Element [Node]
children = lens elementNodes $ \e ns -> e { elementNodes = ns }

attributes :: Lens' Element [(Name, [Content])]
attributes = lens elementAttributes $ \e attrs ->
    e { elementAttributes = attrs }

attributeValue :: Name -> Element -> Maybe [Content]
attributeValue name =
    firstOf (folded . filtered ((== name) . fst) . _2) . elementAttributes

element :: Name -> [Node] -> [Node]
element name = filter (hasName name)
        where hasName :: Name -> Node -> Bool
              hasName n (NodeElement (Element en _ _)) = en == n
              hasName _ _                              = False

-- And the transformations

transformDoc :: Document -> Document
transformDoc doc = let doc' = doc & doctype .~ Nothing
                   in  doc' & root %~ transformElement

mapChildren :: (Node -> Node) -> Element -> Element
mapChildren f = over (children . traverse) f

mapElement :: (Element -> Element) -> Element -> Element
mapElement f el = let el' = f el
                  in  el' { elementNodes = map (mapNode f) $ elementNodes el' }
    where mapNode :: (Element -> Element) -> Node -> Node
          mapNode f' (NodeElement e) = NodeElement $ mapElement f' e
          mapNode _  node            = node

transformElement :: Element -> Element
transformElement el@(Element "text" attrs _)
        | ("lang", [ContentText "greek"]) `elem` attrs = mapChildren transformGreekNode el
        | otherwise = mapChildren transformNode el
transformElement el = mapChildren transformNode el

transformNode :: Node -> Node
transformNode (NodeElement el) = NodeElement $ transformElement el
transformNode n                = n

transformGreekNode :: Node -> Node
transformGreekNode (NodeElement el)              = NodeElement $ transformGreekElement el
transformGreekNode (NodeContent (ContentText c)) = NodeContent . ContentText $ fromBeta c
transformGreekNode n                             = n

transformGreekElement :: Element -> Element
transformGreekElement = mapChildren transformGreekNode

stripDoctype :: Document -> Document
stripDoctype doc = doc & doctype .~ Nothing

-- Program stuff

main :: IO ()
main = shelly $ verbosely $ do
    gkFiles <- findWhen isGkFile inputDir
    forM_ gkFiles $ \inFile -> do
        let outFile = outputDir </> inFile
        echo $ toTextIgnore inFile <> "\t=>\t" <> toTextIgnore outFile
        mkdir_p $ FS.directory outFile
        doc <- liftIO $ U.readFile def inFile
        let doc' = transformDoc doc
        liftIO $ U.writeFile (def { rsPretty = True }) outFile doc'

isGkFile :: FS.FilePath -> Sh Bool
isGkFile = return . T.isSuffixOf "_gk.xml" . toTextIgnore
