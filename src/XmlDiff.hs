{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Data.TreeDiff.Tree
import Text.XML.Light
import System.Environment
import System.Directory (doesFileExist)
import System.FilePath
import Control.Monad (when)
import Data.Tree
import Data.Foldable (for_)
import Data.Char   (isSpace)

import System.Console.GetOpt
import System.Exit (die)

main :: IO ()
main = do
  
  prog <- getProgName
  argv <- getArgs
  let header = prog ++ ": [OPTION...] FILE1 FILE2"

  -- Argument/option parsing
  (opts, fp1, fp2)
    <- case getOpt Permute options argv of
         (o, [f1,f2], []) -> pure (foldl (flip id) defaultOptions o, f1, f2)
         (_, _, errs) -> die (usageInfo header options ++ unlines errs)
 
  for_ [fp1,fp2] $ \fp -> do
    e <- doesFileExist fp
    when (not e) (die $ "There is no file at `" ++ fp ++ "'")

  -- Parse the XML
  xml1 <- parseXml fp1
  xml2 <- parseXml fp2

  -- Perform the diff
  let Options{ optFileOutput   = optFp3
             , optTokenizeFunc = toks
             , optAddedCss     = aCss
             , optRemovedCss   = rCss } = opts
  diffedXml 
    <- case element2Tree toks xml2 `treeDiff` element2Tree toks xml1 of
         Cpy editTree
           | Just html <- addStyleNode aCss rCss (editTree2Element editTree)
           -> pure html
         _ -> die $ "Both files are expected to have a top-level <html> tag"

  -- Print the output
  let ppConf = useShortEmptyTags (flip elem voidTags . qName) defaultConfigPP
      prettyXml = ppcElement ppConf diffedXml
  case optFp3 of
    StdOut -> putStrLn prettyXml
    File fp3 -> writeFile fp3 prettyXml
    Computed -> do
      let fp3 = takeBaseName fp2 ++ "-" ++ takeBaseName fp1
      putStrLn $ "Writing the diffed output to `" ++ fp3 ++ "'"
      writeFile fp3 prettyXml


-- * Program options

type Css = String

data FileOutput = StdOut                  -- ^ to standard out
                | Computed                -- ^ at @<fp1>-<fp2>.html@
                | File FilePath           -- ^ at this file path

data Options = Options
  { optFileOutput   :: FileOutput         -- ^ where to write the output file
  , optTokenizeFunc :: String -> [String] -- ^ how to tokenize text elements
  , optAddedCss     :: Css                -- ^ CSS body of the @added@ class
  , optRemovedCss   :: Css                -- ^ CSS body of the @removed@ class
  }

defaultOptions :: Options
defaultOptions = Options
  { optFileOutput = Computed
  , optTokenizeFunc = hsTokenizer

    -- Dark green text, light green background
  , optAddedCss =   "color:            rgb(0, 153, 51);   " ++
                    "background-color: rgba(0, 255, 0, .2)"

    -- Red struck through text, light red background
  , optRemovedCss = "color:            rgb(255, 0, 0);    " ++
                    "text-decoration:  line-through;      " ++
                    "background-color: rgba(255, 0, 0, .2)"
  }

-- | Command line options
options :: [OptDescr (Options -> Options)]
options = [ Option ['o']  ["output"]
                   (ReqArg (\fp opts -> opts { optFileOutput = File fp })
                           "FILE")
                   "Output FILE"
          , Option []     ["stdout"]
                   (NoArg (\opts -> opts { optFileOutput = StdOut }))
                   "Output to STDOUT"
          , Option []     ["added-css"]
                   (ReqArg (\cssSrc opts -> opts { optAddedCss = cssSrc })
                           "CSS-STRING")
                   "The CSS to apply to added stuff"
          , Option []     ["removed-css"]
                   (ReqArg (\cssSrc opts -> opts { optRemovedCss = cssSrc })
                           "CSS-STRING")
                   "The CSS to apply to removed stuff"
          , Option []     ["no-tokenizer"]
                   (NoArg (\opts -> opts { optTokenizeFunc = noTokenizer }))
                   "Don't tokenize the text when diffing it"
          , Option []     ["hs-tokenizer"]
                   (NoArg (\opts -> opts { optTokenizeFunc = hsTokenizer }))
                   "Diff tokenize text along Haskell tokens"
          ]


-- * Parsing

-- | The 'treeDiff' function which drives the core diffing functionality works
-- over 'Tree'. We flatten the slightly more complex types from "Text.XML.Light"
-- into a @'Tree' 'ElementNode'@ (see 'element2Tree').
data ElementNode
  = QQName QName 
    -- ^ Represents an 'Element' or 'Elem' node. It's first child should always
    -- be a 'QAttrs' node. The remaining nodes should content nodes (so one
    -- of 'QQName', 'QCData', or 'QCRef').

--  | QAttrs
    -- ^ Represents attributes. It's children should always be `QAttr`'s.

  | QAttr Attr   -- ^ Represents an 'Attr' and should never have children.
  | QCData CData -- ^ Represents a 'Text' and should never have children.
  | QCRef String -- ^ Represents a 'CRef' and should never have children.
  deriving (Show, Eq)


-- | Parse a file into its XML structure
parseXml :: FilePath -> IO Element
parseXml fp = do
  fpStr <- readFile fp
  case parseXMLDoc fpStr of
    Nothing -> die ("Failed to parse the file `" ++ fp ++ "'")
    Just x -> pure x
      

-- | Turn an XML element into a 'Tree', following the encoding described in
-- the docs of 'ElementNode'.
element2Tree
  :: (String -> [String])   -- ^ how to tokenize the text in 'CDataText' nodes
  -> Element                -- ^ input XML
  -> Tree ElementNode       -- ^ output tree form of the same XML
element2Tree tokenize = e2Tree
  where
  e2Tree :: Element -> Tree ElementNode
  e2Tree (Element n as sub _) = Node
    { rootLabel = QQName n
    , subForest = map (pure . QAttr) as ++ concatMap c2Tree sub }

  c2Tree :: Content -> [Tree ElementNode]
  c2Tree (Elem e)     = [ e2Tree e ]
  c2Tree (CRef ref)   = [ pure (QCRef ref) ]
  c2Tree (Text t@CData{ cdVerbatim = vb, cdData = str })
    | vb /= CDataText = [ pure (QCData t{ cdLine = Nothing }) ]
    | otherwise       = map (\t' -> pure (QCData (CData CDataText t' Nothing)))
                            (tokenize str)


-- * Tokenizers

-- | Tokenize as best as possible along Haskell tokens
hsTokenizer :: String -> [String]
hsTokenizer str | (l,r) <- span isSpace str, l /= "" = l : hsTokenizer r
                | (l,r) : _ <- lex str     , l /= "" = l : hsTokenizer r
hsTokenizer "" = []
hsTokenizer (c:r) = [c] : hsTokenizer r

-- | Don't tokenize
noTokenizer :: String -> [String]
noTokenizer = pure


-- * Diff the trees and convert them back into XML

-- | Produce an HTML span with the given class name
mkSpan :: String -> Content -> Element
mkSpan cls c = Element { elName = mkQName "span"
                       , elAttribs = [ Attr (mkQName "class") cls ]
                       , elContent = [ c ]
                       , elLine = Nothing
                       }

mkQName :: String -> QName
mkQName s = QName { qName = s, qURI = Nothing, qPrefix = Nothing }

-- | Keep only the newest part of the edit.
keepNew :: Edit a -> Maybe a
keepNew (Ins x) = Just x
keepNew (Del _) = Nothing
keepNew (Swp _ x) = Just x
keepNew (Cpy x) = Just x

-- | Convert an 'EditTree' into a plain document that uses the @added@/@removed@
-- classes to represent the edits.
editTree2Element :: EditTree ElementNode -> Element
editTree2Element = getElem
  where
  getElem :: EditTree ElementNode -> Element
  getElem (EditNode (QQName qname) forest) =
    Element { elName = qname
            , elAttribs = attrs 
            , elContent = contents
            , elLine = Nothing
            }
    where (attrs, contents) = getNewAttrs (map (fmap getContent) forest)


  -- Keep all the new attributes
  getNewAttrs :: [Edit (Either Content Attr)] -> ([Attr], [Content])
  getNewAttrs [] = mempty
  getNewAttrs (a : as) = getNewAttr a <> getNewAttrs as
    where
    getNewAttr (Cpy (Left cnt)) = ([], [cnt])
    getNewAttr (Cpy (Right attr)) = ([attr], [])
    getNewAttr (Ins (Left cnt)) = ([], [ Elem (mkSpan "added" cnt) ])
    getNewAttr (Ins (Right attr)) = ([attr], [])
    getNewAttr (Del (Left cnt)) = ([], [ Elem (mkSpan "removed" cnt) ])
    getNewAttr (Del (Right _)) = ([], [])
    getNewAttr (Swp x y) = getNewAttr (Del x) <> getNewAttr (Ins y)
 
  -- Produce HTML for an edit tree
  getContent :: EditTree ElementNode -> Either Content Attr
  getContent e@(EditNode (QQName _) _) = Left (Elem (getElem e))
  getContent   (EditNode (QCData cdata) subs)
    | null subs = Left (Text cdata)
    | otherwise = error "editTree2Element: unexpected children for QCData"
  getContent   (EditNode (QCRef ref) subs)
    | null subs = Left (CRef ref)
    | otherwise = error "editTree2Element: unexpected children for QCRef"
  getContent   (EditNode (QAttr attr) subs)
    | null subs = Right attr
    | otherwise = error "editTree2Element: unexpected children for QAttr"


-- * Rendering the output

-- | Add a @style@ tag at the end of the HTML containing the specified CSS.
addStyleNode
  :: Css     -- ^ CSS for what to apply to added elements
  -> Css     -- ^ CSS for what to apply to removed elements
  -> Element -> Maybe Element
addStyleNode addedCss removedCss doc
  | Element{ elName = QName{ qName = "html" }, elContent = contents } <- doc
  = Just doc{ elContent = contents ++ [ Elem styleStuff ] }
  | otherwise = Nothing
  where
  styleRaw = unwords [ ".added {", addedCss, "} .removed {", removedCss, "}" ]
  styleStuff = Element { elName = mkQName "style"
                       , elAttribs = [ Attr (mkQName "type") "text/css" ]
                       , elContent = [ Text CData{ cdVerbatim = CDataRaw
                                                 , cdData = styleRaw
                                                 , cdLine = Nothing
                                                 } ]
                       , elLine = Nothing
                       }

-- | Tags which can be written in the condensed @\<tag/\>@ format (as opposed to
-- those requiring a closing tag @\<tag></tag>@).
voidTags :: [String]
voidTags = words "area base br col hr img input link meta param" ++  -- HTML4
           words "command keygen source"                             -- HTML5


-- * Orphan instances

instance Functor Edit where
  fmap f (Ins x) = Ins (f x)
  fmap f (Del x) = Del (f x)
  fmap f (Cpy x) = Cpy (f x)
  fmap f (Swp x y) = Swp (f x) (f y)

-- | Unorthodox but convenient - doesn't check equality of locations
instance Eq CData where
  CData v1 s1 _ == CData v2 s2 _ = v1 == v2 && s1 == s2
