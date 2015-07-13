module Data.OrgMode.Doc (Node(..), Prefix(..), Drawer(..),
                         OrgFileProperty(..), Babel(..), Table(..),
                         OrgDoc(..), NodeChild(..), updateNode, trim,
                         makeNodeLine) where
import Data.List (intercalate)
import Data.OrgMode.Text

--
-- * Data Decls
--

-- |A keyword at the front of a node heading, like TODO or DONE.
data Prefix = Prefix String deriving (Eq, Show)

data Drawer = Drawer
              { drName :: String -- ^ :PROPERTIES: or another name.
              , drProperties :: [(String, String)] -- ^ Key-value pairs.
              , drLines :: [TextLine] -- ^ Literal text of the entire drawer.
              } deriving (Eq, Show)

-- |Currently underimplemented: stores the lines of the babel environment.
data Babel = Babel [TextLine] deriving (Eq, Show)

-- |Currently underimplemented: stores the lines of the table.
data Table = Table [TextLine] deriving (Eq, Show)

-- |Children of top-level Org Nodes.
data NodeChild = ChildText TextLine -- ^ Regular text.
               | ChildDrawer Drawer
               | ChildNode Node -- ^ outline nodes of higher depth.
               | ChildBabel Babel
               | ChildTable Table
                 deriving (Eq, Show)

-- |An outline node in org-mode.  For a node @** TODO Foo a bar :FOOBAR:@
--
--  * @nDepth@ is 2
--  * @nPrefix@ is @Just "TODO"@
--  * @nTags@ is @["FOOBAR"]@
--  * @nTopic@ is @"Foo a bar"@, note the stripped whitespace on the front and back.
--  * @nChildren@ aren't determined by this line, but by the lines after.
data Node = Node
            { nDepth :: Int -- ^ Number of stars on the left.
            , nPrefix :: Maybe Prefix -- ^ E.g., TODO or DONE.
            , nTags :: [String] -- ^:TAGS:AT:END:
            , nChildren :: [NodeChild] -- ^ Everything hierarchially under the node.
            , nTopic :: String -- ^ Text of he header line, minus prefix and tags.
            , nLine :: TextLine -- ^ Literal text of the node header.
            } deriving (Eq, Show)

-- |Properties within the org file.  Examples include @#+TITLE:@
data OrgFileProperty = OrgFileProperty { fpName :: String
                                       , fpValue :: String
                                       } deriving (Eq, Show)
-- |Full contents of an org file.
data OrgDoc = OrgDoc
              { odNodes :: [Node]
              , odProperties :: [OrgFileProperty]
              , odLines :: [TextLine]
              } deriving (Eq, Show)
--
-- * Instance Decls
--

instance TextLineSource NodeChild where
  getTextLines (ChildText l) = [l]
  getTextLines (ChildDrawer d) = drLines d
  getTextLines (ChildNode n) = getTextLines n
  getTextLines (ChildBabel (Babel lines)) = lines
  getTextLines (ChildTable (Table lines)) = lines

instance TextLineSource Node where
  getTextLines node =
    (nLine node) : (concatMap getTextLines $ nChildren node)

instance TextLineSource OrgFileProperty where
  getTextLines prop =
    [TextLine 0 ("#+" ++ (fpName prop) ++ ": " ++ (fpValue prop)) Nothing]

-- ** Utilities
trim xs =
  let rstrip xs = reverse $ lstrip $ reverse xs
      lstrip = dropWhile (== ' ')
  in lstrip $ rstrip xs

makeNodeLine :: Node -> String
makeNodeLine (Node depth prefix tags children topic _) =
  stars ++ " " ++ pfx ++ topic ++ " " ++ tgs
  where
    stars = take depth $ repeat '*'
    pfx = case prefix of
      Just (Prefix s) -> (s ++ " ")
      Nothing -> ""
    tgs = if length tags > 0
          then ":" ++ (intercalate ":" tags) ++ ":"
          else ""

updateNode :: (Node -> Maybe Node) -> Node -> Node
updateNode fn root =
  let top = case (fn root) of
        Nothing -> root
        Just t -> t
      all_children = nChildren top
      updateChild c =
        case c of
          (ChildNode n) -> ChildNode $ updateNode fn n
          otherwise -> c
  in top { nChildren = map updateChild $ all_children }
