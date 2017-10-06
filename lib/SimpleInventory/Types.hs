{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SimpleInventory.Types (
    InventoryItem (..),
    Manufacturer (..),
    ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data InventoryItem = InventoryItem
    { inventoryItemId :: UUID -- ^ 
    , inventoryItemName :: Text -- ^ 
    , inventoryItemReleaseDate :: Text -- ^ 
    , inventoryItemManufacturer :: Manufacturer -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON InventoryItem where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "inventoryItem")
instance ToJSON InventoryItem where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "inventoryItem")

-- | 
data Manufacturer = Manufacturer
    { manufacturerName :: Text -- ^ 
    , manufacturerHomePage :: Text -- ^ 
    , manufacturerPhone :: Text -- ^ 
    } deriving (Show, Eq, Generic)

instance FromJSON Manufacturer where
  parseJSON  = genericParseJSON  (removeFieldLabelPrefix True "manufacturer")
instance ToJSON Manufacturer where
  toJSON     = genericToJSON     (removeFieldLabelPrefix False "manufacturer")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
    { fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars
    }
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars = [("@", "'At"), ("<=", "'Less_Than_Or_Equal_To"), ("[", "'Left_Square_Bracket"), ("\", "'Back_Slash"), ("]", "'Right_Square_Bracket"), ("^", "'Caret"), ("_", "'Underscore"), ("`", "'Backtick"), ("!", "'Exclamation"), (""", "'Double_Quote"), ("#", "'Hash"), ("$", "'Dollar"), ("%", "'Percent"), ("&", "'Ampersand"), ("'", "'Quote"), ("(", "'Left_Parenthesis"), (")", "'Right_Parenthesis"), ("*", "'Star"), ("+", "'Plus"), (",", "'Comma"), ("-", "'Dash"), (".", "'Period"), ("/", "'Slash"), (":", "'Colon"), ("{", "'Left_Curly_Bracket"), ("|", "'Pipe"), ("<", "'LessThan"), ("!=", "'Not_Equal"), ("=", "'Equal"), ("}", "'Right_Curly_Bracket"), (">", "'GreaterThan"), ("~", "'Tilde"), ("?", "'Question_Mark"), (">=", "'Greater_Than_Or_Equal_To")]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer = if forParsing then flip T.replace else T.replace


