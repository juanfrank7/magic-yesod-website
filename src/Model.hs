{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           ClassyPrelude.Yesod
import           Database.Persist.Quasi

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- { "id": 1, "name": "Grand Mage", "mana": 3, "ctype": "Monster", "descr": "a great mage", "atk": 6, "def": 3}
instance ToJSON (Entity Card) where
    toJSON (Entity cardId card) = object
        [ "id"    .= (String $ toPathPiece cardId)
        , "name"  .= cardName card
        , "mana"  .= cardMana card
        , "ctype" .= cardCtype card
        , "descr" .= cardDescr card
        , "atk"   .= cardAtk card
        , "def"   .= cardDef card
        ]

instance FromJSON Card where
    parseJSON (Object card) = Card
        <$> card .: "name"
        <*> card .: "mana"
        <*> card .: "ctype"
        <*> card .: "descr"
        <*> card .: "atk"
        <*> card .: "def"

    parseJSON _ = mzero

data Privileges = 
  CreateCard 
  | SeeCard
  deriving (Show,Read,Eq)

derivePersistField "Privileges"
