{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Card where

import           Control.Applicative   ((<$>), (<*>))
import           Import
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)


cardForm :: Maybe Card -> AForm Handler Card
cardForm mcard = Card
    <$> areq textField "Name" (cardName <$> mcard)
    <*> areq intField  "Mana" (cardMana <$> mcard)
    <*> areq textField "Ctype"(cardCtype <$> mcard)
    <*> areq textField "Descr"(cardDescr <$> mcard)
    <*> aopt intField  "Atk"  (cardAtk <$> mcard)
    <*> aopt intField  "Def"  (cardDef <$> mcard)


getCardNewR ::  Handler Html
getCardNewR = do
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm Nothing
               defaultLayout $ do
                    let actionR = CardNewR
                    $(widgetFile "Card/CardCreate")

postCardNewR :: Handler Html
postCardNewR = do
                ((result, _ ), _ ) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm  Nothing
                case result of
                     FormSuccess card -> do
                                 _ <- runDB $ insert card
                                 redirect CardListR
                     _ -> defaultLayout $ do
                        [whamlet|
                            <span>Hubo un error creando la carta, intenta de nuevo
                        |]
                        redirect CardNewR


getCardListR ::  Handler Html
getCardListR = do
        cards <- runDB $ selectList [] []
        ( _ , _ ) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ cardForm Nothing
        defaultLayout $ do
            $(widgetFile "Card/CardList")

