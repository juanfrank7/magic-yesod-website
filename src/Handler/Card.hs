{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Card where

import           Control.Applicative   ((<$>), (<*>))
import           Data.Text             (Text)
import           Import
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)


cardForm :: Maybe Card -> AForm Handler Card
cardForm mcard = Card
    <$> areq textField     "Name"   (cardName <$> mcard)
    <*> areq cardManaField "Mana"   (cardMana <$> mcard)
    <*> areq textField     "Type"   (cardCtype <$> mcard)
    <*> areq textField     "Text"   (cardDescr <$> mcard)
    <*> aopt cardAtkField  "Attack" (cardAtk <$> mcard)
    <*> aopt intField      "Defense"(cardDef <$> mcard)
  where
    mgMsg :: Text
    mgMsg = "No puede tener valor negativo"

    cardManaField = checkBool (> 0) mgMsg intField
    cardAtkField  = checkBool (> 0) mgMsg intField

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

getCardsJsonR :: Handler Value
getCardsJsonR = do 
        cards <- runDB $ selectList [] [] :: Handler [Entity Card]

        return $ object ["cards" .= cards]

postCardsJsonR :: Handler Value
postCardsJsonR = do
        card <- requireJsonBody :: Handler Card
        _    <- runDB (insert card)

        sendResponseStatus status201 ("CREATED" :: Text) 

getCardJsonR :: CardId -> Handler Value
getCardJsonR cardId = do
        card <- runDB $ get404 cardId

        return $ object ["card" .= (Entity cardId card)]

putCardJsonR :: CardId -> Handler Value
putCardJsonR cardId = do 
        card <- requireJsonBody :: Handler Card
        runDB $ replace cardId card

        sendResponseStatus status200 ("UPDATED" :: Text)

deleteCardJsonR :: CardId -> Handler Value
deleteCardJsonR cardId = do 
        runDB $ delete cardId

        sendResponseStatus status200 ("DELETED" :: Text)
