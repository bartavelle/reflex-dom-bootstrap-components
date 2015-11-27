{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Reflex.Dom.Bootstrap where

import GHCJS.Foreign ()
import GHCJS.Types
import Data.JSString()
import qualified GHCJS.DOM.JSFFI.Generated.Enums as J
import qualified GHCJS.DOM.JSFFI.Generated.XMLHttpRequest as X
import Reflex.Dom

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson
import Control.Monad
import Data.Maybe
import Control.Lens

staticDropdown :: (MonadWidget t m, Ord k, Show k, Read k) => k -> M.Map k String -> m (Dynamic t k)
staticDropdown f entries = _dropdown_value <$> dropdown f (constDyn entries) (DropdownConfig never (constDyn attrs))
    where
        attrs = M.singleton "class" "form-control"

formSelectInput :: (MonadWidget t m, Ord k, Show k, Read k) => String -> String -> k -> M.Map k String -> m (Dynamic t k)
formSelectInput desc fieldid defval mp = formGroup desc fieldid (const True) (staticDropdown defval mp) >>= mapDyn (fromMaybe defval)

enumDropdown :: (MonadWidget t m, Ord k, Show k, Read k, Enum k, Bounded k) => String -> String -> k -> m (Dynamic t k)
enumDropdown desc fieldid defval = formSelectInput desc fieldid defval (M.fromList [ (k,show k) | k <- [minBound .. maxBound]])

horForm :: MonadWidget t m => m a -> m a
horForm = elAttr "form" (M.singleton "form" "form-horizontal")

formGroup :: MonadWidget t m => String -> String -> (a -> Bool) -> m (Dynamic t a) -> m (Dynamic t (Maybe a))
formGroup description fieldid check act = do
    let valid = M.singleton "class" "form-group has-success"
        invalid = M.singleton "class" "form-group has-error"
    rec (_,o) <- elDynAttr' "div" attrs $ do
            elAttr "label" (M.fromList [("for", fieldid), ("class", "col-md-2 control-label")]) (text description)
            divClass "col-md-10" act
        attrs <- mapDyn (\x -> if check x then valid else invalid) o
    mapDyn (\x -> if check x then Just x else Nothing) o

formTextInput :: MonadWidget t m => String -> String -> Maybe String -> (String -> Bool) -> m (Dynamic t (Maybe String))
formTextInput description fieldid initval check = formGroup description fieldid check $ _textInput_value <$>
    textInput (def & textInputConfig_initialValue .~ fromMaybe "" initval
                   & textInputConfig_attributes .~ constDyn (M.fromList [ ("class", "form-control"), ("id", fieldid) ])
              )

formTextArea :: MonadWidget t m => String -> String -> Maybe String -> (String -> Bool) -> m (Dynamic t (Maybe String))
formTextArea description fieldid initval check = formGroup description fieldid check $ _textArea_value <$>
    textArea (def & attributes .~ constDyn (M.singleton "class" "form-control")
                  & textAreaConfig_initialValue .~ fromMaybe "" initval
             )

buttonClass :: MonadWidget t m => String -> String -> m (Event t ())
buttonClass s cl = do
    (e, _) <- elAttr' "button" (M.fromList [("type","button"),("class",cl)]) $ text s
    return $ domEvent Click e

toggleButtonBool :: MonadWidget t m => (String, String) -> (String, String) -> Bool -> m (Dynamic t Bool)
toggleButtonBool (offtext, offclass) (ontext, onclass) startValue = do
    let onattr  = M.fromList [("type","button"),("class", "btn " ++ onclass)]
        offattr = M.fromList [("type","button"),("class", "btn " ++ offclass)]
    rec (e, _) <- elDynAttr' "button" dynattrs $ dynText buttoncaption
        isActivated <- toggle startValue (domEvent Click e)
        dynattrs <- mapDyn (\x -> if x then onattr else offattr) isActivated
        buttoncaption <- mapDyn (\x -> if x then ontext else offtext) isActivated
    return isActivated

bsTextInput :: MonadWidget t m => m (Dynamic t String)
bsTextInput = _textInput_value <$> textInput (def & textInputConfig_attributes .~ constDyn (M.singleton "class" "form-control"))

comb :: MonadWidget t m => Dynamic t a -> Dynamic t (a -> b) -> m (Dynamic t b)
comb e f = combineDyn ($) f e

combf :: (Reflex t, MonadHold t m) => (b -> x) -> Dynamic t b -> Dynamic t (x -> c) -> m (Dynamic t c)
combf trans dn t = combineDyn (\f a -> f (trans a)) t dn

bsAlert :: MonadWidget t m => String -> m ()
bsAlert = elAttr "div" (M.fromList [("class", "alert alert-danger"), ("role", "alert")]) . text

bsDynAlert :: MonadWidget t m => Dynamic t (Maybe String) -> m ()
bsDynAlert = mapDyn (fromMaybe "") >=> dynText

row :: MonadWidget t m => m a -> m a
row = divClass "row"

containerFluid :: MonadWidget t m => m a -> m a
containerFluid = divClass "container-fluid"

table ::MonadWidget t m => m a -> m a
table = elClass "table" "table table-bordered"

addListWorkflow :: MonadWidget t m
                => a -- ^ default value
                -> M.Map Int a -- ^ Initial value
                -> (forall b. m b -> m b) -- ^ header for the list
                -> (a -> m (Event t a)) -- ^ handle a single list item
                -> m (Event t ()) -- ^ Add button
                -> Event t x -- ^ Refresh event
                -> Workflow t m (M.Map Int a)
addListWorkflow defValue initList hdr handlemodification addbutton refresh = Workflow $ do
    modificationEvents <- mergeMap <$> hdr (traverse handlemodification initList)
    listD <- foldDyn M.union initList modificationEvents
    addEvent <- addbutton
    let refreshListE = tagDyn listD refresh
        addListE = attachDynWith (\d _ -> addElement d) listD addEvent
        addElement mp = let mx = if M.null mp then 0 else fst (M.findMax mp) + 1
                        in  mp & at mx ?~ defValue
        changeEvents = leftmost [refreshListE, addListE]
    return (initList, fmap (\mp -> addListWorkflow defValue mp hdr handlemodification addbutton refresh) changeEvents)

addList :: MonadWidget t m
        => a -- ^ default value
        -> [a] -- ^ Initial value
        -> (forall b. m b -> m b) -- ^ header for the list
        -> (a -> m (Event t a)) -- ^ handle a single list item
        -> m (Event t ()) -- ^ Add button
        -> Event t x -- ^ Refresh event
        -> m (Dynamic t [a])
addList defValue initList hdr handlemodification addbutton refresh = workflow (addListWorkflow defValue initmap hdr handlemodification addbutton refresh) >>= mapDyn M.elems
    where
        initmap = M.fromList $ zip [0..] initList

--- queries

data QError = AesonError String
            | HTTPError
            deriving (Show, Eq)


jsonQuery :: (ToJSON a, FromJSON b, MonadWidget t m)
          => String -- method
          -> String -- request path
          -> Event t (Maybe a) -- body
          -> m (Event t (Either QError b))
jsonQuery !method !pth e = fmap (edecode . _xhrResponse_body) <$> performRequestAsync (mkreq <$> e)
    where
        baserequest :: XhrRequest
        baserequest = xhrRequest method pth baseconfig
        baseconfig :: XhrRequestConfig
        baseconfig = def { _xhrRequestConfig_headers = M.singleton "Content-Type" "application/json"
                         , _xhrRequestConfig_responseType = Just J.XMLHttpRequestResponseTypeText
                         }
        edecode :: FromJSON b => Maybe T.Text -> Either QError b
        edecode Nothing = Left HTTPError
        edecode (Just t) = case eitherDecodeStrict' (T.encodeUtf8 t) of
                               Right x -> Right x
                               Left rr -> Left (AesonError rr)
        mkreq :: ToJSON a => a -> XhrRequest
        mkreq b = let !dt = Just $! BS8.unpack (BL.toStrict $ encode b)
                  in  baserequest { _xhrRequest_config = baseconfig { _xhrRequestConfig_sendData = dt }}

getUrlSync :: FromJSON a => JSString -> IO (Either String a)
getUrlSync url = do
    r <- X.newXMLHttpRequest
    X.open r ("GET" :: JSString) url False ("" :: JSString) ("" :: JSString)
    X.send r
    o <- X.getResponseText r
    case o of
        Nothing -> return (Left "loading failed")
        Just cnt -> return $ eitherDecode $ BL.fromStrict $ T.encodeUtf8 cnt

