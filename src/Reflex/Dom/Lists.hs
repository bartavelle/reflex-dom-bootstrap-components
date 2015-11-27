{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Dom.Lists where

import GHCJS.Foreign ()
import Data.JSString()
import Reflex.Dom

import qualified Data.Map.Strict as M

import Control.Lens

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

data DefaultFilter a = FilterFunc (a -> Bool)
                     | RefreshFilter

instance Monoid (DefaultFilter a) where
    mempty = FilterFunc (const True)
    mappend _ RefreshFilter = mempty
    mappend RefreshFilter a = a
    mappend (FilterFunc a) (FilterFunc b) = FilterFunc (\x -> a x && b x)

filterList :: (MonadWidget t m, Monoid f)
           => [a] -- ^ Initial list
           -> f -- ^ Initial filter
           -> ([a] -> f -> m (Event t f)) -- ^ list elements display, including a filtering signal
           -> (f -> a -> Bool) -- ^ filtering function
           -> m (Event t [a]) -- ^ the list in its current form
filterList initlist initFilter dispElems filterElem = do
    rec let initMap = M.fromList $ zip ([0..] :: [Int]) initlist
            filtermap f = M.filter (filterElem f) initMap
        filterD <- foldDyn (flip mappend) initFilter filterE
        elemMapD <- mapDyn filtermap filterD
        foo <- forDyn filterD $ \f -> dispElems (M.elems (filtermap f)) f
        filterE <- dyn foo >>= switchPromptly never
    return (M.elems <$> updated elemMapD)
