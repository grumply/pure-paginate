{-# LANGUAGE RecordWildCards #-}
module Pure.Paginate where

import Pure hiding (features,key)

import Control.Monad
import Data.Typeable

type Offset = Int
type Count  = Int

data Paginate key view response = Paginate
  { as       :: Features -> [View] -> View
  , features :: Features
  , loading  :: view
  , accept   :: response -> view
  , render   :: view -> View
  , loader   :: key -> Offset -> Count -> (response -> IO ()) -> IO ()
  , offset   :: Offset
  , count    :: Count
  , key      :: key
  , buttons  :: key -> Offset -> Count -> (Int -> IO ()) -> View
  }

instance (Eq key, Typeable key, Typeable view, Typeable response) => Pure (Paginate key view response) where
    view =
        ComponentIO $ \self ->
            let
                upd = modify_ self . const

                load off cnt update = do
                    Paginate {..} <- ask self
                    when update (upd $ \(ldd,off,cnt,v) -> (True,off + cnt,cnt,loading))
                    loader key off cnt $ \rsp -> upd $ \(ldd,off,cnt,_) -> (ldd,off,cnt,accept rsp)

            in
                def
                    { construct = do
                        Paginate {..} <- ask self
                        return (False,offset,count,loading)
                    , mounted = do
                        Paginate {..} <- ask self
                        load offset count True
                    , receive = \newprops oldstate -> do 
                        oldprops <- ask self
                        if offset oldprops /= offset newprops || count oldprops /= count newprops || key oldprops /= key newprops
                          then do
                            load (offset newprops) (count newprops) False
                            return (True,offset newprops + count newprops,count newprops,loading newprops)
                          else 
                            return oldstate
                    , Pure.render = \l (loaded,off,cnt,v) -> 
                        (as l) (features l)
                            [ Pure.Paginate.render l v
                            , buttons l (key l) off cnt (\n -> do
                                  (_,off,cnt,_) <- get self
                                  load (off + n * cnt) cnt True
                                )
                            ]
                    }
