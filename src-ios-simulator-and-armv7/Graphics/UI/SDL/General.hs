{-# LINE 1 "Graphics/UI/SDL/General.hsc" #-}

{-# LINE 2 "Graphics/UI/SDL/General.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.General
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.General
    ( -- * Simple Message Box
      showSimpleMessageBox
    , MessageBoxType(..)

       -- * Utilities
    , failWithError
    , unwrapBool
    , unwrapMaybe
    , unwrapInt
    , handleError
    , handleErrorI
    ) where

import Prelude hiding (init)
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C (CString, CUInt(..), withCString)

import Graphics.UI.SDL.Error
import Graphics.UI.SDL.Types (WindowStruct, Window)


unwrapMaybe :: String -> IO (Maybe a) -> IO a
unwrapMaybe errMsg action
    = do val <- action
         case val of
           Just a -> return a
           Nothing -> failWithError errMsg

unwrapInt :: (Int -> Bool) -> String -> IO Int -> IO Int
unwrapInt fn errMsg action
    = do val <- action
         if fn val
            then return val
            else failWithError errMsg

unwrapBool :: String -> IO Bool -> IO ()
unwrapBool errMsg action
    = do val <- action
         case val of
           True -> return ()
           False -> failWithError errMsg



failWithError :: String -> IO a
failWithError msg
    = do err <- fmap (fromMaybe "No SDL error") getError
         ioError $ userError $ msg ++ "\nSDL message: " ++ err

foreign import ccall unsafe "SDL_ShowSimpleMessageBox" sdlShowSimpleMessageBox :: CUInt -> CString -> CString -> Ptr WindowStruct -> IO ()

data MessageBoxType = Error | Warning | Information

-- | Show a message box.
showSimpleMessageBox :: MessageBoxType -> String -> String -> Maybe Window -> IO ()
showSimpleMessageBox flag title msg parent =
  withCString title $ \ctitle ->
  withCString msg $ \cmsg ->
  let go parent' = sdlShowSimpleMessageBox cflag ctitle cmsg parent' in fromMaybe (go nullPtr) (fmap (`withForeignPtr` go) parent)
  where
    cflag = case flag of
      Error -> 16
{-# LINE 77 "Graphics/UI/SDL/General.hsc" #-}
      Warning -> 32
{-# LINE 78 "Graphics/UI/SDL/General.hsc" #-}
      Information -> 64
{-# LINE 79 "Graphics/UI/SDL/General.hsc" #-}

handleError :: String -> Ptr a -> (Ptr a -> IO b) -> IO b
handleError fname ptr fn
  | ptr == nullPtr = (\err -> error $ fname ++ ": " ++ show err) =<< getError
  | otherwise      = fn ptr
{-# INLINE handleError #-}

handleErrorI :: (Num a, Ord a) => String -> a -> (a -> IO b) -> IO b
handleErrorI fname i fn
  | i < 0     = fn i
  | otherwise =  (\err -> error $ fname ++ ": " ++ show err) =<< getError
{-# INLINE handleErrorI #-}


