{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Qi where

import qualified Language.C.Inline.Cpp as CPP
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Data.ByteString.Char8 as BS
--import Language.Haskell.TH

import Foreign hiding (new)
--import GHC.Real
--import Foreign.C.String
--import Foreign.C.Types
import qualified Data.Map as Map
import Data.Monoid
import Data.String
import Control.Monad

data ApplicationSession
data SessionPtr
data Memory
data AnyValue
data StrVector
data Session = Session {
   app :: Ptr ApplicationSession
 , session :: Ptr SessionPtr
-- , memory :: Ptr Memory
 }

C.context $ CPP.cppCtx <> C.funCtx <> C.bsCtx <> mempty {
  C.ctxTypesTable = Map.fromList [
     (C.TypeName "ApplicationSession", [t|ApplicationSession|])
  ,  (C.TypeName "SessionPtr", [t|SessionPtr|])
  ,  (C.TypeName "AnyValue", [t|AnyValue|])
  ,  (C.TypeName "StrVector", [t|StrVector|])
  ]
}


C.include "<iostream>"
C.include "<string>"
C.include "<cstring>"
C.include "<vector>"
C.include "<qi/applicationsession.hpp>"
CPP.using "namespace qi"
CPP.using "namespace std"
C.verbatim "typedef vector<string> StrVector;"

--createStlVector :: IO (Ptr StlVector)

createSession :: String -> IO Session
createSession url = do
  let burl = BS.pack url
  app <- [C.block|ApplicationSession *{
    int argc = 3;
    char argv0[]="program";
    char argv1[]="--qi-url";
    char* argv[] = {argv0,argv1,$bs-ptr:burl};
    char** argvp = argv;
    ApplicationSession* app = new ApplicationSession(argc, argvp);
    app->start();
    return app;
  }|]
  session <- [C.block|SessionPtr *{
    SessionPtr* session = new SessionPtr($(ApplicationSession* app)->session());
    return session;
  }|]
  return $ Session{..}


destroySession :: Session -> IO ()
destroySession Session{..} = do
  [C.block|void{
    delete $(SessionPtr* session);
    delete $(ApplicationSession* app);
  }|]

callServiceStr :: Session -> String -> String -> String -> IO ()
callServiceStr Session{..} service func str = do
  let bservice=BS.pack service
      bfunc=BS.pack func
      bstr=BS.pack str
  [C.block|void{
    AnyObject tts = (*$(SessionPtr* session))->service($bs-ptr:bservice);
    tts.call<void>($bs-ptr:bfunc, $bs-ptr:bstr);
  }|]

callServiceStrVector :: Session -> String -> String -> [String] -> IO ()
callServiceStrVector Session{..} service func strings = do
  vec <- new
  forM_ strings $ \str -> do
    pushStr vec (fromString str)

  let bservice=BS.pack service
      bfunc=BS.pack func
  [C.block|void{
    AnyObject tts = (*$(SessionPtr* session))->service($bs-ptr:bservice);
    tts.call<void>($bs-ptr:bfunc, *$(StrVector* vec));
  }|]
  delete vec


class Object a where
  new :: IO (Ptr a)
  delete :: Ptr a -> IO ()

class Stack a where
  push :: a -> Ptr a -> IO ()
  pop :: Ptr a -> IO a

instance Object StrVector where
  new = [C.exp|StrVector*{ new vector<string>() }|]
  delete ptr = [C.exp|void { delete $(StrVector* ptr) }|]

pushStr :: Ptr StrVector -> BS.ByteString -> IO ()
pushStr vec str = [C.exp|void{ $(StrVector* vec)->push_back($bs-ptr:str) }|]


setEventCallback :: Session -> String -> (Ptr AnyValue -> IO ()) -> IO ()
setEventCallback Session{..} str callback = do
  let bstr = BS.pack str
  [C.block|void {
    struct InnerClass { 
      void static call(void (*callback)(AnyValue*),AnyValue value){
        callback(&value);  
      }
    };
    AnyObject almemory = (*$(SessionPtr* session)) ->service("ALMemory");
    AnyObject subscriber = almemory.call<AnyObject>("subscriber", $bs-ptr:bstr);
    subscriber.connect("haskell-qi",boost::function<void(AnyValue)>(boost::bind(&(InnerClass::call),$fun:(void(*callback)(AnyValue*)),_1)));
    return;
  }|]
  return ()

say :: Session -> String -> IO ()
say ses str = callServiceStr ses "ALTextToSpeech" "say" str

speachReco :: Session -> [String] -> (String -> IO ()) -> IO ()
speachReco ses strings callback = do
  callServiceStrVector ses "ALSpeechRecognition" "setVocabulary" strings
  setEventCallback ses "WordRecognized" $ \anyvalue -> do
    str <- [C.exp|char*{ strdup($(AnyValue* anyvalue)->toString().c_str()) }|]
    bstr <- BS.packCString str
    [C.exp|void { free($(char* str)) }|]
    callback (BS.unpack bstr)

