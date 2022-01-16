{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List
import Data.Proxy
import Data.Maybe
import Data.String
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Vector (Vector)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified GitHub as GH
import Network.NetRc

main :: IO ()
main = do
  unwatchFromOwner "HubSpot"

getWatching :: GH.Name GH.Owner -> GH.Request k (Vector GH.Repo)
getWatching user = GH.reposWatchedByR user 10000

getAuth :: IO (GH.Auth)
getAuth = do
  netrc <- readUserNetRc
  case netrc of
    Just (Right nrc) -> nrc
        & nrHosts
        & find (("api.github.com" `BS.isInfixOf`) . nrhName) 
        & fromJust
        & (nrhLogin &&& nrhPassword)
        & uncurry GH.BasicAuth
        & pure
    _ -> error "Couldn't find .netrc"

toOwner :: Text -> GH.Name GH.Owner
toOwner = GH.mkName (Proxy :: Proxy GH.Owner)

-- I was somehow subscribed to a bunch of `HubSpot` repos
-- and it was getting hella annoying...
unwatchFromOwner :: Text -> IO ()
unwatchFromOwner ownerT = do
  let owner = toOwner ownerT
  auth <- getAuth
  Right user <- GH.github auth GH.userInfoCurrentR
    <&> fmap GH.userLogin
  Right watching <- GH.github auth (getWatching $ GH.fromUserName user)
  let hs = watching
           & V.filter ((== owner) . GH.simpleOwnerLogin . GH.repoOwner)
           <&> GH.repoName
  V.forM_ hs $ \repo -> do
    print repo
    GH.github auth GH.unwatchRepoR owner repo
