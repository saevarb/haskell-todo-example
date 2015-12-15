{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.Quasi
import           Database.Persist.TH
import qualified Web.Scotty as S
import Network.HTTP.Types
import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TodoItem
   priority Int
   text     Text
   ItemPriority priority
     deriving Show
|]

deriveJSON defaultOptions{fieldLabelModifier = map toLower . drop 8} ''TodoItem

runDb query = runSqlite "dev.sqlite3" query

main :: IO ()
main = do
    runDb $ runMigration migrateAll
    S.scotty 3000 $ do
        S.get "/" $ S.html "Hello World"
        S.get "/get/:id" $ do
            i <- S.param "id"
            r <- runDb $ selectList [TodoItemPriority ==. i] []
            case r of
              [] -> S.json Null
              _ -> S.json . entityVal $ head r
        S.get "/all" $ do
            items <- runDb $ selectList [] [Asc TodoItemPriority]
            S.json $ map entityVal items
        S.post "/add" $ do
            v <- S.jsonData
            runDb $ insert (v :: TodoItem)
            S.status created201
