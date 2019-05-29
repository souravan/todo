{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-@ LIQUID "--no-adt" 	                           @-}
-- {-@ LIQUID "--exact-data-con"                      @-}
-- {-@ LIQUID "--higherorder"                         @-}
-- {-@ LIQUID "--no-termination"                      @-}
-- {-@ LIQUID "--ple" @-} 
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
-- import qualified Prelude as P
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- {-@instance PersistEntity User where
--         persistUniqueToFieldNames :: {v:_ | False} -> _;
--         persistUniqueToValues :: {v:_ | False} -> _ 
-- @-}
{-@instance PersistEntity TodoItem where
    persistUniqueToFieldNames :: {v:_ | False} -> _;
    persistUniqueToValues :: {v:_ | False} -> _ 
@-}
{-@instance PersistEntity SharedItem where
    persistUniqueToFieldNames :: {v:_ | False} -> _;
    persistUniqueToValues :: {v:_ | False} -> _ 
@-}