{-# LANGUAGE OverloadedStrings #-}
module Handler.ShareTodoList where

import Import
import qualified Data.Text as T
import DBApi              
-- postJsonR :: Handler Text

postShareTodoListR :: Handler Value
postShareTodoListR = do
    -- let uN = do
    --     person <- requireJsonBody :: Handler User
    --     return $ (userName person) 
    
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    userShare <- (requireJsonBody)
    uName <- return $ (userUserName userShare) 
    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    -- updateWhere [PersonFirstName ==. "Michael"] [PersonAge *=. 2] -- it's been a long day

    currentUserId <- requireAuthId
    -- let todoItem' = todoItem { todoItemUserId = currentUserId }
    user <- runDB $ getUserItem uName
    toUserId <- case user of
            Nothing -> invalidArgs [T.append "User Not Found::" uName] 
            Just(Entity k v) -> return k
            
    let sharedItem' = SharedItem { sharedItemShareFrom = currentUserId, 
                                   sharedItemShareTo = toUserId }

    insertedSharedItem <- runDB $ insertEntity sharedItem'
    returnJson insertedSharedItem

    -- updaterUserItem <- runDB $ updateWhere [UserId ==. currentUserId] [sharedFromUsers =. uName] -- it's been a long day
    -- returnJson updaterUserItem
getUserItem :: MonadIO m => Text -> ReaderT SqlBackend m (Maybe (Entity User))
getUserItem uname = selectFirst [UserUserName ==. uname] []

    