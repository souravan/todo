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
    userTagged <- runDB $ getUserItem' uName
    let user  = content userTagged
    toUserId <- case user of
            [] -> invalidArgs [T.append "User Not Found::" uName] 
            [x] -> return $ refUserUserId x
            (x:xs) -> invalidArgs [T.append "Multiple users with same name:" uName] 
            
    let sharedItem' = SharedItem { sharedItemShareFrom = currentUserId, 
                                   sharedItemShareTo = toUserId }

    insertedSharedItem <- runDB $ insertEntity sharedItem'
    returnJson insertedSharedItem

    -- updaterUserItem <- runDB $ updateWhere [UserId ==. currentUserId] [sharedFromUsers =. uName] -- it's been a long day
    -- returnJson updaterUserItem
getUserItem' :: MonadIO m => Text -> ReaderT SqlBackend m (Tagged [RefinedUser])
getUserItem' uname = selectListUser ((filterUserName uname)?:Empty)

-- getUserItem :: MonadIO m => Text -> ReaderT SqlBackend m [Entity User]
-- getUserItem uname = selectList [UserUserName ==. uname] []

    