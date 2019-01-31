module Handler.TodoItem where

import Import

postTodoItemR :: Handler Value
postTodoItemR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    todoItem <- (requireJsonBody :: Handler TodoItem)

    -- The YesodAuth instance in Foundation.hs defines the UserId to be the type used for authentication.
    maybeCurrentUserId <- maybeAuthId
    let todoItem' = todoItem { todoItemUserId = maybeCurrentUserId }

    insertedTodoItem <- runDB $ insertEntity todoItem'
    returnJson insertedTodoItem
