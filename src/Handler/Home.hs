-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where
{-@ LIQUID "--ple" @-} 

-- import Import
import DBApi
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
-- import qualified Prelude as p
-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    currentUserId <- requireAuthId
    -- allTodoItemsTagged <- runDB $ getAllTodoItems' currentUserId
    allTodoItemsTagged <- runDB $  selectListTodoItem ((filterTodoItemUserId_EQ currentUserId)?:Empty)

    let allTodoItems = content allTodoItemsTagged
    -- allTodoItems <- runDB $ selectList [TodoItemUserId ==. currentUserId] [Asc TodoItemId]
    sharedTodoItemsUsersTagged <- runDB $ getAllsharedFrom' currentUserId
    let projectTest = ( 
            do
                sharedList <-  projectSharedItemShareFrom (content sharedTodoItemsUsersTagged)
                return sharedList
                      )
        
    -- let sharedTodoItemsUsers = content sharedTodoItemsUsersTagged
    -- sharedTodoItems = do
    let sharedFromList = content projectTest
    -- let sharedFromList = refEntityListtoList sharedTodoItemsUsers
    sharedTodoItemsTagged <- runDB $ getAllSharedItems' currentUserId sharedFromList
    let sharedTodoItems = content sharedTodoItemsTagged
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        let   (shareFormId, shareFormAreaId) =  shareIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        -- $(widgetFile "homepage")
        Prelude.undefined

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")


shareIds :: (Text, Text)
shareIds = ("js-shareForm", "js-shareTodoUname")

refEntityListtoList :: [RefinedSharedItem] -> [UserId]
refEntityListtoList [] = []
refEntityListtoList (x:xs) = sharedFronUserId:(refEntityListtoList xs)
        where 
            sharedFronUserId = shareFrom x

-- getAllSharedItems :: MonadIO m => [UserId] -> ReaderT SqlBackend m [Entity TodoItem]
-- getAllSharedItems userIds = selectList [TodoItemUserId <-. userIds] [Asc TodoItemId] 

{-@getAllSharedItems' ::forall<r::UserId->Bool>. currentUserId:UserId->[UserId] -> ReaderT SqlBackend _ (Tagged<{\v-> True}> [RefinedTodoItem<{\_->True}>])@-}
getAllSharedItems' :: MonadIO m => UserId-> [UserId] -> ReaderT SqlBackend m(Tagged [RefinedTodoItem])
getAllSharedItems' currentUserId userIds = selectListTodoItem (testFilter?:Empty)
            where
                {-@testFilter::RefinedFilter<{\_ ->False}, {\row v -> tuserId row == refUserUserId v || sharedItemProp  (tuserId row) (refUserUserId v)}> RefinedTodoItem @-}
                testFilter = (filterTodoItemUserId_IN userIds)

{-@getAllTodoItems' :: currentUserId:UserId -> ReaderT SqlBackend _ (Tagged<{\v-> refUserUserId v== currentUserId}> [RefinedTodoItem])@-}
getAllTodoItems' :: MonadIO m => UserId -> ReaderT SqlBackend m (Tagged [RefinedTodoItem])
getAllTodoItems' currentUserId = selectListTodoItem ((filterTodoItemUserId_EQ currentUserId)?:Empty)

-- getAllTodoItems :: MonadIO m => UserId -> ReaderT SqlBackend m  [Entity TodoItem]
-- getAllTodoItems currentUserId = selectList [TodoItemUserId ==. currentUserId] [Asc TodoItemId]


getAllsharedFrom' :: MonadIO m => UserId -> ReaderT SqlBackend m (Tagged [RefinedSharedItem])
getAllsharedFrom' currentUserId = selectListSharedItem ((filterSharedItemShareTo_EQ currentUserId)?:Empty)

-- getAllsharedFrom :: MonadIO m => UserId -> ReaderT SqlBackend m [Entity SharedItem]
-- getAllsharedFrom currentUserId = selectList [SharedItemShareTo ==. currentUserId] [Asc SharedItemId]