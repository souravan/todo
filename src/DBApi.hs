{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-@ LIQUID "--no-adt" 	                           @-}
{-@ LIQUID "--exact-data-con"                      @-}
{-@ LIQUID "--higherorder"                         @-}
{-@ LIQUID "--no-termination"                      @-}
{-@ LIQUID "--ple" @-} 

module DBApi where
    import           Prelude hiding (filter)
    import           Control.Monad.IO.Class  (liftIO, MonadIO)
    -- import           Control.Monad.Trans.Reader
    -- import           Database.Persist
    import           Model
    -- import Import
    import Foundation            as Import
    import Import.NoFoundation   as Import
    -- import           Database.Persist


    -- test:: Int
    -- test = 1

    selectListUser :: MonadIO m => FilterList RefinedUser -> ReaderT SqlBackend m (Tagged [RefinedUser])
    selectListUser filterList =fmap Tagged (fmap (fmap map_entity_toRefUser) reader_data)
        where
            reader_data = selectList filters []
            filters  = ref_to_pers_filters_user filterList
            map_entity_toRefUser x = RefinedUser{userName = userUserName $ entityVal x, userId = entityKey x}

    ref_to_pers_filters_user::FilterList RefinedUser  -> [Filter Model.User]
    ref_to_pers_filters_user Empty = []
    ref_to_pers_filters_user (Cons f b) = (toPersistentFilterUser f):(ref_to_pers_filters_user b)
    

    data RefinedUser =RefinedUser {userName::Text ,userId:: UserId}
    data FilterList a = Empty | Cons (RefinedFilter a) (FilterList a)
    -- data RefinedFilter record = RefinedFilter (Filter record)

    data RefinedFilter record = forall typ.PersistField typ => RefinedFilter
        { refinedFilterField  :: RefinedEntityField record typ
        , refinedFilterValue  :: typ
        , refinedFilterFilter :: RefinedPersistFilter
        } 
    
    toPersistentFilterUser :: RefinedFilter RefinedUser -> Filter User
    toPersistentFilterUser (RefinedFilter f value filter) =
        case filter of
            EQUAL -> field ==. value
            NE -> field !=. value
            LE -> field <=. value
            LTP -> field <. value
            GE -> field >=. value
            GTP -> field >. value
            where
                field = (refentity_entity_user f)
    


    refentity_entity_user :: RefinedEntityField RefinedUser b ->  EntityField User b
    refentity_entity_user RefUserName = UserUserName


    
    --fortodoitem
    selectListTodoItem :: MonadIO m => FilterList RefinedTodoItem -> ReaderT SqlBackend m (Tagged [RefinedTodoItem])
    selectListTodoItem filterList =fmap Tagged (fmap (fmap map_entity_toRefTodoItem) reader_data)
        where
            reader_data = selectList filters []
            filters  = ref_to_pers_filters_TodoItem filterList
            map_entity_toRefTodoItem x = RefinedTodoItem{task = todoItemTask $ entityVal x, todoItemId = entityKey x, done = todoItemDone $entityVal x,
                                                            userId = todoItemUserId $ entityVal x}

    ref_to_pers_filters_TodoItem::FilterList RefinedTodoItem  -> [Filter Model.User]
    ref_to_pers_filters_TodoItem Empty = []
    ref_to_pers_filters_TodoItem (Cons f b) = (toPersistentFilterTodoItem f):(ref_to_pers_filters_TodoItem b)
    

    data RefinedTodoItem =RefinedTodoItem {task::Text, todoItemId::TodoItemId, done::Bool,userId::UserId}
    -- data FilterList a = Empty | Cons (RefinedFilter a) (FilterList a)
    -- -- data RefinedFilter record = RefinedFilter (Filter record)

    -- data RefinedFilter record = forall typ.PersistField typ => RefinedFilter
    --     { refinedFilterField  :: RefinedEntityField record typ
    --     , refinedFilterValue  :: typ
    --     , refinedFilterFilter :: RefinedPersistFilter
    --     } 
    
    toPersistentFilterTodoItem :: RefinedFilter RefinedTodoItem -> Filter TodoItem
    toPersistentFilterTodoItem (RefinedFilter f value filter) =
        case filter of
            EQUAL -> field ==. value
            NE -> field !=. value
            LE -> field <=. value
            LTP -> field <. value
            GE -> field >=. value
            GTP -> field >. value
            where
                field = (refentity_entity_TodoItem  f)
    
    -- data RefinedEntityField a b where
    --     RefUserName :: RefinedEntityField RefinedTodoItem Text
    
    -- data RefinedPersistFilter = EQUAL | NE | LE | LTP | GE | GTP

    refentity_entity_TodoItem  :: RefinedEntityField RefinedTodoItem b ->  EntityField  b
    refentity_entity_TodoItem  RefTodoTask = todoItemTask
    refentity_entity_TodoItem  RefTodoDone = todoItemDone
    refentity_entity_TodoItem  RefTodoUserId = todoItemUserId

    --for SharedItem
    selectListSharedItem :: MonadIO m => FilterList RefinedSharedItem -> ReaderT SqlBackend m (Tagged [RefinedSharedItem])
    selectListSharedItem filterList =fmap Tagged (fmap (fmap map_entity_toRefSharedItem) reader_data)
        where
            reader_data = selectList filters []
            filters  = ref_to_pers_filters_SharedItem filterList
            map_entity_toRefTodoItem x = RefinedSharedItem{shareFrom = sharedItemShareFrom $ entityVal x, sharedItemId = entityKey x, shareTo = sharedItemShareTo $entityVal x}

    ref_to_pers_filters_SharedItem::FilterList RefinedSharedItem  -> [Filter Model.User]
    ref_to_pers_filters_SharedItem Empty = []
    ref_to_pers_filters_SharedItem (Cons f b) = (toPersistentFilterSharedItem f):(ref_to_pers_filters_SharedItem b)
    

    data RefinedSharedItem =RefinedSharedItem {shareFrom::UserId, shareTo::UserId, sharedItemId :: ShareItemId}
    -- data FilterList a = Empty | Cons (RefinedFilter a) (FilterList a)
    -- -- data RefinedFilter record = RefinedFilter (Filter record)

    -- data RefinedFilter record = forall typ.PersistField typ => RefinedFilter
    --     { refinedFilterField  :: RefinedEntityField record typ
    --     , refinedFilterValue  :: typ
    --     , refinedFilterFilter :: RefinedPersistFilter
    --     } 
    
    toPersistentFilterSharedItem  :: RefinedFilter RefinedSharedItem  -> Filter SharedItem 
    toPersistentFilterSharedItem  (RefinedFilter f value filter) =
        case filter of
            EQUAL -> field ==. value
            NE -> field !=. value
            LE -> field <=. value
            LTP -> field <. value
            GE -> field >=. value
            GTP -> field >. value
            where
                field = (refentity_entity_SharedItem   f)
    
    -- data RefinedEntityField a b where
    --     RefUserName :: RefinedEntityField RefinedTodoItem Text
    
    -- data RefinedPersistFilter = EQUAL | NE | LE | LTP | GE | GTP

    refentity_entity_SharedItem   :: RefinedEntityField RefinedSharedItem  b ->  EntityField  b
    refentity_entity_SharedItem   RefSharedItemShareTo = sharedItemShareTo
    refentity_entity_SharedItem   RefSharedItemShareFrom = sharedItemShareFrom

    --- common stuff
    data Tagged a = Tagged { content :: a }
        deriving Eq

    data RefinedEntityField a b where
        RefUserName :: RefinedEntityField RefinedUser Text
        RefTodoTask :: RefinedEntityField RefinedTodoItem Text
        RefTodoDone :: RefinedEntityField RefinedTodoItem Bool
        RefTodoUserId:: RefinedEntityField RefinedTodoItem UserId
        RefSharedItemShareTo::RefinedEntityField RefinedSharedItem UserId
        RefSharedItemShareFrom::RefinedEntityField RefinedSharedItem UserId

    
    data RefinedPersistFilter = EQUAL | NE | LE | LTP | GE | GTP
        -- getAllsharedFrom :: MonadIO m => UserId -> ReaderT SqlBackend m [Entity SharedItem]
-- getAllsharedFrom currentUserId = selectList [SharedItemShareTo ==. currentUserId] [Asc SharedItemId]
