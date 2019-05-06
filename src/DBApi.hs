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
    import           Model
    import Foundation            as Import
    import Import.NoFoundation   as Import


    -- {-@ assume selectListUser@-}
    {-@ 
    assume selectListUser :: MonadIO m => forall <q :: RefinedUser -> RefinedUser -> Bool, r :: RefinedUser -> Bool, p :: RefinedUser -> Bool>.
    {row :: RefinedUser<r> |- RefinedUser<p> <: RefinedUser<q row>}
    FilterList<q, r> RefinedUser ->  ReaderT SqlBackend m (Tagged<p> [RefinedUser<r>])
    @-}
    selectListUser :: MonadIO m => FilterList RefinedUser -> ReaderT SqlBackend m (Tagged [RefinedUser])
    selectListUser filterList =fmap Tagged (fmap (fmap map_entity_toRefUser) reader_data)
        where
            reader_data = selectList filters []
            filters  = ref_to_pers_filters_user filterList
            map_entity_toRefUser x = RefinedUser{userName = userUserName $ entityVal x, refUserUserId = entityKey x}
    
    -- {-@ @-}
    ref_to_pers_filters_user::FilterList RefinedUser  -> [Filter Model.User]
    ref_to_pers_filters_user Empty = []
    ref_to_pers_filters_user (Cons f b) = (toPersistentFilterUser f):(ref_to_pers_filters_user b)
    

    data RefinedUser =RefinedUser {userName::Text ,refUserUserId:: UserId}
   
    toPersistentFilterUser :: RefinedFilter RefinedUser -> Filter User
    toPersistentFilterUser (RefinedFilter f value lvalue filter) =
        case filter of
            EQUAL -> field ==. value
            NE -> field !=. value
            LE -> field <=. value
            LTP -> field <. value
            GE -> field >=. value
            GTP -> field >. value
            IN -> (field <-. lvalue)
            NOTIN -> field /<-. lvalue
            where
                field = (refentity_entity_user f)
    


    refentity_entity_user :: RefinedEntityField RefinedUser b ->  EntityField User b
    refentity_entity_user RefUserName = UserUserName


    
    --fortodoitem
    {-@ 
    assume selectListTodoItem :: MonadIO m => forall <q :: RefinedTodoItem -> RefinedUser -> Bool, r :: RefinedTodoItem -> Bool, p :: RefinedUser -> Bool>.
    {row :: RefinedTodoItem<r> |- RefinedUser<p> <: RefinedUser<q row>}
    FilterList<q, r> RefinedTodoItem ->  ReaderT SqlBackend m (Tagged<p> [RefinedTodoItem<r>])
    @-}
    selectListTodoItem :: MonadIO m => FilterList RefinedTodoItem -> ReaderT SqlBackend m (Tagged [RefinedTodoItem])
    selectListTodoItem filterList =fmap Tagged (fmap (fmap map_entity_toRefTodoItem) reader_data)
        where
            reader_data = selectList filters []
            filters  = ref_to_pers_filters_TodoItem filterList
            map_entity_toRefTodoItem x = RefinedTodoItem{task = todoItemTask $ entityVal x, refTodoItemTodoItemId = entityKey x, done = todoItemDone $ entityVal x,
                                                            tuserId = todoItemUserId $ entityVal x}

    ref_to_pers_filters_TodoItem::FilterList RefinedTodoItem  -> [Filter Model.TodoItem]
    ref_to_pers_filters_TodoItem Empty = []
    ref_to_pers_filters_TodoItem (Cons f b) = (toPersistentFilterTodoItem f):(ref_to_pers_filters_TodoItem b)
    

    data RefinedTodoItem =RefinedTodoItem {task::Text, refTodoItemTodoItemId::TodoItemId, done::Bool,tuserId::UserId}
    -- data FilterList a = Empty | Cons (RefinedFilter a) (FilterList a)
    -- -- data RefinedFilter record = RefinedFilter (Filter record)

    -- data RefinedFilter record = forall typ.PersistField typ => RefinedFilter
    --     { refinedFilterField  :: RefinedEntityField record typ
    --     , refinedFilterValue  :: typ
    --     , refinedFilterFilter :: RefinedPersistFilter
    --     } 
    
    toPersistentFilterTodoItem :: RefinedFilter RefinedTodoItem -> Filter TodoItem
    toPersistentFilterTodoItem (RefinedFilter f value lvalue filter) =
        case filter of
            EQUAL -> field ==. value
            NE -> field !=. value
            LE -> field <=. value
            LTP -> field <. value
            GE -> field >=. value
            GTP -> field >. value
            IN -> field <-. lvalue
            NOTIN -> field /<-. lvalue
            where
                field = (refentity_entity_TodoItem  f)
    
    -- data RefinedEntityField a b where
    --     RefUserName :: RefinedEntityField RefinedTodoItem Text
    
    -- data RefinedPersistFilter = EQUAL | NE | LE | LTP | GE | GTP

    refentity_entity_TodoItem  :: RefinedEntityField RefinedTodoItem b ->  EntityField  TodoItem b
    refentity_entity_TodoItem  RefTodoTask = TodoItemTask
    refentity_entity_TodoItem  RefTodoDone = TodoItemDone
    refentity_entity_TodoItem  RefTodoUserId = TodoItemUserId

    --for SharedItem
    {-@ measure sharedItemProp :: UserId->UserId-> Bool@-}
    {-@ 
    assume selectListSharedItem :: MonadIO m => forall <q :: RefinedSharedItem -> RefinedUser -> Bool, r :: RefinedSharedItem -> Bool, p :: RefinedUser -> Bool>.
    {row :: RefinedSharedItem<r> |- RefinedUser<p> <: RefinedUser<q row>}
    FilterList<q, r> RefinedSharedItem ->  ReaderT SqlBackend m (Tagged<p> [{v:RefinedSharedItem<r>|sharedItemProp (shareFrom v) (shareTo v)}])
    @-}
    selectListSharedItem :: MonadIO m => FilterList RefinedSharedItem -> ReaderT SqlBackend m (Tagged [RefinedSharedItem])
    selectListSharedItem filterList =fmap Tagged (fmap (fmap map_entity_toRefSharedItem) reader_data)
        where
            reader_data = selectList filters []
            filters  = ref_to_pers_filters_SharedItem filterList
            map_entity_toRefSharedItem x = RefinedSharedItem{shareFrom = sharedItemShareFrom $ entityVal x, refSharedItemSharedItemId = entityKey x, shareTo = sharedItemShareTo $ entityVal x}

    ref_to_pers_filters_SharedItem::FilterList RefinedSharedItem  -> [Filter Model.SharedItem]
    ref_to_pers_filters_SharedItem Empty = []
    ref_to_pers_filters_SharedItem (Cons f b) = (toPersistentFilterSharedItem f):(ref_to_pers_filters_SharedItem b)
    

    {-@
    assume projectSharedItemShareFrom :: forall <r1::RefinedSharedItem -> Bool, r2::UserId -> Bool>.
      {rsi::RefinedSharedItem<r1> |- UserId<r2> <: {from:UserId | sharedItemProp from (shareTo rsi)}}
      xs:[RefinedSharedItem<r1>] -> Tagged<{\v -> True}> [UserId<r2>]
     @-}                                -- ^^^^^^^^^^^ this is the policy on the field ShareFrom
    projectSharedItemShareFrom :: [RefinedSharedItem] -> Tagged [UserId]
    projectSharedItemShareFrom input = Tagged {content = fmap (shareFrom) input}



    data RefinedSharedItem =RefinedSharedItem {shareFrom::UserId, shareTo::UserId, refSharedItemSharedItemId :: SharedItemId}
    -- data FilterList a = Empty | Cons (RefinedFilter a) (FilterList a)
    -- -- data RefinedFilter record = RefinedFilter (Filter record)

    -- data RefinedFilter record = forall typ.PersistField typ => RefinedFilter
    --     { refinedFilterField  :: RefinedEntityField record typ
    --     , refinedFilterValue  :: typ
    --     , refinedFilterFilter :: RefinedPersistFilter
    --     } 
    
    toPersistentFilterSharedItem  :: RefinedFilter RefinedSharedItem  -> Filter SharedItem 
    toPersistentFilterSharedItem  (RefinedFilter f value lvalue filter) =
        case filter of
            EQUAL -> field ==. value
            NE -> field !=. value
            LE -> field <=. value
            LTP -> field <. value
            GE -> field >=. value
            GTP -> field >. value
            IN -> field <-. lvalue
            NOTIN -> field /<-. lvalue
            where
                field = (refentity_entity_SharedItem   f)
    
    -- data RefinedEntityField a b where
    --     RefUserName :: RefinedEntityField RefinedTodoItem Text
    
    -- data RefinedPersistFilter = EQUAL | NE | LE | LTP | GE | GTP

    refentity_entity_SharedItem   :: RefinedEntityField RefinedSharedItem  b ->  EntityField  SharedItem b
    refentity_entity_SharedItem   RefSharedItemShareTo = SharedItemShareTo
    refentity_entity_SharedItem   RefSharedItemShareFrom = SharedItemShareFrom

    --- common stuff
    data Tagged a = Tagged { content :: a }
        deriving Eq
    
    instance Functor Tagged where
        fmap f (Tagged x) = Tagged (f x)
        
    instance Applicative Tagged where
        pure  = Tagged
        -- f (a -> b) -> f a -> f b
        (Tagged f) <*> (Tagged x) = Tagged (f x)
    
    instance Monad Tagged where
        return x = Tagged x
        (Tagged x) >>= f = f x
        (Tagged _) >>  t = t
        fail          = error
    
    {-@ instance Monad Tagged where
        >>= :: forall <p :: User -> Bool, f:: a -> b -> Bool>.
                x:Tagged <p> a
            -> (u:a -> Tagged <p> (b <f u>))
            -> Tagged <p> (b<f (content x)>);
        >>  :: forall <p :: User -> Bool>.
                x:Tagged<{\v -> false}> a
            -> Tagged<p> b
            -> Tagged<p> b;
        return :: a -> Tagged <{\v -> true}> a
    @-}
    {-@
        data FilterList a <q :: a -> User -> Bool, r :: a -> Bool> where
            Empty :: FilterList<{\_ _ -> True}, {\_ -> True}> a
            | Cons :: RefinedFilter<{\_ -> True}, {\_ _ -> False}> a ->
                    FilterList<{\_ _ -> False}, {\_ -> True}> a ->
                    FilterList<q, r> a
    @-}
    {-@ data variance FilterList covariant contravariant covariant @-}
    data FilterList a = Empty | Cons (RefinedFilter a) (FilterList a)
    infixr 5 ?:
    {-@
        (?:) :: forall <r :: a -> Bool, r1 :: a -> Bool, r2 :: a -> Bool,
                        q :: a -> RefinedUser -> Bool, q1 :: a -> RefinedUser -> Bool, q2 :: a -> RefinedUser -> Bool>.
        {a_r1 :: a<r1>, a_r2 :: a<r2> |- {v:a | v == a_r1 && v == a_r2} <: a<r>}
        {row :: a<r> |- RefinedUser<q row> <: RefinedUser<q1 row>}
        {row :: a<r> |- RefinedUser<q row> <: RefinedUser<q2 row>}
        RefinedFilter<r1, q1> a ->
        FilterList<q2, r2> a ->
        FilterList<q, r> a
    @-}
    (?:) :: RefinedFilter a -> FilterList a -> FilterList a
    f ?: fs = f `Cons` fs
    
    {-@ data RefinedFilter record <r :: record -> Bool, q :: record -> RefinedUser -> Bool> = _ @-}
    
{-@ data variance RefinedFilter covariant covariant contravariant @-}


    data RefinedFilter record = forall typ.PersistField typ => RefinedFilter
        { refinedFilterField  :: RefinedEntityField record typ
        , refinedFilterValue  :: typ
        , refinedFilterLValue  :: [typ]
        , refinedFilterFilter :: RefinedPersistFilter
        } 

    {-@
        data RefinedEntityField record typ <q :: record -> User -> Bool> where 
            RefUserName :: RefinedEntityField <{\row v -> True}> RefinedUser {v:_ | True}
         |  RefTodoTask :: RefinedEntityField <{\row v -> True}> RefinedTodoItem {v:_ | True}
         |  RefTodoDone :: RefinedEntityField <{\row v -> True}> RefinedTodoItem {v:_ | True}
         |  RefTodoUserId :: RefinedEntityField <{\row v -> True}> RefinedTodoItem {v:_ | True}
         |  RefSharedItemShareTo :: RefinedEntityField <{\row v -> True}> RefinedSharedItem {v:_ | True}
         |  RefSharedItemShareFrom :: RefinedEntityField <{\row v -> True}> RefinedSharedItem {v:_ | True}
    @-}
    {-@ data variance EntityField covariant covariant contravariant @-}
            
    data RefinedEntityField a b where
        RefUserName :: RefinedEntityField RefinedUser Text
        RefTodoTask :: RefinedEntityField RefinedTodoItem Text
        RefTodoDone :: RefinedEntityField RefinedTodoItem Bool
        RefTodoUserId:: RefinedEntityField RefinedTodoItem UserId
        RefSharedItemShareTo::RefinedEntityField RefinedSharedItem UserId
        RefSharedItemShareFrom::RefinedEntityField RefinedSharedItem UserId

    
    data RefinedPersistFilter = EQUAL | NE | LE | LTP | GE | GTP | IN | NOTIN

    -- data Filter a = Filter
    
    {-@ filterUserName ::
       val: Text -> RefinedFilter<{\row -> userName row == val}, {\row v -> True}> RefinedUser @-}
    filterUserName :: Text -> RefinedFilter RefinedUser
    filterUserName val = RefinedFilter{ refinedFilterField = RefUserName, refinedFilterValue=val,
                         refinedFilterFilter= EQUAL}

    -- {-@ filterSharedTo ::
    --     val: UserId -> RefinedFilter<{\row -> sharedTo row == val}, {\row v -> True}> RefinedSharedItem @-}
    -- filterSharedTo :: UserId -> RefinedFilter RefinedSharedItem
    -- filterSharedTo val = RefinedFilter{ refinedFilterField = RefSharedItemShareTo, refinedFilterValue=val,
    --                     refinedFilterFilter= EQUAL}
    
    {-@ filterSharedItemShareFrom_EQ ::
    val: UserId -> RefinedFilter<{\row -> sharedFrom row == val}, {\row v -> True}> RefinedSharedItem @-}
    filterSharedItemShareFrom_EQ :: UserId -> RefinedFilter RefinedSharedItem
    filterSharedItemShareFrom_EQ val = RefinedFilter{ refinedFilterField = RefSharedItemShareFrom, refinedFilterValue=val,
                        refinedFilterFilter= EQUAL}
    
    {-@ filterTodoItemUserId_EQ ::
    val: UserId -> RefinedFilter<{\row -> tuserId row == val}, 
            {\row v -> tuserId row == refUserUserId v || sharedItemProp (tuserId row) (refUserUserId v)}> RefinedTodoItem @-}
    filterTodoItemUserId_EQ :: UserId -> RefinedFilter RefinedTodoItem
    filterTodoItemUserId_EQ val = RefinedFilter{ refinedFilterField = RefTodoUserId, refinedFilterValue=val,
                        refinedFilterFilter= EQUAL}
    
    
    -- {-@ filterTodoItemUserId_IN ::
    -- val: Int -> RefinedFilter<{\row -> tuserId row == val}, 
    --         {\row v -> tuserId row == refUserUserId v || sharedItemProp (tuserId row) (refUserUserId v)}> RefinedTodoItem @-}

    {-@ filterTodoItemUserId_IN :: forall <r::UserId -> Bool, f :: RefinedTodoItem -> Bool>.
    { v::UserId<r> |- RefinedTodoItem<f> <: RefinedTodoItem<{\r -> tuserId r == v}> }
        val: [UserId<r>] -> RefinedFilter<f, {\row v -> tuserId row == refUserUserId v || sharedItemProp  (tuserId row) (refUserUserId v)}> RefinedTodoItem @-}
    filterTodoItemUserId_IN ::  [UserId] -> RefinedFilter RefinedTodoItem
    filterTodoItemUserId_IN val = RefinedFilter{ refinedFilterField = RefTodoUserId, refinedFilterLValue=val,
                        refinedFilterFilter= IN}
    
    {-@ filterSharedItemShareTo_EQ ::
        val: UserId -> RefinedFilter<{\row -> sharedTo row == val}, {\row v -> True}> RefinedSharedItem @-}
    filterSharedItemShareTo_EQ :: UserId -> RefinedFilter RefinedSharedItem
    filterSharedItemShareTo_EQ val = RefinedFilter{ refinedFilterField = RefSharedItemShareTo, refinedFilterValue=val,
                        refinedFilterFilter= EQUAL}
    
    
        -- getAllsharedFrom :: MonadIO m => UserId -> ReaderT SqlBackend m [Entity SharedItem]
-- getAllsharedFrom currentUserId = selectList [SharedItemShareTo ==. currentUserId] [Asc SharedItemId]
