-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import qualified Prelude as P
import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

-- import Prelude hiding (.)
-- import Yesod.Auth.OpenId    (authOpenId, IdentifierType (Claimed))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import Yesod.Core -- qualified Yesod.Routes.Class

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
-- LIQUID-TH-HASSLES: mkYesodData "App" $(parseRoutesFile "config/routes")


-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            undefined -- LIQUID-TH: $(widgetFile "default-layout")
        undefined -- LIQUID-TH: withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized _ _ = return Authorized
    -- isAuthorized TodoItemR _ = return Authorized
    -- isAuthorized HomeR _ = return Authorized
    -- isAuthorized FaviconR _ = return Authorized
    -- isAuthorized RobotsR _ = return Authorized
    -- isAuthorized (StaticR _) _ = return Authorized

    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userUserName = credsIdent creds
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app =  [authDummy]


-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
--
--
--
-- LIQUID-TH-HASSLES
--
-- src/Foundation.hs:65:1-52: Splicing declarations
instance ParseRoute App where
  parseRoute
    = ((\ f_aesH x_aesI -> (f_aesH ()) x_aesI) ::
         (()
          -> ([Text],
              [(Text, Text)])
             -> Maybe (Route a_aesJ))
         -> ([Text],
             [(Text, Text)])
            -> Maybe (Route a_aesJ))
        helper_aesG
    where
        helper_aesG env1258_aesd req1258_aese
          = helper1258_aesf (fst req1258_aese)
          where
              helper1258_aesf ((:) "static" restPath_aesh)
                = (((((\ _runHandler_aesk _getSub_aesl toMaster_aesm _env_aesn
                         -> (fmap toMaster_aesm
                               . parseRoute))
                        (\ _ _ x_aeso _ -> x_aeso))
                       (\ sub_aesp -> appStatic sub_aesp))
                      (\ sroute_aesq -> StaticR sroute_aesq))
                     env1258_aesd)
                    (((\ p_aesi (_, q_aesj) -> (p_aesi, q_aesj)) restPath_aesh)
                       req1258_aese)
              helper1258_aesf ((:) "favicon.ico" [])
                = ((((\ _ _ x_aesr _ -> x_aesr) (error "mdsGetHandler"))
                      env1258_aesd)
                     (Just FaviconR))
                    req1258_aese
              helper1258_aesf ((:) "robots.txt" [])
                = ((((\ _ _ x_aess _ -> x_aess) (error "mdsGetHandler"))
                      env1258_aesd)
                     (Just RobotsR))
                    req1258_aese
              helper1258_aesf []
                = ((((\ _ _ x_aest _ -> x_aest) (error "mdsGetHandler"))
                      env1258_aesd)
                     (Just HomeR))
                    req1258_aese
              helper1258_aesf ((:) "todo-item" [])
                = ((((\ _ _ x_aesu _ -> x_aesu) (error "mdsGetHandler"))
                      env1258_aesd)
                     (Just TodoItemR))
                    req1258_aese
              helper1258_aesf ((:) "shared-items" [])
                = ((((\ _ _ x_aesv _ -> x_aesv) (error "mdsGetHandler"))
                      env1258_aesd)
                     (Just ShareTodoListR))
                    req1258_aese
              helper1258_aesf ((:) "auth" restPath_aesw)
                = (((((\ _runHandler_aesz _getSub_aesA toMaster_aesB _env_aesC
                         -> (fmap toMaster_aesB
                               . parseRoute))
                        (\ _ _ x_aesD _ -> x_aesD))
                       (\ sub_aesE -> getAuth sub_aesE))
                      (\ sroute_aesF -> AuthR sroute_aesF))
                     env1258_aesd)
                    (((\ p_aesx (_, q_aesy) -> (p_aesx, q_aesy)) restPath_aesw)
                       req1258_aese)
              helper1258_aesf _
                = ((((\ _ _ x_aesg _ -> x_aesg) (error "mds404"))
                      env1258_aesd)
                     Nothing)
                    req1258_aese
instance RenderRoute App where
  data Route App
    = StaticR (Route Static) |
      FaviconR |
      RobotsR |
      HomeR |
      TodoItemR |
      ShareTodoListR |
      AuthR (Route Auth)
    deriving (Show, Eq, Read)
  renderRoute (StaticR sub_aerM)
    = (\ (a_aerN, b_aerO)
         -> ((pack "static" : a_aerN), b_aerO))
        (renderRoute sub_aerM)
  renderRoute FaviconR = ((pack "favicon.ico" : []), [])
  renderRoute RobotsR = ((pack "robots.txt" : []), [])
  renderRoute HomeR = ([], [])
  renderRoute TodoItemR = ((pack "todo-item" : []), [])
  renderRoute ShareTodoListR
    = ((pack "shared-items" : []), [])
  renderRoute (AuthR sub_aerP)
    = (\ (a_aerQ, b_aerR)
         -> ((pack "auth" : a_aerQ), b_aerR))
        (renderRoute sub_aerP)
instance RouteAttrs App where
  routeAttrs _ = undefined 

  -- routeAttrs StaticR {}
  --   = fromList []
  -- routeAttrs FaviconR {}
  --   = fromList []
  -- routeAttrs RobotsR {}
  --   = fromList []
  -- routeAttrs HomeR {}
  --   = fromList []
  -- routeAttrs TodoItemR {}
  --   = fromList []
  -- routeAttrs ShareTodoListR {}
  --   = fromList []
  -- routeAttrs AuthR {}
  --   = fromList []

--resourcesApp :: [ResourceTree String]
resourcesApp = undefined
  -- = [ResourceLeaf
  --      (((((Resource "StaticR")
  --            [Static "static"])
  --           ((Subsite "Static") "appStatic"))
  --          [])
  --         True),
  --    ResourceLeaf
  --      (((((Resource "FaviconR")
  --            [Static "favicon.ico"])
  --           ((Methods Nothing) ["GET"]))
  --          [])
  --         True),
  --    ResourceLeaf
  --      (((((Resource "RobotsR")
  --            [Static "robots.txt"])
  --           ((Methods Nothing) ["GET"]))
  --          [])
  --         True),
  --    ResourceLeaf
  --      (((((Resource "HomeR") [])
  --           ((Methods Nothing) ["GET"]))
  --          [])
  --         True),
  --    ResourceLeaf
  --      (((((Resource "TodoItemR")
  --            [Static "todo-item"])
  --           ((Methods Nothing) ["POST"]))
  --          [])
  --         True),
  --    ResourceLeaf
  --      (((((Resource "ShareTodoListR")
  --            [Static "shared-items"])
  --           ((Methods Nothing) ["POST"]))
  --          [])
  --         True),
  --    ResourceLeaf
  --      (((((Resource "AuthR")
  --            [Static "auth"])
  --           ((Subsite "Auth") "getAuth"))
  --          [])
  --         True)]

type Handler = HandlerFor App
type Widget = WidgetFor App ()
