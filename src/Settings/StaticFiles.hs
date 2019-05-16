-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.StaticFiles where

import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.Static -- (staticFiles)

-- import qualified Prelude as P
-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []
-- staticFiles (appStaticDir compileTimeAppSettings)

css_bootstrap_css :: Yesod.Static.StaticRoute
css_bootstrap_css = undefined
{-
      = (Yesod.Static.StaticRoute
           ((map Data.Text.pack) ["css", "bootstrap.css"]))
          [(Data.Text.pack "etag", Data.Text.pack "PDMJ7RwX")]
-}

fonts_glyphicons_halflings_regular_woff :: Yesod.Static.StaticRoute
fonts_glyphicons_halflings_regular_woff = undefined

{-
      = (Yesod.Static.StaticRoute
           ((map Data.Text.pack)
              ["fonts", "glyphicons-halflings-regular.woff"]))
          [(Data.Text.pack "etag", Data.Text.pack "aO0drAa_")]
-}

fonts_glyphicons_halflings_regular_eot :: Yesod.Static.StaticRoute
fonts_glyphicons_halflings_regular_eot = undefined

{-
      = (Yesod.Static.StaticRoute
           ((map Data.Text.pack)
              ["fonts", "glyphicons-halflings-regular.eot"]))
          [(Data.Text.pack "etag", Data.Text.pack "etF8YIXe")]
-}

fonts_glyphicons_halflings_regular_ttf :: Yesod.Static.StaticRoute
fonts_glyphicons_halflings_regular_ttf = undefined

{-
      = (Yesod.Static.StaticRoute
           ((map Data.Text.pack)
              ["fonts", "glyphicons-halflings-regular.ttf"]))
          [(Data.Text.pack "etag", Data.Text.pack "5J1S50t2")]
-}

fonts_glyphicons_halflings_regular_svg :: Yesod.Static.StaticRoute
fonts_glyphicons_halflings_regular_svg = undefined
{-
      = (Yesod.Static.StaticRoute
           ((map Data.Text.pack)
              ["fonts", "glyphicons-halflings-regular.svg"]))
           [(Data.Text.pack "etag", Data.Text.pack "MpQdYzAE")]<Paste>
-}
