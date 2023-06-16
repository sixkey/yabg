{-# LANGUAGE OverloadedStrings #-}

import System.FilePath.Find ( find, always, extension, (==?) )
import System.FilePath ( (</>), makeRelative, takeDirectory, (<.>), dropExtension )
import System.Directory
import System.Directory.PathWalk

import Control.Monad

import Data.Text ( pack, unpack )

import qualified Text.Pandoc as P
import qualified Text.Pandoc.Definition
import Text.Pandoc.Writers.Shared ( lookupMetaString )

import Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc ( readMarkdown )
import Control.Monad.Except ( ExceptT )
import Path ( mkRelDir )
import Path.Like ( toPath )

-- Data -----------------------------------------------------------------------

data Post = Post { title :: String
                 , image :: FilePath
                 , content :: H.Html }

data YabgSettings = YabgSettings { srcPath :: FilePath
                                 , dstPath :: FilePath
                                 , dirsToCopy :: [ FilePath ]
                                 , defLinks :: [ String ] }

-- File IO --------------------------------------------------------------------

changeExt :: String -> FilePath -> FilePath
changeExt ext filepath = dropExtension filepath <.> ext

copyDir :: FilePath -> FilePath -> IO ()
copyDir srcDir dstDir = pathWalk srcDir $ \ dir subdirs files ->
    do createDirectoryIfMissing True ( tr dir )
       forM_ files $ \ file -> do
           let wholeFile = dir </> file
           copyFile wholeFile ( tr wholeFile )
    where
       tr = translatePath srcDir dstDir

readPost :: FilePath -> IO ( H.Html, P.Meta )
readPost filePath = do content <- pack <$> readFile filePath
                       P.runIOorExplode $
                         do document <- P.readMarkdown
                                        P.def{ P.readerExtensions = P.extensionsFromList $ pure P.Ext_yaml_metadata_block }
                                        content
                            html <- P.writeHtml5 P.def document
                            return ( html, getPandocMeta document )

-- Misc. ----------------------------------------------------------------------

getPandocMeta :: P.Pandoc -> P.Meta
getPandocMeta ( P.Pandoc meta blocks ) = meta;

post :: P.Meta -> H.Html -> Post
post meta content =
    let title = lookupMetaString "title" meta
        image = lookupMetaString "image" meta
     in Post { title = unpack title
             , image = unpack image
             , content = content }

-- Rendering ------------------------------------------------------------------

postTitle :: String -> H.Html
postTitle = H.h1 . H.toHtml

postHeader :: Post -> H.Html
postHeader post = postTitle ( title post )

sideBar :: Post -> H.Html
sideBar post = H.img ! A.src ( H.stringValue $ image post ) ! A.id "title-image"

renderPost :: YabgSettings -> Post -> H.Html
renderPost settings post = H.html $ do
    H.head $ do
        H.title ( H.toHtml $ title post )
        mapM_ ( \href -> H.link ! A.rel "stylesheet" ! A.href ( H.stringValue href ) )
              ( defLinks settings )
    H.body $ do
        H.div ! A.id "root" $ do
            H.div ! A.id "left-wing" $ sideBar post
            H.div ! A.id "main-body" $ do
                postHeader post
                content post

-- Pipelinining ---------------------------------------------------------------

postPipeline :: YabgSettings -> FilePath -> IO ()
postPipeline settings filePath =
    do ( html, meta ) <- readPost ( srcPath settings </> filePath )
       let renderedHtml = renderPost settings ( post meta html )
           outPath = changeExt "html" $ dstPath settings </> filePath
       createDirectoryIfMissing True $ takeDirectory outPath
       writeFile outPath ( renderHtml renderedHtml )

postDirectory :: YabgSettings -> IO ()
postDirectory settings =
    do let src = srcPath settings
       files <- find always ( extension ==? ".pst" ) src
       forM_ files $ postPipeline settings . makeRelative src

translatePath :: FilePath -> FilePath -> FilePath -> FilePath
translatePath srcDir dstDir path = dstDir </> makeRelative srcDir path

yabgCopyDirs :: YabgSettings -> IO ()
yabgCopyDirs settings = forM_ ( dirsToCopy settings ) $ \ dir -> do
    print dir
    copyDir dir ( translatePath ( srcPath settings ) ( dstPath settings ) dir )

yabgPipeline :: YabgSettings -> IO ()
yabgPipeline settings = do
    yabgCopyDirs settings
    postDirectory settings{ srcPath = "tst/posts", dstPath = "bin/posts" }

main :: IO ()
main = do print "yabg"
          yabgPipeline $ YabgSettings { srcPath = "tst"
                                      , dstPath = "bin"
                                      , dirsToCopy = [ "tst/public" ]
                                      , defLinks = [ "/public/index.css" ] }
