{-# LANGUAGE OverloadedStrings #-}

module Yabg where 

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
import qualified Text.Pandoc.Walk as P
import Debug.Trace (traceShowId)

import Options.Applicative

-- Data -----------------------------------------------------------------------

data Post = Post { title :: String
                 , image :: FilePath
                 , content :: H.Html }

data YabgSettings = YabgSettings { srcPath :: FilePath
                                 , dstPath :: FilePath
                                 , dirsToCopy :: [ FilePath ]
                                 , defLinks :: [ String ]
                                 , nav :: [ ( String, String ) ] }

-- Misc. IO -------------------------------------------------------------------

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

renderNavigation :: [ ( String, String ) ] -> H.Html
renderNavigation links = H.ul ! A.id "nav" $
    forM_ links $ \ ( text, url ) ->
        H.li $ H.a ! A.href ( H.stringValue url ) $ H.toHtml text

renderPost :: YabgSettings -> Post -> H.Html
renderPost settings post = H.html $ do
    H.head $ do
        H.title ( H.toHtml $ title post )
        H.script ! A.src ( H.stringValue $ unpack P.defaultMathJaxURL ) $ pure ()
        mapM_ ( \href -> H.link ! A.rel "stylesheet" ! A.href ( H.stringValue href ) )
              ( defLinks settings )
    H.body $ do
        H.div ! A.id "root" $ do
            H.div ! A.id "left-wing" $ sideBar post
            H.div ! A.id "main-body" $ do
                H.div ! A.id "title-nav" $ do
                    postHeader post
                    renderNavigation ( nav settings )
                content post


-- Pipelinining ---------------------------------------------------------------

yabgWriterOptions :: P.WriterOptions
yabgWriterOptions = P.def{ P.writerHTMLMathMethod = P.MathJax P.defaultMathJaxURL }

yabgReaderOptions :: P.ReaderOptions
yabgReaderOptions = P.def{ P.readerExtensions = P.pandocExtensions }

readPost :: FilePath -> IO P.Pandoc
readPost filePath = do content <- pack <$> readFile filePath
                       P.runIOorExplode $ P.readMarkdown yabgReaderOptions content

documentToPost :: P.Pandoc -> IO Post
documentToPost document = P.runIOorExplode $ do
                                html <- P.writeHtml5 yabgWriterOptions document
                                return $ post ( getPandocMeta document ) html

writePost :: YabgSettings -> FilePath -> Post -> IO ()
writePost settings path post = do
    let renderedHtml = renderPost settings post
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path ( renderHtml renderedHtml )

postPipeline :: YabgSettings -> FilePath -> IO ()
postPipeline settings filePath =
    do document <- readPost ( srcPath settings </> filePath )
       post <- documentToPost document
       let outPath = changeExt "html" $ dstPath settings </> filePath
       writePost settings outPath post

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
    postDirectory settings{ srcPath = "tst/pages", dstPath = "bin" }
