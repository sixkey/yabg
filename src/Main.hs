{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Bifunctor ( first, second )

import Control.Monad
import Control.Monad.Trans ( lift )
import Control.Monad.Writer ( WriterT (runWriterT) )
import Control.Monad.RWS ( RWST, MonadWriter (tell), runRWST, get, tell, modify )

import System.FilePath.Find ( find, always, extension, (==?) )
import System.FilePath ( (</>), makeRelative, takeDirectory, (<.>), dropExtension )
import System.Directory
import System.Directory.PathWalk

import Debug.Trace ( traceShowId )

import qualified Data.Text as DT ( lines, unlines, drop )
import Data.Text ( pack, unpack, Text, isPrefixOf, split )

import qualified Text.Pandoc as P
import Text.Pandoc.Shared ( trim )
import Text.Pandoc.Writers.Shared ( lookupMetaString )

import Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as A

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

data YabgBlock = PDoc P.Pandoc
               | InlineDir [ String ] deriving Show

data Yabgdoc = Yabgdoc P.Meta [ YabgBlock ] deriving Show

type ReadPostMonad a = RWST () [ YabgBlock ] ( [ Text ], Maybe P.Meta ) IO a

parseMarkdown :: Text -> IO P.Pandoc
parseMarkdown text = do
    print text
    P.runIOorExplode $ P.readMarkdown yabgReaderOptions text

readPost :: FilePath -> IO Yabgdoc
readPost filePath = do
    text <- readFile filePath
    print text
    goText ( pack text )
    where
        flushLines :: ReadPostMonad ()
        flushLines = do
            lns <- fst <$> get
            unless ( null lns ) $ do
                doc <- lift $ parseMarkdown ( DT.unlines . reverse $ lns )
                oldMeta <- snd <$> get
                when ( isNothing oldMeta ) $ do
                    let meta = getPandocMeta doc
                    modify $ second ( const $ Just meta )
                modify $ first $ const []
                tell . pure $ PDoc doc
        goText :: Text -> IO Yabgdoc
        goText text = do
            ( _, ( _, meta ), blocks ) <- ( \x -> runRWST x () ( [], Nothing ) ) $ do
                mapM_ go ( DT.lines text )
                lns <- fst <$> get
                flushLines
            case meta of
                Just metaContent -> return $ Yabgdoc metaContent blocks
                Nothing -> ioError $ userError "meta not defined"
        go :: Text -> ReadPostMonad ()
        go line =
            if "%%%" `isPrefixOf` line then do
               let cmnd = map unpack $ split ( == ' ' )
                                                ( trim . DT.drop 3 $ line )
               flushLines
               tell $ pure $ traceShowId ( InlineDir cmnd )
               return ()
            else do
               modify $ first ( line : )

documentToPost :: Yabgdoc -> IO Post
documentToPost ( Yabgdoc meta blocks ) = do
        ( _, html ) <- runWriterT $ forM_ blocks $ go
        return $ post meta html
    where
        go :: YabgBlock -> WriterT H.Html IO ()
        go ( InlineDir [ "image-library", dir ] ) = tell $ H.p "spool library"
        go ( InlineDir x ) = undefined
        go ( PDoc document ) =
            do html <- lift . P.runIOorExplode $
                 P.writeHtml5 yabgWriterOptions document
               tell html

writePost :: YabgSettings -> FilePath -> Post -> IO ()
writePost settings path pst = do
    let renderedHtml = renderPost settings pst
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path ( renderHtml renderedHtml )

postPipeline :: YabgSettings -> FilePath -> IO ()
postPipeline settings filePath = do
    document <- readPost ( srcPath settings </> filePath )
    pst <- documentToPost document
    let outPath = changeExt "html" $ dstPath settings </> filePath
    writePost settings outPath pst

postDirectory :: YabgSettings -> IO ()
postDirectory settings =
    do let src = srcPath settings
       files <- find always ( extension ==? ".pst" ) src
       forM_ files $ postPipeline settings . makeRelative src

translatePath :: FilePath -> FilePath -> FilePath -> FilePath
translatePath srcDir dstDir path = dstDir </> makeRelative srcDir path

yabgCopyDirs :: YabgSettings -> IO ()
yabgCopyDirs settings = forM_ ( dirsToCopy settings ) $ \ dir -> do
    copyDir dir ( translatePath ( srcPath settings ) ( dstPath settings ) dir )

yabgPipeline :: YabgSettings -> IO ()
yabgPipeline settings = do
    yabgCopyDirs settings
    postDirectory settings{ srcPath = srcPath settings </> "pages", dstPath = "bin" }

main :: IO ()
main = do print "yabg"
          yabgPipeline $ YabgSettings { srcPath = "xlogin"
                                      , dstPath = "bin"
                                      , dirsToCopy = [ "xlogin/public" ]
                                      , defLinks = [ "/public/index.css" ]
                                      , nav = [ ( "xkucerak", "/" )
                                              , ( "blog", "/blog" )
                                              , ( "js-snippets", "/js-snippets" )
                                              ] }
