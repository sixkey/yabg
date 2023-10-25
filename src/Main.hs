{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Maybe
import Data.Bifunctor ( first, second )

import Control.Monad
import Control.Monad.Trans ( lift, liftIO, MonadIO )
import Control.Monad.Writer ( WriterT ( runWriterT ) )
import Control.Monad.Reader ( ReaderT (runReaderT), MonadReader, local, ask, asks )
import Control.Monad.RWS ( RWST, MonadWriter ( tell ), runRWST, get, tell, modify )

import System.FilePath.Find ( find, always, extension, (==?) )
import System.FilePath ( (</>), makeRelative, takeDirectory, (<.>), dropExtension, dropExtensions )
import System.Directory
import System.Directory.PathWalk

import Debug.Trace ( traceShowId )

import qualified Data.Text as DT ( lines, unlines, drop )
import Data.Text ( pack, unpack, Text, isPrefixOf, split )

import qualified Text.Pandoc as P
import Text.Pandoc.Shared ( trim )
import Text.Pandoc.Writers.Shared ( lookupMetaString )

import Text.Regex.PCRE

import Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as A

import Options.Applicative

-- Data -----------------------------------------------------------------------

data PostMeta = PostMeta { title :: String
                         , desc :: String
                         , image :: Maybe String
                         , postPath :: String } deriving Show

data Post = Post { meta :: PostMeta
                 , content :: H.Html }

data YabgSettings = YabgSettings { srcPath :: FilePath
                                 , dstPath :: FilePath
                                 , rootPath :: String
                                 , dev :: Bool
                                 , dirsToCopy :: [ ( FilePath, FilePath ) ]
                                 , defLinks :: [ String ]
                                 , nav :: [ ( String, String ) ] }

data YabgBlock = PDoc P.Pandoc
               | InlineDir [ String ] deriving Show

data YabgDoc = YabgDoc PostMeta [ YabgBlock ] deriving Show


data YabgContext = YabgContext { settings :: YabgSettings
                               , posts :: [ PostMeta ] }

newtype YabgContextT m a = YabgContextT { runContextT :: ReaderT YabgContext m a }
    deriving ( Functor, Applicative, Monad, MonadReader YabgContext, MonadIO )

type YabgMonadT = YabgContextT
type YabgMonad = YabgMonadT IO

runYabgMonad :: YabgMonad a -> YabgSettings -> [ PostMeta ] -> IO a
runYabgMonad mon stngs psts = runReaderT ( runContextT mon )
                                         ( YabgContext { settings = stngs
                                                       , posts = psts } )


-- Misc. IO -------------------------------------------------------------------

changeExt :: String -> FilePath -> FilePath
changeExt ext filepath = dropExtension filepath <.> ext

translatePath :: FilePath -> FilePath -> FilePath -> FilePath
translatePath srcDir dstDir path = dstDir </> makeRelative srcDir path

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

post :: PostMeta -> H.Html -> Post
post meta content = Post { meta = meta
                         , content = content }

-- Path -----------------------------------------------------------------------

fromRoot :: YabgSettings -> String -> String
fromRoot sttngs = ( rootPath sttngs </> ) 

-- Rendering ------------------------------------------------------------------

comment :: H.Html -> H.Html
comment = H.em ! A.class_ "comment"

postTitle :: String -> H.Html
postTitle = H.h1 . H.toHtml

postHeader :: Post -> H.Html
postHeader = postTitle . title . meta

imagePath :: YabgSettings -> PostMeta -> Maybe String
imagePath sttngs meta = fromRoot sttngs <$> image meta

sideBar :: YabgSettings -> Post -> Maybe H.Html
sideBar sttngs post = do
    imgPath <- imagePath sttngs ( meta post )
    return $ H.img ! ( A.src . H.stringValue $ imgPath )
                   ! A.id "title-image"

renderNavigation :: [ ( String, String ) ] -> H.Html
renderNavigation links = H.ul ! A.id "nav" $
    forM_ links $ \ ( text, url ) ->
        H.li $ H.a ! A.href ( H.stringValue url ) $ H.toHtml text

postUrl :: YabgSettings -> PostMeta -> String
postUrl sttngs pMeta = changeExt "html" $ fromRoot sttngs $ postPath pMeta

listLibrary :: YabgSettings -> [ PostMeta ] -> H.Html
listLibrary sttngs [] = comment "this library is empty"
listLibrary sttngs posts = H.div ! A.class_ "list-lib" $ mapM_ listPost posts
  where
    listPost :: PostMeta -> H.Html
    listPost post = H.div ! A.class_ "list-lib-tile" $ 
        H.a ! A.href ( H.stringValue ( postUrl sttngs post ) ) $ do 
            H.p ! A.class_ "tile-title" $ H.toHtml ( title post )
            H.p ! A.class_ "tile-desc" $ H.toHtml ( desc post )

imageLibrary :: YabgSettings -> [ PostMeta ] -> H.Html
imageLibrary sttngs [] = comment "this library is empty"
imageLibrary sttngs posts = H.div ! A.class_ "image-lib" $ mapM_ imagePost posts
  where
    garImagePath :: PostMeta -> String
    garImagePath meta = fromMaybe ( fromRoot sttngs "public/square.png" ) 
                                  ( imagePath sttngs meta )
    imagePost :: PostMeta -> H.Html
    imagePost post = H.a ! A.href ( H.stringValue $ postUrl sttngs post )
                         ! A.class_ "image-lib-tile" $ do
        H.img ! A.src ( H.stringValue ( garImagePath post ) )
              ! A.class_ "image-lib-tile-bg"
        H.p ! A.class_ "image-lib-tile-fg"
            $ H.toHtml $ title post

maybeElement :: Maybe H.Html -> H.Html
maybeElement ( Just x ) = x
maybeElement Nothing = pure ()

renderPost :: YabgSettings -> Post -> H.Html
renderPost settings post = H.html $ do
    H.head $ do
        H.title ( H.toHtml . title . meta $ post )
        H.script ! A.src ( H.stringValue $ unpack P.defaultMathJaxURL ) $ pure ()
        H.meta ! A.name ( H.stringValue $ "viewport" ) ! A.content ( H.stringValue "width=device-width, initial-scale=1.0" )
        when ( dev settings ) $
           H.script ! A.src ( H.stringValue "https://livejs.com/live.js" ) $ pure ()
        mapM_ ( \href -> H.link ! A.rel "stylesheet" ! A.href ( H.stringValue href ) )
              ( defLinks settings )
    H.body $ do
        H.div ! A.id "root" $ do
            H.div ! A.id "left-wing" $
                maybeElement $ sideBar settings post
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

type ReadPostMonad = RWST () [ YabgBlock ] [ Text ] YabgMonad

parseMarkdown :: Text -> YabgMonad P.Pandoc
parseMarkdown text = liftIO $ do
    P.runIOorExplode $ P.readMarkdown yabgReaderOptions text

postSrc :: PostMeta -> YabgMonad FilePath
postSrc postMeta = asks ( ( </> postPath postMeta ) . srcPath . settings )

readPost :: PostMeta -> YabgMonad YabgDoc
readPost postMeta = do
    pstSrc <- postSrc postMeta
    text <- liftIO $ readFile pstSrc
    goText ( pack text )
    where
        flushLines :: ReadPostMonad ()
        flushLines = do
            lns <- get
            unless ( null lns ) $ do
                doc <- lift $ parseMarkdown ( DT.unlines . reverse $ lns )
                modify $ const []
                tell . pure $ PDoc doc
        goText :: Text -> YabgMonad YabgDoc
        goText text = do
            ( _, _, blocks ) <- ( \x -> runRWST x () [] ) $ do
                mapM_ go ( DT.lines text )
                lns <- get
                flushLines
            return $ YabgDoc postMeta blocks
        go :: Text -> ReadPostMonad ()
        go line =
            if "%%%" `isPrefixOf` line then do
               let cmnd = map unpack $ split ( == ' ' )
                                                ( trim . DT.drop 3 $ line )
               flushLines
               tell $ pure $ InlineDir cmnd
               return ()
            else do
               modify ( line : )

strReplace og nw = map ( \e -> if e == og then nw else e )

documentToPost :: YabgDoc -> YabgMonad Post
documentToPost ( YabgDoc meta blocks ) = do
        ( _, html ) <- runWriterT $ forM_ blocks $ go
        return $ post meta html
    where
        filterAtom :: String -> String -> PostMeta -> Bool
        filterAtom "title" p post = title post =~ p
        filterAtom "path" p post = postPath post =~ p
        filterAtom t p _ = error $ "undefined yabg filter: '"
                                   ++ show t ++ " "
                                   ++ show p ++ "'"
        buildFilter :: [ String ] -> PostMeta -> Bool
        buildFilter [] pst = True
        buildFilter ( "not" : t : p : rest ) pst =
            not ( filterAtom t p pst ) && buildFilter rest pst
        buildFilter ( t : p : rest ) pst =
            filterAtom t p pst && buildFilter rest pst

        buildFilter x _ = error $ "undefined yabg filter: " ++ show x
        postList :: String
                 -> ( YabgSettings -> [ PostMeta ] -> H.Html )
                 -> [ String ]
                 -> WriterT H.Html YabgMonad ()
        postList listTitle listFun fltr = do
            psts <- asks posts
            sttngs <- asks settings
            let relevantPosts = filter ( buildFilter fltr ) psts
            tell ( H.h3 ! A.class_ "lib-title" $ H.toHtml listTitle )
            tell ( listFun sttngs relevantPosts )
        parseTitle = strReplace '_' ' '

        go ( InlineDir ( "image-library" : title : rest ) ) =
            postList ( parseTitle title ) imageLibrary rest
        go ( InlineDir ( "list-library" : title : rest ) ) =
            postList ( parseTitle title ) listLibrary rest
        go ( InlineDir x ) = error $ "undefined yabg command: " ++ show x
        go ( PDoc document ) =
            do html <- liftIO $ P.runIOorExplode $
                 P.writeHtml5 yabgWriterOptions document
               tell html

writePost :: FilePath -> Post -> YabgMonad ()
writePost path pst = do
    stngs <- asks settings
    let renderedHtml = renderPost stngs pst
    liftIO $ do
        createDirectoryIfMissing True $ takeDirectory path
        writeFile path ( renderHtml renderedHtml )

postPipeline :: PostMeta -> YabgMonad ()
postPipeline postMeta = do
    liftIO . print $ postPath postMeta
    stngs <- asks settings
    document <- readPost postMeta
    pst <- documentToPost document
    let outPath = changeExt "html" $ dstPath stngs </> postPath postMeta
    writePost outPath pst

-- This is just awful
readPostMeta :: FilePath -> FilePath -> YabgMonad PostMeta
readPostMeta basePath path = do
    content <- liftIO $ pack <$> readFile path
    meta <- liftIO $ P.runIOorExplode
        ( getPandocMeta <$> P.readMarkdown yabgReaderOptions content )
    return $ fromPandocMeta meta
   where
      fromPandocMeta :: P.Meta -> PostMeta
      fromPandocMeta pMeta = PostMeta { title = metaGet "title"
                                      , desc = metaGet "desc"
                                      , image = maybyfy $ metaGet "image"
                                      , postPath = makeRelative basePath path }
          where
              metaGet s = unpack $ lookupMetaString s pMeta
              maybyfy [] = Nothing
              maybyfy x = Just x


yabgCopyDirs :: YabgMonad ()
yabgCopyDirs = do stngs <- asks settings
                  liftIO $ forM_ ( dirsToCopy stngs ) $ \ ( ogDir, nwDir ) -> do
                        copyDir ogDir ( dstPath stngs </> nwDir )

postDirectory :: YabgMonad ()
postDirectory =
    do src <- asks ( srcPath . settings )
       postNames <- liftIO $ find always ( extension ==? ".pst" ) src
       postMetas <- forM postNames ( readPostMeta src )
       local ( \x -> x{ posts = postMetas } ) $
           forM_ postMetas postPipeline

yabgPipeline :: YabgMonad ()
yabgPipeline = do
    yabgCopyDirs
    local ( \c -> let s = settings c
                   in c{ settings = s{ srcPath = srcPath s </> "pages"
                                     , dstPath = "bin" } } )
          postDirectory

---- Main ---------------------------------------------------------------------

data Argv = Argv
    { aRootPath :: String
    , aDev :: Bool
    }

argumentParser :: Parser Argv
argumentParser = Argv 
    <$> strOption ( long "rootpath" 
                 <> metavar "ROOTPATH" 
                 <> help "The rootpath, e.g. / or /~xkucerak" )
    <*> switch
          ( long "dev"
         <> short 'd'
         <> help "whether to run in development" )

argumentParserInfo :: ParserInfo Argv
argumentParserInfo = info ( argumentParser <**> helper )
    ( fullDesc 
    <> progDesc "Build a site using yet another blog generator." )

main :: IO ()
main = do print "yabg"
          args <- execParser argumentParserInfo
          let root = aRootPath args 
          runYabgMonad yabgPipeline
                       YabgSettings { srcPath = "xlogin"
                                    , dstPath = "bin"
                                    , rootPath = root
                                    , dev = aDev args
                                    , dirsToCopy = [ ( "xlogin/public", "public" ) 
                                                   , ( "xlogin/data", "" ) ]
                                    , defLinks = [ root </> "public/index.css" ]
                                    , nav = [ ( "xkucerak", root </> "" )
                                            , ( "uni", root </> "uni" )
                                            ] } []
