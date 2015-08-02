--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend, mconcat)
import           Hakyll

--------------------------------------------------------------------------------

numRecentPosts :: Int
numRecentPosts = 2

configuration :: Configuration
configuration = defaultConfiguration {
  -- Store backend/temporary files out of sight
  storeDirectory = ".store",
  tmpDirectory = ".store/tmp",

  -- Where to read input files from
  providerDirectory = ".",

  -- Where to put output files
  destinationDirectory = "_site",

  -- How to deploy the site
  -- SSH config deals with identify file and aliasing to the proper server
  deployCommand = "rsync _site/* root@heytasha:/var/www/ --progress --delete --recursive"
}

main :: IO ()
main = hakyllWith configuration $ do
    match "images/favicons/*" $ do
        route (gsubRoute "images/favicons/" (const ""))
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "timeline/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "resume.pdf" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/default.scss" $ do
    route   $ setExtension "css"
    compile $ getResourceString >>=
        withItemBody (unixFilter "sass" ["-s", "--scss", "--trace"]) >>=
        return . fmap compressCss

    match (fromList ["about.markdown", "contact.markdown", "resume/resume.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        let postContext = mconcat 
                [ basePostContext
                , field "blogurl" $ \item -> return "archive.html"
                ]
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postContext
            >>= loadAndApplyTemplate "templates/default.html" postContext
            >>= relativizeUrls

    match "rafiki-posts/*" $ do
        route $ setExtension "html"
        let postContext = mconcat 
                [ basePostContext
                , field "blogurl" $ \item -> return "rafiki-cheesecake.html"
                ]
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postContext
            >>= loadAndApplyTemplate "templates/default.html" postContext
            >>= relativizeUrls

    match "error/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" basePostContext
            >>= relativizeUrls
          

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" basePostContext (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    create ["rafiki-cheesecake.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "rafiki-posts/*"
            let archiveCtx =
                    listField "posts" basePostContext (return posts) `mappend`
                    constField "title" "Rafiki Cheesecake Archives" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            -- All posts
            posts <- recentFirst =<< loadAll "posts/*"
            -- Most recent $n$ posts
            recentPosts <- fmap (take numRecentPosts) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" basePostContext (return posts) `mappend`
                    listField "recentPosts" basePostContext (return recentPosts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
basePostContext :: Context String
basePostContext =
    -- Comment out the below as it causes cyclic dependencies - will fix
    -- later I guess
    -- listField "recentPosts" defaultContext (fmap (take numRecentPosts) . recentFirst =<< loadAll "posts/*") `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
