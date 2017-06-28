--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/posts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ version "preview" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= (\(Item _ str) -> makeItem . take100Words $ str)
            >>= loadAndApplyTemplate "templates/post-preview.html"    postPreviewCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "preview")
            let indexCtx =
                    listField "posts" postPreviewCtx (return posts) <>
                    constField "title" "Статьи"                     <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%F" <>
    defaultContext

postPreviewCtx :: Context String
postPreviewCtx = 
    bodyField "postBody" <>
    postCtx

take100Words :: String -> String
-- take100Words = ((++ " [...]") . unwords . take 100 . words)
take100Words = unwords . take 100 . words