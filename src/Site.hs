{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import Text.Blaze.Html
import Data.List

main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "*.html" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  let ctx = postCtx tags

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let tagsCtx =
            listField "posts" ctx (return posts)
              `mappend` constField "title" tag
              `mappend` constField "tag" tag
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

  create ["index.html", "posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" ctx (return posts)
              `mappend` constField "title" "posts"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/home.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["tags.html"] $ do
    route idRoute
    compile $ do
      let tagsCtx =
            listField "tags" ctx (getAllTags tags)
              `mappend` constField "title" "tags"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler
  where
    getAllTags ts = pure . map (mkItem . fst) $ tagsMap ts
      where
        mkItem :: String -> Item String
        mkItem t = Item (tagsMakeId ts t) t

postCtx :: Tags -> Context String
postCtx tags =
  tagsField' "tags" tags
    `mappend` dateField "date" "%Y-%m-%d"
    `mappend` defaultContext
    where
      tagsField' = tagsFieldWith getTags simpleRenderLink mconcat
      simpleRenderLink _   Nothing         = Nothing
      simpleRenderLink tag (Just filePath) = Just $
          H.a ! A.title (H.stringValue tag)
              ! A.href (toValue $ toUrl filePath)
              $ toHtml tag
