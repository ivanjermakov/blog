{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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

  postTags <- buildTags "posts/*" (fromCapture "tags/*.html")
  let postCtx = mkPostCtx postTags

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  draftTags <- buildTags "drafts/*" (fromCapture "tags/*.html")
  let draftCtx = mkPostCtx draftTags

  match "drafts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/draft.html" draftCtx
        >>= loadAndApplyTemplate "templates/default.html" draftCtx
        >>= relativizeUrls

  tagsRules draftTags $ \tag pattern -> do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let tagsCtx =
            listField "posts" postCtx (return posts)
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
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "posts"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["drafts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "drafts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "drafts"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/drafts.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  create ["tags.html"] $ do
    route idRoute
    compile $ do
      let tagsCtx =
            listField "tags" postCtx (getAllTags postTags)
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

mkPostCtx :: Tags -> Context String
mkPostCtx tags =
  tagsField' "tags" tags
    `mappend` dateField "date" "%Y-%m-%d"
    `mappend` defaultContext
  where
    tagsField' = tagsFieldWith getTags simpleRenderLink mconcat
    simpleRenderLink _ Nothing = Nothing
    simpleRenderLink tag (Just filePath) =
      Just $
        H.a ! A.title (H.stringValue tag)
          ! A.href (toValue $ toUrl filePath)
          $ toHtml tag
