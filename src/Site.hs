{-# LANGUAGE OverloadedStrings #-}

import Data.List (isPrefixOf)
import Hakyll
import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc

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
  let postCtx = mkCtxWithTags postTags
  buildPostPages postCtx
  buildPostsPage postCtx

  allTags <- buildTags (fromGlob "posts/*" .||. fromGlob "drafts/*") (fromCapture "tags/*.html")
  let draftCtx = mkCtxWithTags allTags
  buildDraftPages draftCtx
  buildDraftsPage draftCtx

  buildTagPages allTags postCtx
  -- we don't want to show tags that are only in drafts on the tag list page
  buildTagsPage postTags postCtx

  match "templates/*" $ compile templateBodyCompiler

mkCtxWithTags :: Tags -> Context String
mkCtxWithTags tags =
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

buildPostPages :: Context String -> Rules ()
buildPostPages ctx = do
  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

buildPostsPage :: Context String -> Rules ()
buildPostsPage ctx = do
  create ["index.html", "posts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" ctx (return posts)
              `mappend` constField "title" "posts"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

buildDraftPages :: Context String -> Rules ()
buildDraftPages ctx = do
  match "drafts/*" $ do
    route $ setExtension "html"
    compile $
      customPandocCompiler
        >>= loadAndApplyTemplate "templates/draft.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

buildDraftsPage :: Context String -> Rules ()
buildDraftsPage ctx = do
  create ["drafts.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "drafts/*"
      let indexCtx =
            listField "posts" ctx (return posts)
              `mappend` constField "title" "drafts"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/drafts.html" indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

buildTagPages :: Tags -> Context String -> Rules ()
buildTagPages tags ctx = do
  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      taggedPages <- loadAll pattern
      -- filter out draft tags from tagged pages
      posts <- recentFirst . filter (\it -> "posts" `isPrefixOf` (toFilePath . itemIdentifier $ it)) $ taggedPages
      let tagsCtx =
            listField "posts" ctx (return posts)
              `mappend` constField "title" tag
              `mappend` constField "tag" tag
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls

buildTagsPage :: Tags -> Context String -> Rules ()
buildTagsPage tags ctx = do
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
  where
    getAllTags ts = pure . map (mkItem . fst) $ tagsMap ts
      where
        mkItem :: String -> Item String
        mkItem t = Item (tagsMakeId ts t) t

customPandocCompiler :: Compiler (Item String)
customPandocCompiler = pandocCompilerWith customReaderOptions defaultHakyllWriterOptions
  where
    customReaderOptions =
      def
        { readerExtensions =
            disableExtension Ext_smart
              . disableExtension Ext_blank_before_blockquote
              $ (readerExtensions defaultHakyllReaderOptions)
                <> extensionsFromList
                  [ Ext_pipe_tables,
                    Ext_raw_html,
                    Ext_native_divs,
                    Ext_auto_identifiers,
                    Ext_gfm_auto_identifiers,
                    Ext_autolink_bare_uris,
                    Ext_strikeout,
                    Ext_task_lists,
                    Ext_emoji,
                    Ext_fenced_code_blocks,
                    Ext_backtick_code_blocks,
                    Ext_tex_math_dollars,
                    Ext_tex_math_single_backslash,
                    Ext_tex_math_double_backslash,
                    Ext_implicit_header_references,
                    Ext_implicit_figures,
                    Ext_abbreviations,
                    Ext_fenced_divs
                  ]
        }
