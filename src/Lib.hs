{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Lib
    ( plotGlueDags
    ) where

import Control.Lens
    ( (<&>)
    , view
    , Lens'
    )

import Control.Monad.IO.Class
    ( liftIO
    )

import Control.Monad.Trans.AWS
    ( Credentials(Discover)
    , newEnv
    , runResourceT
    , runAWST
    , send
    )

import Conduit
  ( (.|)
  , mapM_C
  , mapC
  , foldC
  , foldMapC
  , concatC
  , concatMapC
  , runConduit
  , filterC
  , mapCE
  , sinkList
  )

import Network.AWS.Glue
    ( getTriggers
    , gttrsTriggers
    , triName
    , Trigger
    , triActions
    , aJobName
    , triPredicate
    , Predicate
    , pConditions
    , cJobName
    )

import qualified Data.Text.Lazy as TL

import Network.AWS (paginate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (isPrefixOf, isSuffixOf, Text, stripSuffix, stripPrefix)
import Data.Functor (void)

import Data.GraphViz
    ( addExtension
    , defaultParams
    , GraphvizOutput(Png)
    , GraphvizCommand(..)
    , runGraphvizCommand
    , DotGraph
    , GraphvizParams
    , isDirected
    , globalAttributes
    , fmtEdge
    , GlobalAttributes(GraphAttrs, NodeAttrs)
    , nonClusteredParams
    , X11Color
    , X11Color(DarkGreen)
    , graphElemsToDot
    , runGraphviz
    )

import qualified Data.GraphViz.Attributes.Complete as AC
import qualified Data.GraphViz.Attributes as A

plotGlueDags :: FilePath -> Text -> Text -> IO ()
plotGlueDags output prefix suffix = do
    env <- newEnv Discover

    jobs <- runResourceT . runAWST env $ runConduit $ paginate getTriggers
        .| concatMapC (view gttrsTriggers)
        .| filterC (doesNameMatchPrefixAnsSuffix prefix suffix)
        .| concatMapC (toArcs normalizeName)
        .| sinkList

    liftIO $ plot output jobs

    where
        normalizeName name = fromMaybe "*" $ stripPrefix prefix name >>= stripSuffix suffix


doesNameMatchPrefixAnsSuffix :: Text -> Text -> Trigger -> Bool
doesNameMatchPrefixAnsSuffix suffix prefix trigger =
    let triggerName = extractName triName trigger
    in isPrefixOf suffix triggerName && isSuffixOf prefix triggerName

toArcs :: (Text -> Text) -> Trigger -> [(Text, Text, Text)]
toArcs normalizeName trigger =
    (,,) <$> predicateJobs <*> actionJobs <*> [""]
    where
        actionJobs = normalizeName. extractName aJobName <$> view triActions trigger
        predicateJobs = maybe [] (extractJobs normalizeName) $ view triPredicate trigger

extractJobs :: (Text -> Text) -> Predicate -> [Text]
extractJobs normalizeName predicate =
    view pConditions predicate <&> normalizeName . extractName cJobName

extractName :: Lens' s (Maybe Text) -> s -> Text
extractName getter =
    fromJust . view getter


plot :: FilePath -> [(Text, Text, Text)] -> IO ()
plot file edges =
    void $ runGraphviz (graphElemsToDot params [] edges) Png file
    where
        params :: GraphvizParams Text Text Text () Text
        params = defaultParams
            { globalAttributes =
                [ GraphAttrs
                    [ AC.RankDir AC.FromTop
                    ]
                , NodeAttrs
                    [ A.shape AC.BoxShape
                    , A.style A.rounded
                    ]
                ]
            }
