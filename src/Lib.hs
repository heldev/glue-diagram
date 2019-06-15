{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Lib
    ( listAllTriggers2
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
import Data.Graph.DGraph (DGraph, fromArcsList, arcs)
import Data.Graph.Types ((-->), Arc(..),  Graph, vertices)
import Data.Graph.Visualize (plotDGraphPng)
import Data.Functor (void)

import Data.GraphViz
    ( addExtension
    , GraphvizOutput(Png)
    , GraphvizCommand(..)
    , runGraphvizCommand
    , DotGraph
    , GraphvizParams
    , isDirected
    , globalAttributes
    , fmtEdge
    , GlobalAttributes(GraphAttrs, EdgeAttrs)
    , nonClusteredParams, X11Color, X11Color(DarkGreen), graphElemsToDot
    )

--import Data.GraphViz.Attributes.Complete (Overlap(ScaleOverlaps), Overlap, Attribute(FontColor), Label, Label(..))
import Data.GraphViz.Attributes.Complete

listAllTriggers :: IO ()
listAllTriggers =
    do
        env <- newEnv Discover
        runResourceT . runAWST env $ do
            triggers <- send getTriggers <&> view gttrsTriggers
            liftIO $ mapM_ print triggers

listAllTriggers2 :: IO ()
listAllTriggers2 = do
    env <- newEnv Discover

    jobs <- runResourceT . runAWST env $ runConduit $ paginate getTriggers
        .| concatMapC (view gttrsTriggers)
        .| filterC (doesNameMatchPrefixAnsSuffix "appliances-reliability-" "-development")
        .| concatMapC (toArcs normalizeName)
        .| sinkList

    liftIO $ plot "appliances-reliability-development" jobs

    where
        normalizeName name = fromMaybe "*" $ stripPrefix "appliances-reliability-" name >>= stripSuffix "-development"


doesNameMatchPrefixAnsSuffix :: Text -> Text -> Trigger -> Bool
doesNameMatchPrefixAnsSuffix suffix prefix trigger =
    let triggerName = extractName triName trigger
    in isPrefixOf suffix triggerName && isSuffixOf prefix triggerName

toArcs :: (Text -> Text) -> Trigger -> [Arc Text ()]
toArcs normalizeName trigger =
    (-->) <$> predicateJobs <*> actionJobs
    where
        actionJobs = normalizeName. extractName aJobName <$> view triActions trigger
        predicateJobs = maybe [] (extractJobs normalizeName) $ view triPredicate trigger

extractJobs :: (Text -> Text) -> Predicate -> [Text]
extractJobs normalizeName predicate =
    view pConditions predicate <&> normalizeName . extractName cJobName

plot :: FilePath -> [Arc Text ()] -> IO ()
plot file =
    void . flip plotDGraphMy file . fromArcsList

extractName :: Lens' s (Maybe Text) -> s -> Text
extractName getter =
    fromJust . view getter

plotDGraphMy :: DGraph Text () -> FilePath -> IO FilePath
plotDGraphMy g = addExtension (runGraphvizCommand Dot $ toDirectedDot False g) Png

toDirectedDot :: Bool
 -> DGraph Text ()
 -> DotGraph Text
toDirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
    where params = sensibleDotParams True labelEdges

labeledNodes :: (Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = (\v -> (v, show v)) <$> vertices g

labeledArcs :: (Show e) => DGraph Text e -> [(Text, Text, String)]
labeledArcs g = (\(Arc v1 v2 attr) -> (v1, v2, show attr)) <$> arcs g

sensibleDotParams
 :: Bool -- ^ Directed
 -> Bool -- ^ Label edges
 -> GraphvizParams t l String () l
sensibleDotParams directed edgeLabeled =
  nonClusteredParams
    { isDirected = directed
    , globalAttributes = [GraphAttrs [Overlap ScaleOverlaps], EdgeAttrs [FontColor (X11Color DarkGreen)]]
    , fmtEdge = edgeFmt
    }
  where
    edgeFmt (_, _, l) = [Label $ StrLabel $ TL.pack l | edgeLabeled]
