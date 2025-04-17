# TODO: Add comment
# 
# Author: fec
###############################################################################

FeatureReductionContainerProvider <- R6Class("FeatureReductionContainerProvider",
    public = list(
        volumeColName = NULL,
        preCalulcatedFeatures = list(),
        initialize = function() {
        },
        setPrecalulatedFeatures = function(ruleName, features) {
          self$preCalulcatedFeatures[ruleName] <- list(features)
        },
        radiomicsFeatureEliminationRulesMattoPolyLM = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          normalizer <- NormalizeFeaturesrule$new()
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          univariateFeatureReduction <- UnivariateFeatureReductionRuleGlm$new(pValueThreshold = 1.0)
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          clusterAnalysis$setPrecomputedCorrelations(self$preCalulcatedFeatures["ClusterFeaturesWithPreComputedFeature"][[1]])
          lassoRegressionAnalysis <- LassoFeatureReduction$new()
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          featReductionContainer$addRule(normalizer, "NormalizeFeaturesrule")
          featReductionContainer$addRule(keepFeaturesNoVolumeCorrelation, "RemoveFeaturesThatCorrelateWithVolume")
          featReductionContainer$addRule(univariateFeatureReduction,"UnivariateFeatureReductionRulePolyLM")
          featReductionContainer$addRule(clusterAnalysis, "ClusterFeaturesWithPreComputedFeature")
          featReductionContainer$addRule(lassoRegressionAnalysis, "LassoFeatureReduction")
          
          return(featReductionContainer)
          
        },
        radiomicsFeatureEliminationRulesMattoLM = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          normalizer <- NormalizeFeaturesrule$new()
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          univariateFeatureReduction <- UnivariateFeatureReductionRuleLM$new()
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          clusterAnalysis$setPrecomputedCorrelations(self$preCalulcatedFeatures["ClusterFeaturesWithPreComputedFeature"][[1]])
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          featReductionContainer$addRule(normalizer, "NormalizeFeaturesrule")
          featReductionContainer$addRule(keepFeaturesNoVolumeCorrelation, "RemoveFeaturesThatCorrelateWithVolume")
          featReductionContainer$addRule(univariateFeatureReduction,"UnivariateFeatureReductionRuleLM")
          featReductionContainer$addRule(clusterAnalysis, "ClusterFeaturesWithPreComputedFeature")
          
          return(featReductionContainer)
          
        },
        radiomicsFeatureEliminationRulesMattoCox = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          normalizer <- NormalizeFeaturesrule$new()
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          univariateFeatureReduction <- UnivariateFeatureReductionRuleCox$new()
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          clusterAnalysis$setPrecomputedCorrelations(self$preCalulcatedFeatures["ClusterFeaturesWithPreComputedFeature"][[1]])
          
          sortFeaturesWithPreComputedFeature <- SortFeaturesWithPreComputedFeature$new()
          sortFeaturesWithPreComputedFeature$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          featReductionContainer$addRule(normalizer, "NormalizeFeaturesrule")
          featReductionContainer$addRule(keepFeaturesNoVolumeCorrelation, "RemoveFeaturesThatCorrelateWithVolume")
          featReductionContainer$addRule(univariateFeatureReduction,"UnivariateFeatureReductionRuleCox")
          featReductionContainer$addRule(clusterAnalysis, "ClusterFeaturesWithPreComputedFeature")
          featReductionContainer$addRule(sortFeaturesWithPreComputedFeature, "SortFeaturesWithPreComputedFeature")
          
          return(featReductionContainer)
          
        },
        radiomicsFeatureEliminationRulesMattoGlm = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          normalizer <- NormalizeFeaturesrule$new()
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          univariateFeatureReduction <- UnivariateFeatureReductionRuleGlm$new()
          infoGainFeatureCalculation <- InformationGainReduction$new()
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(infoGainFeatureCalculation$getCalculatedFeatures)
          clusterAnalysis$setPrecomputedCorrelations(self$preCalulcatedFeatures["ClusterFeaturesWithPreComputedFeature"][[1]])
          
          sortFeaturesWithPreComputedFeature <- SortFeaturesWithPreComputedFeature$new()
          sortFeaturesWithPreComputedFeature$setPrecomputedFeaturesProvider(infoGainFeatureCalculation$getCalculatedFeatures)
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          featReductionContainer$addRule(normalizer, "NormalizeFeaturesrule")
          featReductionContainer$addRule(keepFeaturesNoVolumeCorrelation, "RemoveFeaturesThatCorrelateWithVolume")
          featReductionContainer$addRule(univariateFeatureReduction,"UnivariateFeatureReductionRuleGlm")
          featReductionContainer$addRule(infoGainFeatureCalculation,"InformationGainReduction")
          featReductionContainer$addRule(clusterAnalysis, "ClusterFeaturesWithPreComputedFeature")
          featReductionContainer$addRule(sortFeaturesWithPreComputedFeature, "SortFeaturesWithPreComputedFeature")
          return(featReductionContainer)
          
        },
        radiomicsFeatureEliminationRulesMattoPolyGlm = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          normalizer <- NormalizeFeaturesrule$new()
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          univariateFeatureReduction <- UnivariateFeatureReductionRulePolyGlm$new()
          #univariateFeatureReduction <- UnivariateFeatureReductionRuleGlm$new()
          infoGainFeatureCalculation <- InformationGainReduction$new()
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(infoGainFeatureCalculation$getCalculatedFeatures)
          clusterAnalysis$setPrecomputedCorrelations(self$preCalulcatedFeatures["ClusterFeaturesWithPreComputedFeature"][[1]])
          
          sortFeaturesWithPreComputedFeature <- SortFeaturesWithPreComputedFeature$new()
          sortFeaturesWithPreComputedFeature$setPrecomputedFeaturesProvider(infoGainFeatureCalculation$getCalculatedFeatures)
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          featReductionContainer$addRule(normalizer, "NormalizeFeaturesrule")
          featReductionContainer$addRule(keepFeaturesNoVolumeCorrelation, "RemoveFeaturesThatCorrelateWithVolume")
          featReductionContainer$addRule(univariateFeatureReduction,"UnivariateFeatureReductionRulePolyGlm")
          featReductionContainer$addRule(infoGainFeatureCalculation,"InformationGainReduction")
          featReductionContainer$addRule(clusterAnalysis, "ClusterFeaturesWithPreComputedFeature")
          featReductionContainer$addRule(sortFeaturesWithPreComputedFeature, "SortFeaturesWithPreComputedFeature")
          return(featReductionContainer)
        },
        radiomicsFeatureEliminationRules = function(outcomeTrainSet, radiomicFeaturesTrainSets) {
          ## define Rules to apply
          centerAgnosticRule <- CenterAgnosticFeatures$new(0.001)# disabled because all features fulfill this rule
          #classWiseDistribution <- InspectClassWiseDistribution$new(0.1)
          #centerWiseNormlisation <- CenterSpecifiNormalisation$new()
          keepFeaturesNoVolumeCorrelation <- RemoveFeaturesThatCorrelateWithVolume$new()
          keepFeaturesNoVolumeCorrelation$setVolumeColName(self$volumeColName)
          keepComparableFeatureRule <- KeepComparableFeatures$new(0.05, radiomicFeaturesTrainSets[-1])
          normalizer <- NormalizeFeaturesrule$new()
          univariateFeatureReduction <- UnivariateFeatureReductionRuleCox$new()
          #clusterAnalysis <- ClusterFeatures$new()
          clusterAnalysis <- ClusterFeaturesWithPreComputedFeature$new()
          clusterAnalysis$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          
          sortFeaturesWithPreComputedFeature <- SortFeaturesWithPreComputedFeature$new()
          sortFeaturesWithPreComputedFeature$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, radiomicFeaturesTrainSets[[1]])
          #featReductionContainer$addRule(classWiseDistribution)
          #featReductionContainer$addRule(centerAgnosticRule)
          featReductionContainer$addRule(keepComparableFeatureRule)
          featReductionContainer$addRule(normalizer)
          #featReductionContainer$addRule(centerWiseNormlisation)
          featReductionContainer$addRule(univariateFeatureReduction)
          featReductionContainer$addRule(clusterAnalysis)
          featReductionContainer$addRule(sortFeaturesWithPreComputedFeature)
          
          return(featReductionContainer)
          
        },
        clinicalFeatureEliminationRules = function(outcomeTrainSet, clinicalTrainSet, clinicalTestSet) {
          normalizer <- NormalizeFeaturesrule$new()
          univariateFeatureReduction <- UnivariateFeatureReductionRuleCox$new()
          
          sortFeaturesWithPreComputedFeature <- SortFeaturesWithPreComputedFeature$new()
          sortFeaturesWithPreComputedFeature$setPrecomputedFeaturesProvider(univariateFeatureReduction$getCalculatedFeatures)
          
          featReductionContainer <- FeatureReductionContainer$new(outcomeTrainSet, clinicalTrainSet)
          featReductionContainer$addRule(normalizer)
          featReductionContainer$addRule(univariateFeatureReduction)
          featReductionContainer$addRule(sortFeaturesWithPreComputedFeature)
          return(featReductionContainer)
        }
    )
)



