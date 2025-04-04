# TODO: Add comment
# 
# Author: fec
###############################################################################


library(R6)

MattoSettingsGlm <- R6Class("MattoSettingsGlm", 
    public = list(
      outerStartFold = 1,
      outerEndFold = 30,
      innerStartFold = 1,
      innerEndFold = 30,
      maxFeaturesInmodel = 8,#with eight features and no outer loop and a train/test split of 80/20, we have around 15 samples per feature
      baseModel = NULL,
      ensembleBaseModel = NULL,
      ensemble = TRUE,
      featureSetsAndReductionRules = NULL,
      featureSetParserMapping = NULL,
      csvParser = NULL,
      featureSets = NULL,
      featureDeterminationMethod = "recursive",#"takeNFeatures",#"exhaustive",
      volumeColName = "original_shape_MeshVolume",
      featureReductionContainerProvider = NULL,
      initialize = function(csvParser = NULL) {
        
        self$baseModel <- Model$new()
        ## self$baseModel$setEvaluationFunction("AUC")
        self$ensembleBaseModel <- GlmEnsembleModel$new()
        ## self$ensembleBaseModel$setEvaluationFunction("AUC")
        if (is.null(csvParser)) {
          self$csvParser = MattoCSVParserProgression$new()
        } else {
          self$csvParser = csvParser  
        }
        
        self$featureSets <- c("radiomics")
        self$featureReductionContainerProvider <- FeatureReductionContainerProvider$new()
        self$featureReductionContainerProvider$volumeColName <- self$volumeColName
        self$featureSetsAndReductionRules = list("radiomics" = self$featureReductionContainerProvider$radiomicsFeatureEliminationRulesMattoGlm)
        self$featureSetParserMapping = list("radiomics" = self$csvParser$getRadiomcFeaturesAsList)
      },
      getDataSplitterInnerLoop = function() {
        dataSplitter <- DataSplitter$new()
        #dataSplitter$setSampleFunction(dataSplitter$getIndexWithNoSampling)
        dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringPatIdsUnderSample)
        #dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringPatIds)
        return(dataSplitter)
      },
      getDataSplitterOuterLoop = function() {
        dataSplitter <- DataSplitter$new()
        dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringPatIdsUnderSample)
        #dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringPatIds)
        #dataSplitter$setSampleFunction(dataSplitter$splitByCenter)
        return(dataSplitter)
      }
    
    ),
    active = list(
        nuOfOuterFolds = function(value) {
          if (missing(value)) {
            self$outerEndFold - self$outerStartFold + 1
          } else {
            stop("`$nuOfFolds` is read only", call. = FALSE)
          }
        },
        nuOfInnerFolds = function(value) {
          if (missing(value)) {
            self$innerEndFold - self$innerStartFold + 1
          } else {
            stop("`$nuOfFolds` is read only", call. = FALSE)
          }
        }
    ),
    private = list(
    )
)

MattoSettingsLM <- R6Class(
    "MattoSettingsLM",
    inherit = MattoSettingsGlm,
    public = list(
        initialize = function(csvParser = NULL) {
          super$initialize(csvParser)
          self$baseModel <- LModel$new()
          self$ensembleBaseModel <- LEnsembleModel$new()
          
          self$featureSetsAndReductionRules = list("radiomics" = self$featureReductionContainerProvider$radiomicsFeatureEliminationRulesMattoLM)
        }
    )
)

MattoSettingsCox <- R6Class(
    "MattoSettingsCox",
    inherit = MattoSettingsGlm,
    public = list(
        initialize = function(csvParser = NULL) {
          super$initialize(csvParser)
          self$baseModel <- CoxModel$new()
          self$ensembleBaseModel <- CoxEnsembleModel$new()
          
          self$featureSetsAndReductionRules = list("radiomics" = self$featureReductionContainerProvider$radiomicsFeatureEliminationRulesMattoCox)
        },
        getDataSplitterInnerLoop = function() {
          dataSplitter <- DataSplitter$new(0.2)
          #dataSplitter$setSampleFunction(dataSplitter$getIndexWithNoSampling)
          dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringTime)
          return(dataSplitter)
        },
        getDataSplitterOuterLoop = function() {
          dataSplitter <- DataSplitter$new(0.2)
          dataSplitter$setSampleFunction(dataSplitter$getIndexForTrainTestSetConsideringTime)
          return(dataSplitter)
        }
    )
)

