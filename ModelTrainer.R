# TODO: Add comment
#
# Author: fec
###############################################################################

library(R6)
library(foreach)
library(doParallel)

ModelTrainer <- R6Class("ModelTrainer",
  public = list(
    initialize = function(model, trainingOutcome, trainingData, validationOutcome, validationData, maxFeaturesInmodel, featureSearchMethod = "exhaustive") {
      private$model <- model
      private$trainingOutcome <- trainingOutcome
      private$trainingData <- trainingData
      private$validationOutcome <- validationOutcome
      private$validationData <- validationData
      private$maxFeaturesInmodel <- maxFeaturesInmodel
      if (featureSearchMethod == "exhaustive") {
        private$featureDeterminationMethod <- private$findBestFeatureCombinationExhaustiveParallel
      } else if (featureSearchMethod == "takeNFeatures") {
        private$featureDeterminationMethod <- private$takeNFeatures
        }else {
        private$featureDeterminationMethod <- private$findBestFeatureCombinationCoxRecursive
      }
    },
    findBestFeatureCombination = function() {
      dataFrameForModelFitting <- private$model$bindData(private$trainingOutcome, private$trainingData)
      private$featureDeterminationMethod(NULL, dataFrameForModelFitting)
      if (private$model$getNumberOfFields() > 0) {
            private$model$trainModel(dataFrameForModelFitting)#train again so that model and model features are in accordance
      }
    },
    evaluate = function(testingOutcome, testingData) {
      testSetPerformance <- private$model$evaluate(cbind(testingOutcome, testingData))
      return(testSetPerformance)
    },
    getModel = function() {
      return(private$model)
    }
  ),
  private = list(
    model = NULL,
    trainingOutcome = NULL,
    trainingData = NULL,
    validationOutcome = NULL,
    validationData = NULL,
    maxFeaturesInmodel = NULL,
    featureDeterminationMethod = NULL,
    takeNFeatures = function(perFormanceToBeat, dataFrameForModelFitting) {
      featureToTake <- ""
      for (i in seq_len(private$maxFeaturesInmodel)) {
        featureName <- colnames(private$trainingData)[i]
        private$model$addFieldToFormula(featureName)
        successfullyTrained <- tryCatch(
          {
            private$model$trainModel(dataFrameForModelFitting)#train again so that model and model features are in accordance
            TRUE
          },
          error=function(e) {
            return(FALSE)
          }
        )
        if (!successfullyTrained) {
          private$model$popFieldFromFormula()
          break
        }
      }
    },
    findBestFeatureCombinationCoxRecursive = function(perFormanceToBeat, dataFrameForModelFitting) {
      featureToTake <- ""
      for (i in seq_len(ncol(private$trainingData))) {
        featureName <- colnames(private$trainingData)[i]
        private$model$addFieldToFormula(featureName)
        
        maxCoefsPvalue <- tryCatch(
          {
            private$model$trainModel(dataFrameForModelFitting)
            maxCoefsPvalue <- max(private$model$getPValuesOfFittedModel())
            maxCoefsPvalue
          },
          error=function(e) {
            return(1.0)
          }
        )
        

        

        if (!is.na(maxCoefsPvalue) & maxCoefsPvalue <= 0.1) {
          validationSetPerformance <- private$model$evaluate(private$model$bindData(private$validationOutcome, private$validationData))
          trainSetPerformance <- private$model$evaluate(dataFrameForModelFitting)

          curPerformance <- validationSetPerformance# + trainSetPerformance
          if (is.null(perFormanceToBeat) || private$model$comparePerformanceValues(curPerformance,perFormanceToBeat)) {
            perFormanceToBeat <- curPerformance
            featureToTake <- featureName
          }

        }

        private$model$popFieldFromFormula()
      }

      if (featureToTake != "") {
        addedFeatures <- private$model$addFieldToFormula(featureToTake)
        if (addedFeatures < private$maxFeaturesInmodel) {
          featuresToTake <- private$findBestFeatureCombinationCoxRecursive(perFormanceToBeat, dataFrameForModelFitting)
        }
      }
    },
    findBestFeatureCombinationExhaustive = function(perFormanceToBeat, dataFrameForModelFitting) {
      
      numberofFeatures = ncol(private$trainingData)
      
      maxNumberOfFeatures = min(private$maxFeaturesInmodel,numberofFeatures)
      featuresToTake = ""
      for (i in 1:maxNumberOfFeatures)
      {
        featureCombinations <- combn(numberofFeatures,i)
        
        for (featureCombinationCol in 1:ncol(featureCombinations))
        {
          featureColIdx = featureCombinations[,featureCombinationCol]
          
          featureNames <- colnames(private$trainingData)[featureColIdx]
          for (featName in featureNames) {
            private$model$addFieldToFormula(featName)
          }
          
          maxCoefsPvalue <- tryCatch(
            {
              private$model$trainModel(dataFrameForModelFitting)
              maxCoefsPvalue <- max(private$model$getPValuesOfFittedModel())
              return(maxCoefsPvalue)
            },
            error=function(e) {
              return(1.0)
            }
          )          
          
          if (!is.na(maxCoefsPvalue) & maxCoefsPvalue <= 0.05) {
            validationSetPerformance <- private$model$evaluate(private$model$bindData(private$validationOutcome, private$validationData))
            trainSetPerformance <- private$model$evaluate(dataFrameForModelFitting)
            
            curPerformance <- validationSetPerformance + trainSetPerformance
            if (is.null(perFormanceToBeat) || private$model$comparePerformanceValues(curPerformance,perFormanceToBeat)) {
              perFormanceToBeat <- curPerformance
              featuresToTake <- featureNames
            }
            
          }
          for (featName in featureNames) {
            private$model$popFieldFromFormula()
          }
        }
      }
      if (featuresToTake != "") {
        for (featName in featuresToTake) {
          private$model$addFieldToFormula(featName)
        }
      }
    },
    findBestFeatureCombinationExhaustiveParallel = function(perFormanceToBeat, dataFrameForModelFitting) {
      
      numberofFeatures = ncol(private$trainingData)
      
      maxNumberOfFeatures = min(private$maxFeaturesInmodel,numberofFeatures)
      featuresToTake = ""
      
      cores=detectCores()
      cl <- makeCluster(cores[1]-1) #not to overload your computer
      registerDoParallel(cl)
      
      currModel <- private$model$clone(deep=TRUE)
      currTrainingData <- private$trainingData
      currValidationOutcome <- private$validationOutcome
      currValidationData <- private$validationData
      for (i in 1:maxNumberOfFeatures)
      {
        featureCombinations <- combn(numberofFeatures,i)
        featCombinationPerformance <- foreach(featureCombinationCol = 1:ncol(featureCombinations), .combine=c, .packages="survival") %dopar% {
          model = currModel$clone(deep=TRUE)
          featureColIdx = featureCombinations[,featureCombinationCol]
          curPerformance = 0.0
          featureNames <- colnames(currTrainingData)[featureColIdx]
          for (featName in featureNames) {
            model$addFieldToFormula(featName)
          }
          
          model$trainModel(dataFrameForModelFitting)
          
          maxCoefsPvalue <- max(model$getPValuesOfFittedModel())
          
          if (!is.na(maxCoefsPvalue) & maxCoefsPvalue <= 0.05) {
            validationSetPerformance <- model$evaluate(model$bindData(currValidationOutcome, currValidationData))
            trainSetPerformance <- model$evaluate(dataFrameForModelFitting)
            
            curPerformance <- validationSetPerformance + trainSetPerformance
            
          }
          paramNames <- model$getModelParamNames()
          retlist = list()
          retlist[paramNames] = curPerformance
          retlist
        }
        bestFeatureCombination <- which.max(featCombinationPerformance)
        bestFeatureCombinationFields <- names(bestFeatureCombination)
        if (is.null(perFormanceToBeat) || private$model$comparePerformanceValues(bestFeatureCombination,perFormanceToBeat)) {
          perFormanceToBeat <- bestFeatureCombination
          featuresToTake <- bestFeatureCombinationFields
        }
      }
      
      stopCluster(cl)
      if (featuresToTake != "") {
        if (grepl("+",featuresToTake,fixed=TRUE)) {
          featuresToTake <- strsplit(featuresToTake, "+", ,fixed=TRUE)[[1]]
        }
        for (featName in featuresToTake) {
          private$model$addFieldToFormula(featName)
        }
      }
    }
  )
)
