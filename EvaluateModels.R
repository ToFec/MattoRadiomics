#! /usr/bin/Rscript --vanilla

library(MuMIn)
library(survival)
library("survminer")


source("DataSplitter.R")
source("Outcome.R")
source("FeatureReduction.R")
source("Model.R")
source("PredefinedFeatureReductionRuleSequences.R")
source("ModelTrainer.R")
source("ModelSelectionInnerLoop.R")
source("TrackVariables.R")
source("MattoSettings.R")
source("MattoCSVParser.R")
source("Preprocessors.R")
source("EvaluationFunctions.R")


args <- commandArgs(trailingOnly = TRUE)



if (length(args) > 0) {
  path <- args[1]
} else {
  stop("Path to models must be supplied.n", call. = FALSE)
}

file <- file(file.path(path, "info.txt"), "w")
sink(file, type = "m")
sink(file, type = "o")
testAccuracies <- readRDS(file.path(path, "testAccuracy.rds"))
trainIdxs <- NULL
if (file.exists(file.path(path, "trainIdxs.rds"))) {
  trainIdxs <- readRDS(file.path(path, "trainIdxs.rds"))
}
testIdxs <- NULL
if (file.exists(file.path(path, "testIdxs.rds"))) {
  testIdxs <- readRDS(file.path(path, "testIdxs.rds"))
}
innerLoopTrainTestIdxs <- NULL
if (file.exists(file.path(path, "trainTestSetsInnerLoop.rds"))) {
  innerLoopTrainTestIdxs <- readRDS(file.path(path, "trainTestSetsInnerLoop.rds"))
}

bestModels <- readRDS(file.path(path, "bestModels.rds"))
model <- bestModels[[1]]

if (length(args) > 1) {
  settingsObjectName <- args[2]
  if (exists(settingsObjectName, mode="environment")) {
    
    csvParser <- NULL
    if (length(args) > 2) {
      dataSetName <- args[3]
      if (exists(dataSetName, mode="environment")) {
        csvParser <- get(dataSetName)$new()
      }
    }
    
    settings <- get(settingsObjectName)$new(csvParser)
  } else {
    stop("settings object is not correctly defined", call. = FALSE)
  }
  
  
} else {
  stop("Wrong number of arguments.n", call. = FALSE)
}



settings$featureSets <- model$getFeatureSets()


dataList <- settings$csvParser$readCSVFiles()

featureSets <- list()
for (featureSetName in settings$featureSets) {
  parser <- settings$featureSetParserMapping[[featureSetName]]
  featureSets[[featureSetName]] <- parser(dataList)
}

outcome <- settings$csvParser$getOutcome(dataList[[1]])
message(path)
message(paste("mean tesetaccuracy:", mean(testAccuracies), "+/-", sd(testAccuracies)))
message(toString(testAccuracies))
dataSplitter <- settings$getDataSplitterOuterLoop()
calculatedTestAccuracies <- c()
figuresForModels <- c()
for (i in seq_len(length(bestModels))) {
  message("############################")
  message("############################")
  message("############################")
  model <- bestModels[[i]]
  evalFunctionCalculator <- getEvaluationFunctionObject(model)
  if (!is.null(testIdxs)) {
    testIdx <- testIdxs[[i]]
    trainIdx <- trainIdxs[[i]]
    dataSplitter$setTestIdxs(testIdx)
    dataSplitter$setTrainIdxs(trainIdx)
    
    outcomeSubsets <- dataSplitter$getOutcomeSubsetWithCurrentIdx(outcome)
    
    message(paste("Fold", i, "eventRatio in train set", sum(outcomeSubsets$trainSet$status) / length(outcomeSubsets$trainSet$status)))
    message(paste("Fold", i, "eventRatio in test set", sum(outcomeSubsets$testSet$status) / length(outcomeSubsets$testSet$status)))
    idsInSubset <- outcomeSubsets$testSet$ids[outcomeSubsets$testSet$status == 1]
    message(paste("Size of train set: ", length(outcomeSubsets$trainSet$ids)))
    message(paste("Ids in train set: ", toString(outcomeSubsets$trainSet$ids)))
    message(paste("Size of test set: ", length(outcomeSubsets$testSet$ids)))
    message(paste("Ids in test set: ", toString(outcomeSubsets$testSet$ids)))
    message(paste("Ids with event in test set: ", toString(idsInSubset)))
    
    
    
    trainTestSetsSplits <- list()
    for (featureSetName in settings$featureSets) {
      trainTestSetsSplits[[featureSetName]] <- dataSplitter$getSubsetWithCurrentIdx(featureSets[[featureSetName]])
    }
    
    trainSets <- lapply(trainTestSetsSplits, `[[`, "trainSet")
    testSets <- lapply(trainTestSetsSplits, `[[`, "testSet")
    
    model$trainModelOnNewData(outcomeSubsets$trainSet, trainSets)
    #model$setEvaluationFunction("AUC")
    trainAccuracy <- model$evaluateModelOnNewData(outcomeSubsets$trainSet, trainSets)
    testAccuracy <- model$evaluateModelOnNewData(outcomeSubsets$testSet, testSets)
    
    predictions <- model$modelPredictionOnNewData(outcomeSubsets$testSet, testSets)
    
    evalFunctionCalculator$setOutcome(outcomeSubsets$testSet)
    evalFunctionCalculator$setPrediction(predictions)

    
  } else {
    trainAccuracy <- model$evaluateModelOnNewData(outcome, featureSets)
    testAccuracy <- 0.0
  }
  
  message(paste("Model Fold", i))
  model$printModelSummary()
  
  trainAccuraciesInnerLoops <- c()
  testAccuraciesInnerLoops <- c()
  if (!is.null(innerLoopTrainTestIdxs)) {
    numberOfInnerLoops = length(innerLoopTrainTestIdxs) / length(bestModels)
    for (innerLoopNr in 1:numberOfInnerLoops) {
      innerLoopData = innerLoopTrainTestIdxs[[((i-1)*numberOfInnerLoops) + innerLoopNr]]
      if (!is.null(innerLoopData)) {
        model$trainModelOnNewData(innerLoopData$outcomeTrain, innerLoopData$featuresTrain)
        trainAccuracyInnerLoop <- model$evaluateModelOnNewData(innerLoopData$outcomeTrain, innerLoopData$featuresTrain)
        trainAccuraciesInnerLoops <- append(trainAccuraciesInnerLoops, trainAccuracyInnerLoop)
        testAccuracyInnerLoop <- model$evaluateModelOnNewData(innerLoopData$outcomeTest, innerLoopData$featuresTest)
        testAccuraciesInnerLoops  <- append(testAccuraciesInnerLoops, testAccuracyInnerLoop)
      }
      
    }
  }

  evalFunctionCalculator$calculateFigures()
  figuresForModel <- evalFunctionCalculator$getFigures()
  figuresForModels[[length(figuresForModels) + 1]] <- figuresForModel

  message(paste("Train accuracy:", trainAccuracy))
  message(paste("Test accuracy:", testAccuracy))
  
  message(paste("Train accuracy inner loops:", toString(trainAccuraciesInnerLoops)))
  message(paste("Test accuracy inner loops:", toString(testAccuraciesInnerLoops)))
  calculatedTestAccuracies <- append(calculatedTestAccuracies, testAccuracy)
}
message("############################")
message("############################")
message("############################")
message(paste("mean calculated tesetaccuracy:", mean(calculatedTestAccuracies), "+/-", sd(calculatedTestAccuracies)))
message(paste("values: ", toString(calculatedTestAccuracies)))

if (length(figuresForModels) > 0) {
  figureNames <- names(figuresForModels[[1]])
  for (figureName in figureNames) {
    figureValues <- unlist(lapply(figuresForModels, function(x) x[[figureName]]))
    message("############################")
    message(paste("mean", figureName, ":", mean(figureValues), "+/-", sd(figureValues)))
    message(paste("values: ", toString(figureValues)))
  }
}

sink(file = NULL, type = "m")
sink(file = NULL, type = "o")
close(file)

## newModel <- coxph(as.formula(cModel$getFormulaString()), data = trainData)
## sf <- survfit(newModel, newdata=testData)
## sf$cumhaz
## sf$surv
