#! /usr/bin/Rscript --vanilla

library(MuMIn)
library(survival)
library("survminer")

set.seed(7)

source("DataSplitter.R")
source("Outcome.R")
source("MattoCSVParser.R")
source("FeatureReduction.R")
source("Model.R")
source("PredefinedFeatureReductionRuleSequences.R")
source("MattoSettings.R")
source("ModelTrainer.R")
source("ModelSelectionInnerLoop.R")
source("TrackVariables.R")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1) {
  path <- args[1]
  
  
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
    stop("settings object is not defined", call. = FALSE)
  }
  
  
} else {
  stop("Wrong number of arguments.n", call. = FALSE)
}

mattoNestedCrossValidation = function(settings) {
  
  dataList <- settings$csvParser$readCSVFiles()
  
  featureSets <- list()
  for (featureSetName in settings$featureSets) {
    parser <- settings$featureSetParserMapping[[featureSetName]]
    featureSets[[featureSetName]] <- parser(dataList)
  }
  
  outcome <- settings$csvParser$getOutcome(dataList[[1]])
  
  
  dataForCorrelation <- featureSets[["radiomics"]][[1]]
  
  rhos <- abs(cor(dataForCorrelation, method = "spearman"))
  rhos[is.na(rhos)] = 0.0
  settings$featureReductionContainerProvider$setPrecalulatedFeatures("ClusterFeaturesWithPreComputedFeature", rhos)
  
  dataSplitter <- settings$getDataSplitterOuterLoop()
  
  variableTracker = TrackVariables$new(path)
  for (outerFoldNumber in seq_len(settings$nuOfOuterFolds)) {
    
    trainTestSetOutcome <- dataSplitter$sampleDataset(outcome)
    
    trainTestSetsSplits <- list()
    for (featureSetName in settings$featureSets) {
      trainTestSetsSplits[[featureSetName]] <- dataSplitter$getSubsetWithCurrentIdx(featureSets[[featureSetName]])
    }
    
    trainSets <- lapply(trainTestSetsSplits, `[[`, "trainSet")
    testSets <- lapply(trainTestSetsSplits, `[[`, "testSet")
    
    modelsPreprocessorsDatasets <- modelSelectionInnerLoop(trainTestSetOutcome$trainSet, settings, trainSets)
    if (settings$ensemble) 
    {
      selectedModel <- getEnsembleModel(modelsPreprocessorsDatasets$models, trainTestSetOutcome$trainSet, trainSets)
    } else {
      selectedModel <- evaluateAllModelsOnAllValidationSets(modelsPreprocessorsDatasets$models, modelsPreprocessorsDatasets$testTrainSets)
      selectedModel$trainModelOnNewData(trainTestSetOutcome$trainSet, trainSets)  
    }
    if (!is.null(selectedModel))
    {
      testAccuracy <- selectedModel$evaluateModelOnNewData(trainTestSetOutcome$testSet, testSets)
      print("TestAccuracy:")
      print(testAccuracy)
      variableTracker$trackVariable(testAccuracy)
      variableTracker$trackVariableWithName(selectedModel, "bestModels")
      # variableTracker$trackVariableWithName(selectedModel$getFeatureReductionContainers(), "preprocessors")
      variableTracker$trackVariableWithName(list(dataSplitter$getTestIdxs()), "testIdxs")
      variableTracker$trackVariableWithName(list(dataSplitter$getTrainIdxs()), "trainIdxs")
      # variableTracker$trackVariableWithName(modelsPreprocessorsDatasets$testTrainSets, "trainTestSetsInnerLoop")
      
      variableTracker$saveTrackedVariablesToRds()
    }
  }  
}
mattoNestedCrossValidation(settings)

