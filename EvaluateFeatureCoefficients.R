#! /usr/bin/Rscript --vanilla

library(MuMIn)
library(survival)
library("survminer")


source("DataSplitter.R")
source("Outcome.R")
source("FeatureReduction.R")
source("Model.R")
source("PredefinedFeatureReductionRuleSequences.R")
source("MattoSettings.R")
source("ModelTrainer.R")
source("ModelSelectionInnerLoop.R")
source("TrackVariables.R")
source("MattoCSVParser.R")
source("Preprocessors.R")


args <- commandArgs(trailingOnly = TRUE)
modelCoefficients <- list()
getModelCoefficients = function(coxModel) {
  coefNames <- names(coxModel$coefficients)
  hr <- exp(coxModel$coefficients)
  
  for (coefName in coefNames) {
    if (coefName %in% names(modelCoefficients)) {
      modelCoefficients[[coefName]] <<- append(modelCoefficients[[coefName]],as.vector(hr[coefName]))
    } else {
      modelCoefficients[coefName] <<- list(as.vector(hr[coefName]))
    }
  }
}

if (length(args) > 0) {
  path <- args[1]
} else {
  stop("Path to models must be supplied.n", call. = FALSE)
}

file <- file(file.path(path, "featureInfo.txt"), "w")
sink(file, type = "m")
sink(file, type = "o")
bestModelFiles <- list.files(path, "bestModels.rds",  recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
totalModels <- 0
for (bestModelFile in bestModelFiles) {
  bestModels <- readRDS(bestModelFile)
  
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
  featureSets <- NULL
  for (featureSetName in settings$featureSets) {
    parser <- settings$featureSetParserMapping[[featureSetName]]
    if (is.null(featureSets)) {
      featureSets <- parser(dataList)[[1]]
    }
    else {
      featureSets <- cbind(featureSets, parser(dataList)[[1]])
    }    
  }
  
  featCor <- abs(cor(featureSets, method = "spearman"))
  featDist <- as.dist(1 - featCor)
  featDist[is.na(featDist)] = 1.0
  featClusters <- hclust(featDist, method = "average")
  clusterIdxs <- cutree(featClusters, h = 1 - 0.8)
  
  message(bestModelFile)
  message(paste("Number of Models", length(bestModels)))
#  totalModels <- totalModels + length(bestModels)
  for (i in seq_len(length(bestModels))) {
    model <- bestModels[[i]]
    if (grepl("ensemble",tolower(class(model)[1])))
    {
      modelsInEnsemble <- model[[".__enclos_env__"]][["private"]][["models"]]
      for (modelInEnsemble in modelsInEnsemble)
      {
        coxModel <- modelInEnsemble[[".__enclos_env__"]][["private"]][["model"]]
        getModelCoefficients(coxModel)
        totalModels <- totalModels + 1
      }
    } else {
      coxModel <- model[[".__enclos_env__"]][["private"]][["model"]]
      getModelCoefficients(coxModel)
      totalModels <- totalModels + 1
    }
    
    
  }
}

message("############################")
message("############################")
message("############################")

for (coefName in names(modelCoefficients)) {
  hr <- modelCoefficients[[coefName]]
  if (coefName %in% names(clusterIdxs)) {
    clusterId <- clusterIdxs[[coefName]]
  } else {
    clusterId <- -1
  }
  message(paste(coefName, mean(hr), "+/-", sd(hr), "in", length(hr), "of", totalModels, "in cluster", clusterId))
}

sink(file = NULL, type = "m")
sink(file = NULL, type = "o")
close(file)

## newModel <- coxph(as.formula(cModel$getFormulaString()), data = trainData)
## sf <- survfit(newModel, newdata=testData)
## sf$cumhaz
## sf$surv
