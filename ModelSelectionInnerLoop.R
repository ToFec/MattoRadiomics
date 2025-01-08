
evaluateAllModelsOnAllValidationSets <- function(models, testTrainSetList) {
  bestPerformance <- 0
  bestModel <- NULL
  featurePreprocessor <- NULL
  for (i in seq_len(length(models))) {#run again over all folds to get a performance value for each validation set and each model
    modelCIndices <- c()
    model <- models[[i]]
    rfFeatContainers <- model$getFeatureReductionContainers()
    for (testTrainSet in testTrainSetList) {
      processedTrainData <- NULL
      processedTestData <- NULL
      for (featureName in names(testTrainSet$featuresTrain)) {
        currFeaturesToProcessTrain <- testTrainSet$featuresTrain[[featureName]]
        currFeaturesToProcessTest <- testTrainSet$featuresTest[[featureName]]
        rfFeatContainer <- rfFeatContainers[[featureName]]
        rfTrainSet <- rfFeatContainer$applyRulesToTestData(testTrainSet$outcomeTrain, currFeaturesToProcessTrain)
        rfTestSet <- rfFeatContainer$applyRulesToTestData(testTrainSet$outcomeTest, currFeaturesToProcessTest)
        if (is.null(processedTrainData)) {
          processedTrainData <- rfTrainSet
          processedTestData <- rfTestSet
        } else {
          processedTrainData <- cbind(processedTrainData, rfTrainSet)
          processedTestData <- cbind(processedTestData, rfTestSet)
        }
      }
      
      if (length(processedTrainData) > 0) {
        trainData <- model$bindData(testTrainSet$outcomeTrain, processedTrainData)
        testData <- model$bindData(testTrainSet$outcomeTest, processedTestData)
        model$trainModel(trainData)
        cIndex <- model$evaluate(testData)
        
        modelCIndices <- append(modelCIndices, cIndex)
      }
    }
    meanPerformance <- mean(modelCIndices)
    if (model$comparePerformanceValues(meanPerformance, bestPerformance)) {
      bestPerformance <- meanPerformance
      bestModel <- model
    }
  }
  return(bestModel)
}

getEnsembleModel <- function(models, trainOutcome, trainData)
{
  ensembleModel <- NULL
  if (length(models) > 0)
  {
    ensembleModel <- settings$ensembleBaseModel$clone(deep = TRUE)
    
    for (i in seq_len(length(models))) {
      model <- models[[i]]
      model$trainModelOnNewData(trainOutcome, trainData)
      ensembleModel$addModel(model)
    }  
  }
  
  return(ensembleModel)
     
}


getSpecifFeatValues <- function(featName, model, outcome, data) {
  processedTrainData <- model$prepareNewData(outcome, data)
  processedTrainData[[featName]]
}

getAveragePatient <- function(model, outcome, data) {
  processedTrainData <- model$prepareNewData(outcome, data)
  
  allFeaturesMean = processedTrainData%>%summarise(across(where(is.numeric),mean))#allFeaturesTest%>%mutate_if(is.numeric,mean)
  allFeaturesMin = processedTrainData%>%summarise(across(where(is.factor),~as.factor(0)))
  averagePatient = cbind(allFeaturesMean, allFeaturesMin)
  return(averagePatient)
}

#radiomicFeatures, clinicalFeatures, doseFeatures
modelSelectionInnerLoop <- function(outcome, settings, trainSets) {
  testTrainSets <- list()
  models <- list()
  featureReductioContainers <- list()
  dataSplitter <- settings$getDataSplitterInnerLoop()
  variableTracker = TrackVariables$new()
  processedTrainDataName <- "processedTrainData"
  ## counter <- 0
  ## repeat{
  ##   currprocessedTrainDataName <- paste(processedTrainDataName,counter,sep='')
  ##   currprocessedTrainDataFileName  <- paste(currprocessedTrainDataName,".rds",sep='')
  ##   if(!file.exists(currprocessedTrainDataFileName)){
  ##     break
  ##   }
  ##   counter <- counter +1
  ## }
  
  for (foldNumber in seq_len(settings$nuOfInnerFolds)) {
    
    trainTestSetOutcome <- dataSplitter$sampleDataset(outcome)

    featReduction <- c()
    originalTrainSets <- c()
    originalTestSets <- c()
    processedTrainData <- NULL
    processedTestData <- NULL
    for(featureName in names(trainSets)) {
      featureSet = trainSets[[featureName]]
      featReductionRules <- settings$featureSetsAndReductionRules[[featureName]]
      trainTestSetFeatures <- dataSplitter$getSubsetWithCurrentIdx(featureSet)
      rfFeatContainer <- featReductionRules(trainTestSetOutcome$trainSet, trainTestSetFeatures$trainSet)
      
      if (is.null(names(trainTestSetFeatures$trainSet))) {
        trainFeatures <- trainTestSetFeatures$trainSet[[1]]
        testFeatures <- trainTestSetFeatures$testSet[[1]]
      } else {
        trainFeatures <- trainTestSetFeatures$trainSet
        testFeatures <- trainTestSetFeatures$testSet
      }
      originalTrainSets[[featureName]] <- trainFeatures
      originalTestSets[[featureName]] <-  testFeatures
      
      featReduction[[featureName]] <- rfFeatContainer
      if (is.null(processedTrainData)) {
        processedTrainData <- rfFeatContainer$applyRules()
        processedTestData <- rfFeatContainer$applyRulesToTestData(trainTestSetOutcome$testSet, testFeatures)
      } else {
        processedTrainData <- cbind(processedTrainData, rfFeatContainer$applyRules())
        processedTestData <- cbind(processedTestData, rfFeatContainer$applyRulesToTestData(trainTestSetOutcome$testSet, testFeatures))
      }
      
    }

    ## variableTracker$trackVariableWithName(processedTrainData, currprocessedTrainDataName)
    if (length(processedTrainData) > 0) {
      model <- settings$baseModel$clone(deep = TRUE)
      trainer <- ModelTrainer$new(model, trainTestSetOutcome$trainSet, processedTrainData, trainTestSetOutcome$testSet, processedTestData, settings$maxFeaturesInmodel, settings$featureDeterminationMethod)
      trainer$findBestFeatureCombination()
      if (model$getNumberOfFields() > 0) {
        model$setFeatureReductionContainers(featReduction)
        models[[toString(foldNumber)]] <- model
        testTrainSets[[foldNumber]] <- list("outcomeTrain" = trainTestSetOutcome$trainSet, "featuresTrain" = originalTrainSets, "outcomeTest" = trainTestSetOutcome$testSet, "featuresTest" = originalTestSets)
      }
    }
    

  }
  variableTracker$saveTrackedVariablesToRds()  
  
  list("models" = models, "testTrainSets" = testTrainSets)
}

