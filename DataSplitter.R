# TODO: Add comment
#
# Author: fec
###############################################################################


library(R6)
library(reticulate)
source("Outcome.R")
source_python("pythonFunctions/pythonCode.py")

DataSplitter <- R6Class("DataSplitter",
    public = list(
        sampleFunction = NULL,
        initialize = function(prcntTestSet = 0.20) {
          private$prcntTestSet <- prcntTestSet
        },
        splitByCenter = function(outcome) {
          centers <- unique(outcome$center)
          centerOccurences <- table(outcome$center)
          trainCenterIdx <- which.max(centerOccurences)
          trainIdxs <- which(outcome$center == trainCenterIdx)
          testIdxs <- which(outcome$center != trainCenterIdx)
          return(list("testIdxs" = testIdxs, "trainIdxs" = trainIdxs))
        },
        getIndexForTrainTestSetConsideringPatIdsUnderSample = function(outcome) {
          uniqueNamesWithEvents <- aggregate(outcome$status, list(outcome$ids), sum)
          colnames(uniqueNamesWithEvents) <- c("Ids", "Events")
          
          patWithEvent <- uniqueNamesWithEvents$Ids[uniqueNamesWithEvents$Events > 0]
          patWithoutEvent <- uniqueNamesWithEvents$Ids[uniqueNamesWithEvents$Events == 0]
          
          nuOfSamplesClass0 <- length(patWithoutEvent)
          nuOfSamplesClass1 <- length(patWithEvent)
          
          nuOfSamplesPerCrossValidation0 <- floor(nuOfSamplesClass0 * (1.0 - private$prcntTestSet))
          nuOfSamplesPerCrossValidation1 <- floor(nuOfSamplesClass1 * (1.0 - private$prcntTestSet))
          if (nuOfSamplesPerCrossValidation0 == 0) {
            nuOfSamplesPerCrossValidation0 <- 1
          }
          if (nuOfSamplesPerCrossValidation1 == 0) {
            nuOfSamplesPerCrossValidation1 <- 1
          }          
          nuOfSamplesPerCrossValidation <- min(nuOfSamplesPerCrossValidation0, nuOfSamplesPerCrossValidation1)
          
          namesGrp1 <- sample(patWithEvent, nuOfSamplesPerCrossValidation)
          namesGrp0 <- sample(patWithoutEvent, nuOfSamplesPerCrossValidation)
          
          idxs0 <- which(outcome$ids %in% namesGrp0)
          idxs1 <- which(outcome$ids %in% namesGrp1)

          trainIdxs <- c(idxs0, idxs1)
          testIdxs <- seq_len(length(outcome$ids))[trainIdxs*-1]
          
          nuOfEvents <- sum(outcome$status[trainIdxs])
          nuOfNonEvents <- sum(outcome$status[trainIdxs] == 0)
          samplesPerClass <- min(nuOfEvents, nuOfNonEvents)
          trainIdxs0 <- sample(trainIdxs[outcome$status[trainIdxs] == 0], samplesPerClass)
          trainIdxs1 <- sample(trainIdxs[outcome$status[trainIdxs] == 1], samplesPerClass)
          
          trainIdxs <- c(trainIdxs0, trainIdxs1)
          
          return(list("testIdxs" = testIdxs, "trainIdxs" = trainIdxs))
        },
        getIndexForTrainTestSet = function(outcome) {
          numberOfPatients = length(outcome$ids)
          nuOfSamplesTestSet = floor(numberOfPatients * private$prcntTestSet)
          rowIndices = seq_len(length(outcome$ids))
          testIdxs <- sample(rowIndices, nuOfSamplesTestSet)
          trainIdxs <- rowIndices[testIdxs*-1]
          return(list("testIdxs" = testIdxs, "trainIdxs" = trainIdxs))
        },
        getIndexWithNoSampling = function(outcome) {
          numberOfPatients = length(outcome$ids)
          rowIndices = seq_len(length(outcome$ids))
          return(list("testIdxs" = rowIndices, "trainIdxs" = rowIndices))
        },        
        getIndexForTrainTestSetConsideringPatIds = function(outcome) {
          uniqueNamesWithEvents <- aggregate(outcome$status, list(outcome$ids), sum)
          colnames(uniqueNamesWithEvents) <- c("Ids", "Events")
          
          patWithEvent <- uniqueNamesWithEvents$Ids[uniqueNamesWithEvents$Events > 0]
          patWithoutEvent <- uniqueNamesWithEvents$Ids[uniqueNamesWithEvents$Events == 0]
          
          nuOfSamplesClass0 <- length(patWithoutEvent)
          nuOfSamplesClass1 <- length(patWithEvent)
          
          nuOfSamplesPerCrossValidation0 <- floor(nuOfSamplesClass0 * private$prcntTestSet)
          nuOfSamplesPerCrossValidation1 <- floor(nuOfSamplesClass1 * private$prcntTestSet)
          
          if (nuOfSamplesPerCrossValidation0 == 0) {
            nuOfSamplesPerCrossValidation0 <- 1
          }
          if (nuOfSamplesPerCrossValidation1 == 0) {
            nuOfSamplesPerCrossValidation1 <- 1
          }          
          
          namesGrp1 <- sample(patWithEvent, nuOfSamplesPerCrossValidation1)
          namesGrp0 <- sample(patWithoutEvent, nuOfSamplesPerCrossValidation0)
          
          idxs0 <- which(outcome$ids %in% namesGrp0)
          idxs1 <- which(outcome$ids %in% namesGrp1)
          
          
          testIdxs <- c(idxs0, idxs1)
          trainIdxs <- seq_len(length(outcome$ids))[testIdxs*-1]
          return(list("testIdxs" = testIdxs, "trainIdxs" = trainIdxs))
        },
        getIndexForTrainTestSetConsideringTime = function(outcome) {
          nuOfSamplesClass0 <- sum(outcome$status == 0)
          nuOfSamplesClass1 <- sum(outcome$status == 1)
          
          nuOfSamplesPerCrossValidation0 <- floor(nuOfSamplesClass0 * private$prcntTestSet)
          nuOfSamplesPerCrossValidation1 <- floor(nuOfSamplesClass1 * private$prcntTestSet)
          
          if (nuOfSamplesPerCrossValidation0 == 0) {
            nuOfSamplesPerCrossValidation0 <- 1
          }
          if (nuOfSamplesPerCrossValidation1 == 0) {
            nuOfSamplesPerCrossValidation1 <- 1
          }          
          
          sampleIdxClass0 <- which(outcome$status == 0)
          sampleIdxClass1 <- which(outcome$status == 1)
          
          time1 <- outcome$time[sampleIdxClass1] #order Idx by time and take samples evenly distributed over time
          time1Chunks <- split(sampleIdxClass1[order(time1)], ceiling(seq_along(time1) / (length(time1) / nuOfSamplesPerCrossValidation1)))
          idxs1 <- unlist(lapply(time1Chunks, sample, 1))
          
          
          time0 <- outcome$time[sampleIdxClass0] #order Idx by time and take samples evenly distributed over time
          time0Chunks <- split(sampleIdxClass0[order(time0)], ceiling(seq_along(time0) / (length(time0) / nuOfSamplesPerCrossValidation0)))
          idxs0 <- unlist(lapply(time0Chunks, sample, 1))
          
          testIdxs <- c(idxs0, idxs1)
          trainIdxs <- seq_len(length(outcome$ids))[testIdxs*-1]
          return(list("testIdxs" = testIdxs, "trainIdxs" = trainIdxs))
        },
        getIndexForTrainTestSetConsideringClassBalance = function(outcome) {
          trainTestSplit <- classBalancedSplit(outcome$ids, outcome$status, private$prcntTestSet)
          testIdxs <- trainTestSplit[[3]] + 1
          trainIdxs <- seq_len(length(outcome$ids))[testIdxs*-1]
          return(list("testIdxs" = testIdxs, "trainIdxs" = trainIdxs))
        },
        setSampleFunction = function(sampleFunction) {
          self$sampleFunction <- sampleFunction
        },
        sampleDataset = function(outcome) {
          testTrainIdxs <- self$sampleFunction(outcome)
          private$testIdxs <- testTrainIdxs$testIdxs
          private$trainIdxs <- testTrainIdxs$trainIdxs
          trainTestSetList <- self$getOutcomeSubsetWithCurrentIdx(outcome)
          return(trainTestSetList)
        },
        getTestIdxs = function() {
          return(private$testIdxs)
        },
        setTestIdxs = function(testIdxsNew) {
          private$testIdxs <- testIdxsNew
        },
        getTrainIdxs = function() {
          return(private$trainIdxs)
        },
        setTrainIdxs = function(trainIdxsNew) {
          private$trainIdxs <- trainIdxsNew
        },
        getSubsetWithCurrentIdx = function(data) {
          if (is.data.frame(data)) {
            return(self$getSubsetFromDataFrameWithCurrentIdx(data))
          }
          if (is.list(data)) {
            return(self$getSubsetFromDataListWithCurrentIdx(data))
          }
        },
        getSubsetFromDataFrameWithCurrentIdx = function(data) {
          trainSet <- data[private$trainIdxs, ]
          testSet <- data[private$testIdxs, ]
          list("trainSet" = trainSet, "testSet" = testSet)
        },
        getSubsetFromDataListWithCurrentIdx = function(data) {
          trainTestSets <- lapply(data, self$getSubsetFromDataFrameWithCurrentIdx)
          trainSets <- lapply(trainTestSets, `[[`, "trainSet")
          testSets <- lapply(trainTestSets, `[[`, "testSet")
          list("trainSet" = trainSets, "testSet" = testSets)
        },
        getOutcomeSubsetWithCurrentIdx = function(outcome) {
          testSet <- outcome$getSubSet(private$testIdxs)
          trainSet <- outcome$getSubSet(private$trainIdxs)
          list("trainSet" = trainSet, "testSet" = testSet)
        }
    ),
    private = list(
        prcntTestSet = NULL,
        testIdxs = NULL,
        trainIdxs = NULL
    )
)
