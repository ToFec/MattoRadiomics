# TODO: Add comment
# 
# Author: fec
###############################################################################


library(R6)
source("Outcome.R")

MattoCSVParserProgression <- R6Class("MattoCSVParserProgression", 
		public = list(
        inputDataCSV = '/media/fechter/DATA/Bilder/MATTO/RadiomicsAnalysis/radiomicFeaturesCombinedQSqrtWavelet.csv',
				
				getRadiomicFeatures = function(inputData) {
          features <- inputData[,-(1:9)]
          features <- private$applyPreprocessors(features)
					return(features)
				},
        getRadiomcFeaturesAsList = function(inputData) {
          lapply(inputData, self$getRadiomicFeatures)
        },
        getDoseFeaturesForFirstDataSet = function(inputData) {
          return(self$getDoseFeatures(inputData[[1]]))
        },
				getDoseFeatures = function(inputData)
				{
					return(list())
				},
				getOutcome = function(inputData)
				{
					idsAll = inputData$Id
					timeAll <- inputData$Time_To_Progression
					statusAll <- inputData$Recurrence
          centerIds <- private$getCenter(inputData$Study)
					
					outcome = Outcome$new(statusAll, idsAll, timeAll, centerIds)
				},
        getClinicalFeaturesForFirstDataSet = function(inputData) {
          return(self$getClinicalFeatures(inputData[[1]]))
        },
				getClinicalFeatures = function(inputData)
				{
					clinicalFeatures <- list()
					return(clinicalFeatures)
				},
				readCSVFiles = function()
				{
          inputData <- private$readInputData()
					list(inputData)
				},
        addPreprocessor = function(newPreprocessor) {
          if (is.null(private$preProcessors)) {
            private$preProcessors <- c(newPreprocessor)
          } else {
            private$preProcessors <- append(private$preProcessors, newPreprocessor)
          }
        }
		),
		private = list(
        getCenter = function(centers)
        {
          centerIds <- c(rep(-1,private$nFeatures))
          uniqueVals =unique(centers)
          centerId = 0
          for (uniqueVal in uniqueVals) {
            centerIds[centers == uniqueVal] = centerId
            centerId = centerId + 1
          }
          return(centerIds)
        },
        readInputData = function() {
          inputData <- read.csv(self$inputDataCSV)
          private$nFeatures <- nrow(inputData)
          return(inputData)
        },
        applyPreprocessors = function(inputData) {
          for (preProcessor in private$preProcessors) {
            inputData <- preProcessor$apply(inputData)
          }
          return(inputData)
        },
        nFeatures = NULL,
        preProcessors = NULL
		)
)

MattoCSVParserOS <- R6Class(
    "MattoCSVParserOS",
    inherit = MattoCSVParserProgression,
    public = list(
        getOutcome = function(inputData)
        {
          idsAll = inputData$Id
          timeAll <- inputData$OS
          statusAll <- inputData$Dead
          centerIds <- private$getCenter(inputData$Study)
          
          outcome = Outcome$new(statusAll, idsAll, timeAll, centerIds)
        }
    )
)

MattoCSVParserLocalDistant <- R6Class(
    "MattoCSVParserLocalDistant",
    inherit = MattoCSVParserProgression,
    public = list(
        getOutcome = function(inputData)
        {
          idsAll = inputData$Id
          timeAll <- inputData$Time_To_Progression
          statusAll <- inputData$Local_Distant
          centerIds <- private$getCenter(inputData$Study)
          
          outcome = Outcome$new(statusAll, idsAll, timeAll, centerIds)
        }
    ),
    private = list(
        readInputData = function() {
          inputData <- read.csv(self$inputDataCSV)
          inputData <- inputData[!is.na(inputData$Local_Distant),]
          private$nFeatures <- nrow(inputData)
          return(inputData)
        }
    )
)


