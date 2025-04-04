# TODO: Add comment
#
# Author: fec
###############################################################################


library(R6)
library(pROC)

GenericModel <- R6Class(
  "GenericModel",
  public = list(
  trainModelOnNewData = function(outcome, data) {
  },
  evaluateModelOnNewData = function(outcome, data) {
    return(NULL)
  },
  modelPredictionOnNewData = function(outcome, data, predictionType = "risk") {
    return(NULL)
  }, 
  getFeatureSets = function() {
    return(NULL)
  },
  printModelSummary = function() {
    return(NULL)
  }
  ),
  private = list()
)

Model <- R6Class(
  "Model",
  inherit = GenericModel,
  public = list(
    initialize = function() {
      private$formulaFields <- c()
      private$nuOfFeatures <- 0
      private$evaluationFunction <- "ACC"
      self$setFormulaBase()
    },
    getEvaluationFunction = function() {
      return(private$evaluationFunction)
    },
    setEvaluationFunction = function(evaluationFunction) {
      private$evaluationFunction = evaluationFunction
    },
    trainModel = function(trainData) {
      private$model  <-
        glm(as.formula(self$getFormulaString()),
            family = binomial(),
            data = trainData)
    },
    trainModelOnNewData = function(outcome, data) {
      processedTrainData <- self$prepareNewData(outcome, data)
      trainData <- self$bindData(outcome, processedTrainData)
      self$trainModel(trainData)
    },
    evaluateModelOnNewData = function(outcome, data) {
      processedData <- self$prepareNewData(outcome, data)
      testData <- self$bindData(outcome, processedData)
      testAccuracy <- self$evaluate(testData)
      return(testAccuracy)
    },
    modelPredictionOnNewData = function(outcome, data, predictionType = "response") {
      processedData <- self$prepareNewData(outcome, data)
      testData <- self$bindData(outcome, processedData)
      prediction <- private$predict(testData, predictionType)
      return(prediction)
    },
    prepareNewData = function(outcome, data) {
      processedTrainData <- NULL
      for (featureName in names(data)) {
        currDataToProcess <- data[[featureName]]
        if (is.null(names(currDataToProcess))) {
          currDataToProcess <- currDataToProcess[[1]]
        }
        preprocessor <-
          private$featureReductionContainers[[featureName]]
        if (is.null(preprocessor))
        {
          processedData <- currDataToProcess
        } else {
          processedData <-
            preprocessor$applyRulesToTestData(outcome, currDataToProcess)
        }
        if (is.null(processedTrainData)) {
          processedTrainData <- processedData
        } else {
          processedTrainData <- cbind(processedTrainData, processedData)
        }
      }
      return(processedTrainData)
    },
    evaluate = function(data) {
      prediction <- predict(private$model, data, type = "response")
      modelACC = -1
      if (private$evaluationFunction == "AUC") {
        modelACC <- suppressMessages(auc(data$status, prediction))[[1]]
      } else {
        modelAccuracy <-
            confusionMatrix(factor(round(prediction), levels = 0:1), factor(data$status, levels = 0:1))
        modelACC <-
            modelAccuracy[["byClass"]][["Balanced Accuracy"]]
      }
      return(modelACC)
    },
    comparePerformanceValues = function(perfValue1, perfValue2) {
      #returns TRUE if first value is better, otherwise FALSE
      if (perfValue1 > perfValue2) {
        return(TRUE)
      }
      return(FALSE)
    },
    addFieldToFormula = function(fieldName) {
      private$formulaFields <- append(private$formulaFields, fieldName)
      length(private$formulaFields)
    },
    getNumberOfFields = function() {
      length(private$formulaFields)
    },
    popFieldFromFormula = function() {
      private$formulaFields <-
        private$formulaFields[1:length(private$formulaFields) - 1]
      length(private$formulaFields)
    },
    setFormulaBase = function() {
      private$formulaBase <- "status"
    },
    bindData = function(outcome, features) {
      return(data.frame(cbind("status" = outcome$status, features)))
    },
    getFormulaString = function() {
      fields <- paste(private$formulaFields, collapse = "+")
      paste(private$formulaBase, "~", fields)
    },
    getPValuesOfFittedModel = function() {
      modelSummaryCoefs <- coef(summary(private$model))
      modelSummaryCoefs[2:nrow(modelSummaryCoefs), 4]
    },
    getModelParamNames = function() {
      paste(sort(private$formulaFields), collapse = "+")
    },
    printModelSummary = function() {
      print(summary(private$model))
    },
    messageModelSummary = function() {
      message(summary(private$model))
    },
    getFeatureReductionContainers = function() {
      return(private$featureReductionContainers)
    },
    setFeatureReductionContainers = function(newFeatureReductionContainers) {
      private$featureReductionContainers <- newFeatureReductionContainers
    },
    getFeatureSets = function() {
      featRedContainer <- self$getFeatureReductionContainers()
      featureSets <- names(featRedContainer)
      return(featureSets)
    }
  ),
  private = list(
    model = NULL,
    formulaBase = NULL,
    formulaFields = NULL,
    nuOfFeatures = NULL,
    evaluationFunction = NULL,
    featureReductionContainers = NULL,
    predict = function(data, predictionType = "response") {
      prediction <- predict(private$model, data, type = predictionType)
      return(prediction)
    }
  )
)

CoxModel <- R6Class(
  "CoxModel",
  inherit = Model,
  public = list(
    evaluate = function(data) {
      cIndex <- concordance(private$model, newdata = data)
      modelACC <- cIndex$concordance
    },
    modelPredictionOnNewData = function(outcome, data, predictionType = "risk") {
      prediction <-
        super$modelPredictionOnNewData(outcome, data, predictionType)
      return(prediction)
    },
    trainModel = function(trainData) {
      private$model <-
        coxph(as.formula(self$getFormulaString()), data = trainData)
    },
    setFormulaBase = function() {
      private$formulaBase <- "Surv(time,status)"
    },
    bindData = function(outcome, features) {
      return(data.frame(
        cbind(
          "time" = outcome$time,
          "status" = outcome$status,
          features
        )
      ))
    },
    getPValuesOfFittedModel = function() {
      modelSummaryCoefs <- coef(summary(private$model))
      modelSummaryCoefs[1:nrow(modelSummaryCoefs), 5]
    },
    getSurvFit = function(data) {
      return(survfit(private$model, newdata = data))
    }
  ),
  private = list(
    predict = function(data, predictionType = "risk") {
      prediction <- super$predict(data, predictionType)
      return(prediction)
    }
  )
  
)

CoxEnsembleModel <- R6Class(
  "CoxEnsembleModel",
  inherit = GenericModel,
  public = list(
    initialize = function() {
      private$models <- list()
    },
    addModel = function(model) {
      private$models <- append(private$models, model)
    },
    evaluate = function(data) {
      predictions <- predict(data)
      tmpData <- cbind(
        "time" = data$time,
        "status" = data$status,
        "features" = predictions
      )
      
      cIndex <-
        1 - concordance(Surv(time, status) ~ features, data = tmpData)
      return(cIndex)
    },
    trainModelOnNewData = function(outcome, data) {
      for (model in private$models)
      {
        model$trainModelOnNewData(outcome, data)
      }
    },
    evaluateModelOnNewData = function(outcome, data) {
      predictions <- self$modelPredictionOnNewData(outcome, data)
      cIndex <- -1
      if (length(predictions) > 0) {
        tmpData <- data.frame(cbind(
                "time" = outcome$time,
                "status" = outcome$status,
                "features" = predictions
            ))
        
        cIndex <-
            1 - concordance(Surv(time, status) ~ features, data = tmpData)$concordance
      }
      
      return(cIndex)
    },
    modelPredictionOnNewData = function(outcome, data, predictionType = "risk") {
      predictions = data.frame()
      for (model in private$models)
      {
        prediction <-
          model$modelPredictionOnNewData(outcome, data, predictionType)
        if (length(nearZeroVar(prediction)) == 0)
        {
          predictions <- rbind(predictions, prediction)
        }
      }
      avgPrediction <- c()
      if (nrow(predictions) > 0) {
        sdevs <- sapply(predictions, sd)
        avgPrediction <- colMeans(predictions,na.rm=TRUE)
        insideInterVal0 <- apply(predictions,1, function(x) x > avgPrediction - 2*sdevs)
        insideInterVal1 <- apply(predictions,1, function(x) x < avgPrediction + 2*sdevs)
        tmp <- t(insideInterVal0 & insideInterVal1)
        predictions[!tmp] <- NA  
        avgPrediction <- colMeans(predictions,na.rm=TRUE)
      }
      return(avgPrediction)
    },
    getFeatureSets = function() {
      featureSetUnion = c()
      for (model in private$models)
      {
        featuresSes <- model$getFeatureSets()
        featureSetUnion <- union(featureSetUnion, featuresSes)
      }
      return(featureSetUnion)
    },
    printModelSummary = function() {
      for (model in private$models)
      {
        model$printModelSummary()
      }
    }
  ),
  private = list(
    models = NULL,
    predict = function(data, predictionType = "risk") {
      predictions = data.frame()
      for (model in private$models)
      {
        prediction <- model$predict(data, predictionType)
        if (length(nearZeroVar(prediction)) == 0)
        {
          predictions <- rbind(predictions, prediction)
        }
      }
      avgPrediction <- colMeans(predictions,na.rm=TRUE)
      return(avgPrediction)
    }
  )
  
)

GlmEnsembleModel <- R6Class(
    "GlmEnsembleModel",
    inherit = GenericModel,
    public = list(
        initialize = function() {
          private$models <- list()
          private$evaluationFunction <- "ACC"
        },
        getEvaluationFunction = function() {
          return(private$evaluationFunction)
        },
        setEvaluationFunction = function(evaluationFunction) {
          private$evaluationFunction = evaluationFunction
        },
        addModel = function(model) {
          private$models <- append(private$models, model)
        },
        evaluate = function(data) {
          predictions <- predict(data)
          modelACC = -1
          if (private$evaluationFunction == "AUC") {
            modelACC <- suppressMessages(auc(data$status, predictions))[[1]]
          } else {
            modelAccuracy <- confusionMatrix(factor(round(predictions), levels = 0:1), factor(data$status, levels = 0:1))
            modelACC <- modelAccuracy[["byClass"]][["Balanced Accuracy"]]
          }
          return(modelACC)
        },
        trainModelOnNewData = function(outcome, data) {
          for (model in private$models)
          {
            model$trainModelOnNewData(outcome, data)
          }
        },
        evaluateModelOnNewData = function(outcome, data) {
          predictions <- self$modelPredictionOnNewData(outcome, data)
          modelACC <- -1
          if (length(predictions) > 0) {
            if (private$evaluationFunction == "AUC") {
              modelACC <- suppressMessages(auc(outcome$status, predictions))[[1]]
            } else {
              modelAccuracy <- confusionMatrix(factor(round(predictions), levels = 0:1), factor(outcome$status, levels = 0:1))
              modelACC <- modelAccuracy[["byClass"]][["Balanced Accuracy"]]
            }
          }
          
          return(modelACC)
        },
        modelPredictionOnNewData = function(outcome, data, predictionType = "response") {
          predictions = data.frame()
          for (model in private$models)
          {
            prediction <-
                model$modelPredictionOnNewData(outcome, data, predictionType)
            if (length(nearZeroVar(prediction)) == 0)
            {
              predictions <- rbind(predictions, prediction)
            }
          }
          avgPrediction <- c()
          if (nrow(predictions) > 0) {
            sdevs <- sapply(predictions, sd)
            avgPrediction <- colMeans(predictions,na.rm=TRUE)
            insideInterVal0 <- apply(predictions,1, function(x) x > avgPrediction - 2*sdevs)
            insideInterVal1 <- apply(predictions,1, function(x) x < avgPrediction + 2*sdevs)
            tmp <- t(insideInterVal0 & insideInterVal1)
            predictions[!tmp] <- NA  
            avgPrediction <- colMeans(predictions,na.rm=TRUE)
          }
          return(avgPrediction)
        },
        getFeatureSets = function() {
          featureSetUnion = c()
          for (model in private$models)
          {
            featuresSes <- model$getFeatureSets()
            featureSetUnion <- union(featureSetUnion, featuresSes)
          }
          return(featureSetUnion)
        },
        printModelSummary = function() {
          for (model in private$models)
          {
            model$printModelSummary()
          }
        }
    ),
    private = list(
        models = NULL,
        evaluationFunction = NULL,
        predict = function(data, predictionType = "response") {
          predictions = data.frame()
          for (model in private$models)
          {
            prediction <- model$predict(data, predictionType)
            if (length(nearZeroVar(prediction)) == 0)
            {
              predictions <- rbind(predictions, prediction)
            }
          }
          avgPrediction <- colMeans(predictions,na.rm=TRUE)
          return(avgPrediction)
        }
    )

)

LEnsembleModel <- R6Class(
    "LEnsembleModel",
    inherit = GlmEnsembleModel,
    public = list(
        evaluate = function(data) {
          predictions <- predict(data)
          rmse <- sqrt(mean((data$time - predictions)^2))
          return(rmse)
        },
        evaluateModelOnNewData = function(outcome, data) {
          predictions <- self$modelPredictionOnNewData(outcome, data)
          rmse <- -1
          if (length(predictions) > 0) {
            rmse <- sqrt(mean((outcome$time - predictions)^2))
          }
          return(rmse)
        }
    )
)

LModel <- R6Class(
    "LModel",
    inherit = Model,
    public = list(
        evaluate = function(data) {
          prediction <- predict(private$model, data)
          rmse <- sqrt(mean((data$time - prediction)^2))
        },
        comparePerformanceValues = function(perfValue1, perfValue2) {
          #returns TRUE if first value is better, otherwise FALSE
          if (perfValue1 < perfValue2) {
            return(TRUE)
          }
          return(FALSE)
        },
        trainModel = function(trainData) {
          private$model <- lm(as.formula(self$getFormulaString()), data = trainData)
        },
        setFormulaBase = function() {
          private$formulaBase <- "time"
        },
        bindData = function(outcome, features) {
          return(data.frame(
                  cbind(
                      "time" = outcome$time,
                      features
                  )
              ))
        }
    )
)

PolyLModel <- R6Class(
    "PolyLModel",
    inherit = Model,
    public = list(
        initialize = function(degree = 3) {
          super$initialize()
          private$degree = degree
        },
        evaluate = function(data) {
          prediction <- predict(private$model, data)
          rmse <- sqrt(mean((data$time - prediction)^2))
        },
        setDegree = function(degree) {
          private$degree = degree
        },
        getDegree = function(degree) {
          private$degree
        },
        comparePerformanceValues = function(perfValue1, perfValue2) {
          #returns TRUE if first value is better, otherwise FALSE
          if (perfValue1 < perfValue2) {
            return(TRUE)
          }
          return(FALSE)
        },
        trainModel = function(trainData) {
          private$model <- lm(as.formula(self$getFormulaString()), data = trainData)
        },
        setFormulaBase = function() {
          private$formulaBase <- "time"
        },
        bindData = function(outcome, features) {
          return(data.frame(
                  cbind(
                      "time" = outcome$time,
                      features
                  )
              ))
        },
        getFormulaString = function () {
          fields <- paste0("poly(", private$formulaFields, ",", private$degree, ")", collapse = "+")
          paste(private$formulaBase, "~", fields)
        }
    ),
    private = list(
        degree = NULL
    )
)

PolyGLModel <- R6Class(
    "PolyGLModel",
    inherit = Model,
    public = list(
        initialize = function(degree = 3) {
          super$initialize()
          private$degree = degree
        },
        setDegree = function(degree) {
          private$degree = degree
        },
        getDegree = function(degree) {
          private$degree
        },
        getFormulaString = function () {
          fields <- paste0("poly(", private$formulaFields, ",", private$degree, ")", collapse = "+")
          paste(private$formulaBase, "~", fields)
        }
    ),
    private = list(
        degree = NULL
    )
)
