# TODO: Add comment
# 
# Author: fec
###############################################################################

getEvaluationFunctionObject = function(model) {
  binaryModels = c("PolyGLModel", "GlmEnsembleModel", "Model", "PolyGLModel")
  coxModels = c("CoxModel", "CoxEnsembleModel")
  linearModels = c("LModel", "LEnsembleModel", "PolyLModel")
  if (class(model)[1] %in% binaryModels) {
    return(GLMEvaluationFunction$new())
  }
  else if (class(model)[1] %in% coxModels) {
    return(CoxEvaluationFunction$new())
  }
  else if (class(model)[1] %in% linearModels) {
    return(LMEvaluationFunction$new())
  }
  else {
    return(NULL)
  }
}


EvaluationFunction <- R6Class(
    "EvaluationFunction",
    public = list(
        initialize = function() {
        },
        setOutcome = function(outcome) {
          private$outcome = outcome
        },
        getOutcome = function() {
          private$outcome
        },
        setPrediction = function(prediction) {
          private$prediction = prediction
        },
        getPrediction = function() {
          private$prediction
        },
        calculateFigures = function () {
          private$figures = c()
        },
        getFigures = function() {
          private$figures
        }
    ),
    private = list(
        outcome = NULL,
        prediction = NULL,
        model = NULL,
        figures = NULL
    )
)


CoxEvaluationFunction <- R6Class(
    "CoxEvaluationFunction",
    inherit = EvaluationFunction,
    public = list(
        calculateFigures = function () {
          if (!is.null(private$outcome) && !is.null(private$prediction)) {
            
            ci <- self$concordanceIndex(private$outcome$status, private$outcome$time, private$prediction)
            private$figures <- c("ConcordanceIndex" = ci)
          }
        },
        concordanceIndex = function(actualStatus, actualTime, prediction) {
          tmpData <- cbind(
              "time" = actualTime,
              "status" = actualStatus,
              "features" = predictions
          )
          
          cIndex <-
              1 - concordance(Surv(time, status) ~ features, data = tmpData)
          return(cIndex)
        }
    )
)

LMEvaluationFunction <- R6Class(
    "LMEvaluationFunction",
    inherit = EvaluationFunction,
    public = list(
        calculateFigures = function () {
          if (!is.null(private$outcome) && !is.null(private$prediction)) {
            
            rmse <- self$rmse(private$outcome$time, private$prediction)
            private$figures <- c("RootMeanSquaredError" = rmse)
          }
        },
        rmse = function(actual, prediction) {
          rmse <- sqrt(mean((data$time - prediction)^2))
        }
    )
)

GLMEvaluationFunction <- R6Class(
    "GLMEvaluationFunction",
    inherit = EvaluationFunction,
    public = list(
        setPrediction = function(prediction) {
          if (!is.factor(prediction)) {
            prediction = round(prediction)
          }
          super$setPrediction(prediction)
        },
        calculateFigures = function () {
          if (!is.null(private$outcome) && !is.null(private$prediction)) {
              
            AUC <- self$calculateAUC(private$outcome$status, private$prediction)
            balancedACC <- self$calculateBalancedAccuracy(private$outcome$status, private$prediction)
            precision <- self$precision(private$outcome$status, private$prediction)
            recall <- self$recall(private$outcome$status, private$prediction)
            f1_score <- self$f1_score(private$outcome$status, private$prediction)
            mcc <- self$mcc(private$outcome$status, private$prediction)
            
            private$figures <- c("AUC" = AUC, "BalancedAccuracy" = balancedACC, "Precision" = precision, "Recall" = recall, "F1Score" = f1_score, "MatthewsCorrelationCoefficient" = mcc)
          }
        },
        calculateAUC = function(status, prediction) {
          AUC <- suppressMessages(auc(status, prediction))[[1]]
          return(AUC)
        },
        calculateBalancedAccuracy = function(status, prediction) {
          modelAccuracy <- confusionMatrix(factor(round(prediction), levels = 0:1), factor(status, levels = 0:1))
          balancedAccuracy <- modelAccuracy[["byClass"]][["Balanced Accuracy"]]
        },
        precision = function(actual, predicted) {
          tp <- sum(actual == 1 & predicted == 1)
          fp <- sum(actual == 0 & predicted == 1)
          if ((tp + fp) == 0) return(NA)
          tp / (tp + fp)
        },
        # Recall: TP / (TP + FN)
        recall = function(actual, predicted) {
          tp <- sum(actual == 1 & predicted == 1)
          fn <- sum(actual == 1 & predicted == 0)
          if ((tp + fn) == 0) return(NA)
          tp / (tp + fn)
        },
        
        # F1 Score: 2 * (Precision * Recall) / (Precision + Recall)
        f1_score = function(actual, predicted) {
          p <- self$precision(actual, predicted)
          r <- self$recall(actual, predicted)
          if (is.na(p) || is.na(r) || (p + r) == 0) return(NA)
          2 * p * r / (p + r)
        },
        
        # Matthews Correlation Coefficient (MCC)
        mcc = function(actual, predicted) {
          tp <- sum(actual == 1 & predicted == 1)
          tn <- sum(actual == 0 & predicted == 0)
          fp <- sum(actual == 0 & predicted == 1)
          fn <- sum(actual == 1 & predicted == 0)
          denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
          if (denom == 0) return(NA)
          (tp * tn - fp * fn) / denom
        }
    )
)
