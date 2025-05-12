# TODO: Add comment
#
# Author: fec
###############################################################################


library(R6)
library(survival)
library(dplyr)
library(caret)
library(CORElearn)
library(glmnet)
library(randomForest)
library(FSelector)

FeatureReductionContainer <- R6Class("FeatureReductionContainer",
    public = list(
        initialize = function(outcome, features) {
          private$reduceRadiomicFeatures <- ReduceFeatures$new(outcome, features)
          private$ruleSet <- list()
        },
        applyRules = function() {
          for (rule in private$ruleSet) {
            private$reduceRadiomicFeatures <- private$reduceRadiomicFeatures$run(rule)
          }
          private$reduceRadiomicFeatures$getData()
        },
        applyRulesToTestData = function(outcome, newData) {
          for (rule in private$ruleSet) {
            newData <- private$reduceRadiomicFeatures$runOnTestData(rule, outcome, newData)
          }
          return(newData)
        },
        addRule = function(featureReductionRule, ruleName = NULL) {
          if (is.null(ruleName)) {
            private$ruleSet <- append(private$ruleSet, featureReductionRule)  
          }
          else {
            private$ruleSet[ruleName] <- list(featureReductionRule)
          }
        },
        getRule = function(ruleName) {
          if (ruleName %in% names(private$ruleSet))
          {
            return(private$ruleSet[[ruleName]])
          }
          eles
          {
            return(NULL)
          }
          
        }
    ),
    private = list(
        reduceRadiomicFeatures = NULL,
        ruleSet = NULL
    )
)

ReduceFeatures <- R6Class("ReduceFeatures",
  public = list(
    initialize = function(outcome, features) {
      private$outcome <- outcome
      private$features <- features
    },
    run = function(featureReductionRule) {
      private$features <- featureReductionRule$apply(private$outcome, private$features)
      invisible(self)
    },
    runOnTestData = function(featureReductionRule, outcome, testData) {
      featureReductionRule$applyToTestSet(outcome, testData)
    },
    getData = function() {
      return(private$features)
    }
  ),
  private = list(
    outcome = NULL,
    features = NULL
  )
)

FeatureReductionRule <- R6Class("FeatureReductionRule",
  public = list(
    apply = function(outcome, data) {
      return(data)
    },
    applyToTestSet = function(outcome, newData) {
      return(newData)
    }
  )
)

NormalizeFeaturesrule <- R6Class("NormalizeFeaturesrule",
  inherit = FeatureReductionRule,
  public = list(
    scaleMean = NULL,
    scaleSD = NULL,
    apply = function(outcome, data) {
      normlizedData <- data %>% mutate_if(is.numeric, scale)
      self$scaleMean <- normlizedData %>% summarise(across(where(is.numeric), ~ attr(.x, which = "scaled:center")))
      self$scaleSD <- normlizedData %>% summarise(across(where(is.numeric), ~ attr(.x, which = "scaled:scale")))
      normlizedData <- normlizedData %>% mutate_if(is.numeric, as.vector)
      return(normlizedData)
    },
    applyToTestSet = function(outcome, newData) {
      meanSubtract <- (apply(newData[colnames(self$scaleMean)], 1, "-", unlist(self$scaleMean)))
      if (is.null(dim(meanSubtract))) {
        tmp <- meanSubtract / unlist(self$scaleSD)
      }else {
        tmp <- t(apply(meanSubtract, 2, "/", unlist(self$scaleSD)))
      }
      normalizedTestSet <- newData
      normalizedTestSet[colnames(self$scaleMean)] <- tmp
      return(normalizedTestSet)
    }
  )
)

CenterSpecifiNormalisation <- R6Class("CenterSpecifiNormalisation",
    inherit = FeatureReductionRule,
    public = list(
        apply = function(outcome, data) {
          centers = unique(outcome$center)
          private$noramlizationObjects = list()
          normalizedData = c()
          for (center in centers) {
            centerData <- data[outcome$center == center,]
            normalizer <- NormalizeFeaturesrule$new()
            normalizedCenterData <- normalizer$apply(outcome$getSubSet(outcome$center == center), centerData)
            normalizedData = rbind(normalizedData, normalizedCenterData)
            private$noramlizationObjects[[toString(center)]] = normalizer
          }
          return(normalizedData)
        },
        applyToTestSet = function(outcome, newData) {
          centers = unique(outcome$center)
          normalizedData = c()
          for (center in centers) {
            normalizer <- private$noramlizationObjects[[toString(center)]]
            centerData <- newData[outcome$center == center,]
            normalizedCenterData <- normalizer$applyToTestSet(outcome, centerData)
            normalizedData = rbind(normalizedData, normalizedCenterData)
          }
          return(normalizedData)
        }
    ),
    private = list(
        noramlizationObjects = NULL
    )
)


UnivariateFeatureReductionRuleCox <- R6Class("UnivariateFeatureReductionRuleCox",
  inherit = FeatureReductionRule,
  public = list(
    pValueThreshold = NULL,
    pValueThresholdCoxReq = NULL,
    adjustP = NULL,
    initialize = function(pValueThreshold = 0.05, pValueThresholdCoxReq = 0.05, adjustP = FALSE) {
      self$pValueThreshold <- pValueThreshold
      self$pValueThresholdCoxReq <- pValueThresholdCoxReq
      self$adjustP <- adjustP
    },
    apply = function(outcome, data) {
      featuresToTake <- rep(100, ncol(data))
      otherValue <- rep(0, ncol(data))

      for (featIdx in seq_len(ncol(data))) {
        hasZeroVariance <- nearZeroVar(data[, featIdx], saveMetrics = TRUE)
        if (!hasZeroVariance$zeroVar && !hasZeroVariance$nzv) {
          
          pValue <- tryCatch(
            {
              res.cox <- coxph(Surv(outcome$time, outcome$status) ~ data[, featIdx])
              summary(res.cox)$logtest[3]
            },
            error=function(e) {
              return(1.0)
            })  
          
          otherValue[featIdx] <- tryCatch(
            {
              coxRequirementsMet <- cox.zph(res.cox)
              coxRequirementsMet$table[1, 3]
            },
            error=function(e) {
              return(0.0)
            })     
          

          featuresToTake[featIdx] <- pValue
        }
      }
      
      otherValue[is.na(otherValue)] <- 0.0
      featuresToTake[is.na(featuresToTake)] <- 1.0
      
      data <- data[, featuresToTake < 1, drop = FALSE]
      otherValue <- otherValue[featuresToTake < 1, drop = FALSE]
      featuresToTake <- featuresToTake[featuresToTake < 1, drop = FALSE]

      ## remove features where the cox requirement is not met
      data <- data[, otherValue > self$pValueThresholdCoxReq, drop = FALSE]
      featuresToTake <- featuresToTake[otherValue > self$pValueThresholdCoxReq, drop = FALSE]

      if (self$adjustP) {
        pAdjust <- p.adjust(featuresToTake, "holm")
      } else {
        pAdjust <- featuresToTake
      }
      
      data <- data[, pAdjust < self$pValueThreshold, drop = FALSE]
      private$calculatedFeatures <- data.frame(t(pAdjust[pAdjust < self$pValueThreshold]))
      colnames(private$calculatedFeatures) <- colnames(data)
      private$namesOfColsToKeep <- colnames(data)
      print(paste("after univariate filtering:",length(data)))
      return(data)
    },
    applyToTestSet = function(outcome, newData) {
      return(newData[private$namesOfColsToKeep])
    },
    getCalculatedFeatures = function() {
    	return(private$calculatedFeatures)
    }
  ),
  private = list(
    namesOfColsToKeep = NULL,
    calculatedFeatures = NULL
  )
)

GeneralUnivariateFeatureReductionRule <- R6Class("UnivariateFeatureReductionRuleGlm",
    inherit = FeatureReductionRule,
    public = list(
        pValueThreshold = NULL,
        adjustP = NULL,
        initialize = function(pValueThreshold = 0.05, adjustP = FALSE) {
          self$pValueThreshold <- pValueThreshold
          self$adjustP <- adjustP
        },
        apply = function(outcome, data, innerCode) {
          print(paste("before univariate filtering:",length(data)))
          featuresToTake <- rep(100, ncol(data))
          
          for (featIdx in seq_len(ncol(data))) {
            hasZeroVariance <- nearZeroVar(data[, featIdx], saveMetrics = TRUE)
            if (!hasZeroVariance$zeroVar && !hasZeroVariance$nzv) {
              
              pValue <- innerCode(outcome, data, featIdx)  
              featuresToTake[featIdx] <- pValue
            }
          }
          
          featuresToTake[is.na(featuresToTake)] <- 1.0
          
          data <- data[, featuresToTake < 1, drop = FALSE]
          featuresToTake <- featuresToTake[featuresToTake < 1, drop = FALSE]
          
          if (self$adjustP) {
            pAdjust <- p.adjust(featuresToTake, "holm")
          } else {
            pAdjust <- featuresToTake
          }
          
          data <- data[, pAdjust < self$pValueThreshold, drop = FALSE]
          private$calculatedFeatures <- data.frame(t(pAdjust[pAdjust < self$pValueThreshold]))
          colnames(private$calculatedFeatures) <- colnames(data)
          private$namesOfColsToKeep <- colnames(data)
          print(paste("after univariate filtering:",length(data)))
          return(data)
        },
        applyToTestSet = function(outcome, newData) {
          return(newData[private$namesOfColsToKeep])
        },
        getCalculatedFeatures = function() {
          return(private$calculatedFeatures)
        }
    ),
    private = list(
        namesOfColsToKeep = NULL,
        calculatedFeatures = NULL
    )
)

UnivariateFeatureReductionRuleGlm <- R6Class("UnivariateFeatureReductionRuleGlm",
    inherit = GeneralUnivariateFeatureReductionRule,
    public = list(
        innerLoopLogic = function(outcome, data, featIdx) {
          pValue <- tryCatch(
              {
                model <- glm(outcome$status ~ data[, featIdx], family = binomial())
                modelSummaryCoefs <- coef(summary(model))
                modelSummaryCoefs[2, 4]
              },
              error=function(e) {
                return(1.0)
              })
          return(pValue)
        },
        apply = function(outcome, data) {
          data <- super$apply(outcome, data, self$innerLoopLogic)
          return(data)          
        }
    )
)

UnivariateFeatureReductionRulePolyGlm <- R6Class("UnivariateFeatureReductionRulePolyGlm",
    inherit = GeneralUnivariateFeatureReductionRule,
    public = list(
        innerLoopLogic = function(outcome, data, featIdx) {
          pValue <- tryCatch(
              {
                minPValue = 1.0
                for (degree in 1:3)
                {
                  model <- glm(outcome$status ~ I(data[, featIdx]^degree), family = binomial())
                  modelSummaryCoefs <- coef(summary(model))
                  if ( minPValue > modelSummaryCoefs[2, 4]) {
                    minPValue <- modelSummaryCoefs[2, 4]
                  }
                }
                return(minPValue)
              },
              error=function(e) {
                return(1.0)
              })
          return(pValue)
        },
        apply = function(outcome, data) {
          data <- super$apply(outcome, data, self$innerLoopLogic)
          return(data)          
        }
    )
)

UnivariateFeatureReductionRulePolyLM <- R6Class("UnivariateFeatureReductionRulePolyLM",
    inherit = GeneralUnivariateFeatureReductionRule,
    public = list(
        innerLoopLogic = function(outcome, data, featIdx) {
          pValue <- tryCatch(
              {
                model <- lm(outcome$time ~ poly(data[, featIdx],3))
                modelSummaryCoefs <- coef(summary(model))
                min(modelSummaryCoefs[2:4, 4])
              },
              error=function(e) {
                return(1.0)
              })
          return(pValue)
        },
        apply = function(outcome, data) {
          data <- super$apply(outcome, data, self$innerLoopLogic)
          return(data)
        }
    )
)

UnivariateFeatureReductionRuleLM <- R6Class("UnivariateFeatureReductionRuleLM",
    inherit = GeneralUnivariateFeatureReductionRule,
    public = list(
        innerLoopLogic = function(outcome, data, featIdx) {
          pValue <- tryCatch(
              {
                model <- lm(outcome$time ~ data[, featIdx])
                modelSummaryCoefs <- coef(summary(model))
                modelSummaryCoefs[2, 4]
              },
              error=function(e) {
                return(1.0)
              })
          return(pValue)
        },
        apply = function(outcome, data) {
          data <- super$apply(outcome, data, self$innerLoopLogic)
          return(data)          
        }
    )
)


KeepComparableFeatures <- R6Class("KeepComparableFeatures",
  inherit = FeatureReductionRule,
  public = list(
    pValueThreshold = NULL,
    initialize = function(pValueThreshold = 0.05, otherFeatureCollections) {
      self$pValueThreshold <- pValueThreshold
      private$otherFeatureCollections <- otherFeatureCollections
    },
    apply = function(outcome, data) {
      for (featureCollection in private$otherFeatureCollections) {
        compareAbleFeatureNames <- private$keepComparableFeatures(data, featureCollection, self$pValueThreshold)
        data <- data[compareAbleFeatureNames]
      }
      private$namesOfColsToKeep <- colnames(data)
      print(paste("contour invariant features", length(data)))
      return(data)
    },
    applyToTestSet = function(outcome, newData) {
      return(newData[private$namesOfColsToKeep])
    }
  ),
  private = list(
    otherFeatureCollections = NULL,
    namesOfColsToKeep = NULL,
    keepComparableFeatures = function(featureSet1, featureSet2, pValue = 0.05) {
      p <- list()
      for (i in names(featureSet1)) {
        group1 <- featureSet1[[i]]
        group2 <- featureSet2[[i]]
        if (nearZeroVar(group1, saveMetrics = TRUE)$zeroVar & nearZeroVar(group2, saveMetrics = TRUE)$zeroVar) {
          p[i] <- 1.0
        } else {
          if (!is.numeric(group1)) {
            tab1 <- table(varVal = c(group1, group2), center = c(rep(0, length(group1)), rep(1, length(group2))))
            p[i] <- chisq.test(tab1)$p.value
          } else {
            p[i] <- wilcox.test(group1, group2)$p.value
          }
        }
      }
      return(names(featureSet1)[p > pValue])
    }
  )
)

GroupComparisonTest <- R6Class("GroupComparisonTest",
    inherit = FeatureReductionRule,
    public = list(
        initialize = function(pValueThreshold = 0.05) {
          private$pValueThreshold = pValueThreshold
        },
        applyToTestSet = function(outcome, data) {
          return(data[private$namesOfColsToKeep])
        },
        wilcoxTest = function(group1,group2) {
          return(wilcox.test(group1,group2)$p.value)
        },
        chisqTest = function(group1,group2) {
          tab1 = table(varVal=c(group1,group2),center=c(rep(0,length(group1)),rep(1,length(group2))))
          return(chisq.test(tab1)$p.value)
        }
    ),
    private = list(
        inspectClassWiseDistributions = function(classVariable, data, op = "<") {
          unicqueClassVals = unique(classVariable)
          classCombinations <- combn(length(unicqueClassVals),2)
          pValues <- c()
          for (featIdx in seq_len(ncol(data))) {
            currentFeature <- data[, featIdx]
            testToCall <- self$wilcoxTest
            if (!is.numeric(currentFeature)) {
              testToCall <- self$chisqTest
            }
            currpValues <- c()
            for (classComboIdx in 1:ncol(classCombinations)) {
              classCombo = unicqueClassVals[classCombinations[,classComboIdx]]
              currentFeature1 <- currentFeature[classVariable == classCombo[1]]
              currentFeature2 <- currentFeature[classVariable == classCombo[2]]
              currpValues <- append(currpValues, testToCall(currentFeature1, currentFeature2))
            }
            pValues <- append(pValues, mean(currpValues))
          }
          comFun <- match.fun(op)
          data <- data[, comFun(pValues, private$pValueThreshold), drop = FALSE]
          private$namesOfColsToKeep <- colnames(data)
          return(data)
        },
        pValueThreshold = NULL,
        namesOfColsToKeep = NULL
    )
)

InspectClassWiseDistribution <- R6Class("InspectClassWiseDistribution",
  inherit = GroupComparisonTest,
  public = list(
      apply = function(outcome, data) {
        private$inspectClassWiseDistributions(outcome$status, data)
      }
  )
)

InspectMetaVarDistribution <- R6Class("InspectMetaVarDistribution",
    inherit = GroupComparisonTest,
    public = list(
        initialize = function(metaVarName, pValueThreshold = 0.05) {
          private$pValueThreshold = pValueThreshold
          private$metaVarName = metaVarName
        },
        apply = function(outcome, data) {
          metaVar <- outcome$getMetaVariable(private$metaVarName)
          private$inspectClassWiseDistributions(metaVar, data, ">")
        }
    ),
    private = list(
        metaVarName = NULL
    )
)

CenterAgnosticFeatures <- R6Class("CenterAgnosticFeatures",
  inherit = GroupComparisonTest,
  public = list(
    apply = function(outcome, data) {
      centers = unique(outcome$center)
      status = unique(outcome$status)
      if (length(centers) > 1) {
        centerCombinations <- combn(length(centers),2)
        
        pValues <- c()
        for (featIdx in seq_len(ncol(data))) {
          currentFeature <- data[, featIdx]
          testToCall <- self$wilcoxTest
          if (!is.numeric(currentFeature)) {
            testToCall <- self$chisqTest
          }
          currpValues <- c()
          for (centerComboIdx in 1:ncol(centerCombinations)) {
            centerCombo = centers[centerCombinations[,centerComboIdx]]
            for (stat in status) {
              if ((length(currentFeature[outcome$center == centerCombo[1] & outcome$status == stat]) > 0) & (length(currentFeature[outcome$center == centerCombo[2] & outcome$status == stat]) > 0)) {
                currentFeature1 <- currentFeature[outcome$center == centerCombo[1] & outcome$status == stat]
                currentFeature2 <- currentFeature[outcome$center == centerCombo[2] & outcome$status == stat]
                currpValues <- append(currpValues, testToCall(currentFeature1, currentFeature2))
              }
            }
          }
          pValues <- append(pValues, mean(currpValues))
        }
        
        data <- data[, pValues > private$pValueThreshold, drop = FALSE]
      }
      
      private$namesOfColsToKeep <- colnames(data)
      print(paste("number of center agnostic features:",length(data)))
      return(data)
    }
  )
)

ClusterFeatures <- R6Class("ClusterFeatures",
  inherit = FeatureReductionRule,
  public = list(
    clusterSpearmanThreshold = NULL,
    clusterMethod = NULL,
    keepOnePerCluster = NULL,
    clusterIdxs = NULL,
    pcaObjects = list(),
    initialize = function(clusterSpearmanThreshold = 0.8, clusterMethod = "average", keepOnePerCluster = TRUE) {
      self$clusterSpearmanThreshold <- clusterSpearmanThreshold
      self$clusterMethod <- clusterMethod
      self$keepOnePerCluster <- keepOnePerCluster
    },
    applyToTestSet = function(outcome, newData) {
      principalComponentData <- list()
      for (clustIdx in 1:max(self$clusterIdxs)) {
        columnsOfCurrCluster <- newData[names(self$clusterIdxs)[self$clusterIdxs == clustIdx]]
        pcPredict <- predict(self$pcaObjects[[clustIdx]], newdata = columnsOfCurrCluster)
        principalComponentData[[toString(clustIdx)]] <- pcPredict[, 1]
      }
      return(data.frame(principalComponentData))
    },
    apply = function(outcome, data) {

      print("remainingFeatures: ")
      print(length(data))

      if (length(data) == 0) {
        return(data)
      }

      if (length(data) > 1) {
        featCor <- abs(cor(data, method = "spearman"))
        featDist <- as.dist(1 - featCor)
        featDist[is.na(featDist)] = 1.0
        featClusters <- hclust(featDist, method = self$clusterMethod)
        self$clusterIdxs <- cutree(featClusters, h = 1 - self$clusterSpearmanThreshold)
      } else {
        self$clusterIdxs <- c(1)
      }

      print("number of clusters: ")
      print((max(self$clusterIdxs)))

      principalComponentData <- list()
      for (clustIdx in 1:max(self$clusterIdxs)) {
        columnsOfCurrCluster <- data[, self$clusterIdxs == clustIdx]
        pcObject <- prcomp(columnsOfCurrCluster, scale = TRUE)
        self$pcaObjects[[clustIdx]] <- pcObject
        principalComponentData[[toString(clustIdx)]] <- pcObject$x[, 1]
      }

      return(data.frame(principalComponentData))

    }
  )
)

ClusterFeaturesWithPreComputedFeature <- R6Class("ClusterFeaturesWithPreComputedFeature",
    inherit = FeatureReductionRule,
    public = list(
        clusterSpearmanThreshold = NULL,
        clusterMethod = NULL,
        keepOnePerCluster = NULL,
        clusterIdxs = NULL,
        pcaObjects = list(),
        initialize = function(clusterSpearmanThreshold = 0.8, clusterMethod = "average", keepOnePerCluster = TRUE) {
          self$clusterSpearmanThreshold <- clusterSpearmanThreshold
          self$clusterMethod <- clusterMethod
          self$keepOnePerCluster <- keepOnePerCluster
        },
        setPrecomputedFeaturesProvider = function(featureFunction) {
          private$preComptedFeatureProvider <- featureFunction
        },
        setPrecomputedCorrelations = function(corrValues) {
          private$corrValues <- corrValues
        },
        applyToTestSet = function(outcome, newData) {
          return(newData[private$featureNames])
        },
        apply = function(outcome, data) {
          
          print(paste("features before clustering:",length(data)))
          
          if (length(data) == 0) {
            return(data)
          }
          
          if (length(data) > 1) {
            if (is.null(private$corrValues)) {
              featCor <- abs(cor(data, method = "spearman"))
            }
            else {
              featCor <- private$corrValues
              featCor <- featCor[names(data),names(data)]
            }
            
            featDist <- as.dist(1 - featCor)
            featDist[is.na(featDist)] = 1.0
            featClusters <- hclust(featDist, method = self$clusterMethod)
            self$clusterIdxs <- cutree(featClusters, h = 1 - self$clusterSpearmanThreshold)
          } else {
            self$clusterIdxs <- c(1)
          }
          
          print(paste("number of clusters:",(max(self$clusterIdxs))))
          private$featureNames <- c()
          preComptedFeatures <- private$preComptedFeatureProvider()
          principalComponentData <- list()
          for (clustIdx in 1:max(self$clusterIdxs)) {
            preComputedFeatureForClass <- preComptedFeatures[self$clusterIdxs == clustIdx]
            featureIdxWithsmallesValue <- which(preComputedFeatureForClass == min(preComputedFeatureForClass))
            if (self$keepOnePerCluster)
            {
              featureIdxWithsmallesValue = min(featureIdxWithsmallesValue)
            }
            private$featureNames <- append(private$featureNames, colnames(data)[self$clusterIdxs == clustIdx][featureIdxWithsmallesValue])
          }
          
          return(data[private$featureNames])
          
        }
    ),
    private = list(
        preComptedFeatureProvider = NULL,
        corrValues = NULL,
        featureNames = NULL
    )
)

RemoveFeaturesThatCorrelateWithVolume <- R6Class("RemoveFeaturesThatCorrelateWithVolume",
    inherit = FeatureReductionRule,
    public = list(
        rhoThreshold = NULL,
        initialize = function(rhoThreshold = 0.8) {
          self$rhoThreshold <- rhoThreshold
        },
        setVolumeColName = function(volumeColName) {
          private$volumeColName <- volumeColName
        },
        applyToTestSet = function(outcome, newData) {
          return(newData[private$featureNames])
        },
        apply = function(outcome, data) {
          print(paste("before volume correlation analysis:",length(data)))
          volumeColNumber <- which(colnames(data) == private$volumeColName)
          volumes <- data[,volumeColNumber,drop=FALSE]
          otherFeatures <- data[,-volumeColNumber,drop=FALSE]
          
          rhos <- abs(cor(volumes, otherFeatures, method="spearman"))
          rhos[is.na(rhos)] = 0.0
          remainingFeatures <- otherFeatures[,rhos < 0.8]
          
          private$featureNames <- c(colnames(volumes), colnames(remainingFeatures))
          print(paste("after volume correlation analysis:",length(data[private$featureNames])))
          return(data[private$featureNames])
          
        }
    ),
    private = list(
        volumeColName = NULL,
        featureNames = NULL
    )
)

LassoFeatureReduction <- R6Class("LassoFeatureReduction",
    inherit = FeatureReductionRule,
    public = list(
        applyToTestSet = function(outcome, newData) {
          return(newData[private$featureNames])
        },
        apply = function(outcome, data) {
          print(paste("before lasso regressioin analysis:",length(data)))
          
          x <- data.matrix(data)
          y <- outcome$status
          
          numberOfFeaturesBefore = ncol(x)
          numberOfFeaturesAfter = -1
          
          iterationCounter = 1
          while (numberOfFeaturesBefore != numberOfFeaturesAfter) {
            
            cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
            
            model <- glmnet(x, y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
            modelCoefficient <- coef(model, cv.lasso$lambda.min)
            #print(modelCoefficient)
            #[-1] is the intercept
            modelCoefficientValuesNotZero <- as.vector(modelCoefficient)[-1] != 0
            featureNames <- rownames(modelCoefficient)[-1][modelCoefficientValuesNotZero]
            
            numberOfFeaturesAfter <- length(featureNames)
            numberOfFeaturesBefore = ncol(x)
            #print(paste("after lasso iteration ",iterationCounter,":",numberOfFeaturesAfter))
            x <- data.matrix(x[,modelCoefficientValuesNotZero])
            iterationCounter = iterationCounter + 1
            if (numberOfFeaturesAfter < 2) {
              break
            }
          }
          
          private$featureNames <- featureNames
          
          print(paste("after lasso regressioin analysis:",length(private$featureNames)))
          
          return(data[private$featureNames])
          
        }
    ),
    private = list(
        volumeColName = NULL,
        featureNames = NULL
    )
)

SortFeaturesWithPreComputedFeature <- R6Class("SortFeaturesWithPreComputedFeature",
     inherit = FeatureReductionRule,
     public = list(
       initialize = function(decreasing = FALSE) {
         private$decreasing = decreasing
       },
       setPrecomputedFeaturesProvider = function(featureFunction) {
         private$preComptedFeatureProvider <- featureFunction
       },
       applyToTestSet = function(outcome, newData) {
         return(newData[private$featureOrder])
       },
       apply = function(outcome, data) {
         
         if (length(data) == 0) {
           return(data)
         }
         
         preComptedFeatures <- private$preComptedFeatureProvider()
         preComptedFeatures <- preComptedFeatures[colnames(data)]
         private$featureOrder <- order(as.double(preComptedFeatures), decreasing=private$decreasing)
         return(data[private$featureOrder])
         
       }
     ),
     private = list(
       preComptedFeatureProvider = NULL,
       featureOrder = NULL,
       decreasing = NULL
     )
)

ChiSquareParameterRanking <- R6Class("ChiSquareParameterRanking",
    inherit = FeatureReductionRule,
    public = list(
        threshold = NULL,
        initialize = function(threshold = 1.0) {
          self$threshold <- threshold
        },
        apply = function(outcome, data) {
          print(paste("before chi-square filtering:",length(data)))
          
          
          
          result <- as.data.frame(
              lapply(data, function(col) {
                    if (is.numeric(col)) {
                      col < median(col, na.rm = TRUE)
                    } else {
                      col
                    }
                  })
          )
          
          tmp <- cbind("status"= outcome$status, result)
          
          featuresToTake <- 1.0 - chi.squared(status ~ ., tmp)
          
          featuresToTake[is.na(featuresToTake)] <- 1.0
          
          data <- data[, featuresToTake < 1, drop = FALSE]
          featuresToTake <- featuresToTake[featuresToTake < 1, drop = FALSE]
          
          data <- data[, featuresToTake < self$threshold, drop = FALSE]
          private$calculatedFeatures <- data.frame(t(featuresToTake[featuresToTake < self$threshold]))
          colnames(private$calculatedFeatures) <- colnames(data)
          private$namesOfColsToKeep <- colnames(data)
          print(paste("after chi-square filtering:",length(data)))
          return(data)
        },
        applyToTestSet = function(outcome, newData) {
          return(newData[private$namesOfColsToKeep])
        },
        getCalculatedFeatures = function() {
          return(private$calculatedFeatures)
        }
    ),
    private = list(
        namesOfColsToKeep = NULL,
        calculatedFeatures = NULL
    )
)

RandomForestParameterRanking <- R6Class("RandomForestParameterRanking",
    inherit = FeatureReductionRule,
    public = list(
        threshold = NULL,
        initialize = function(threshold = 1.0) {
          self$threshold <- threshold
        },
        apply = function(outcome, data) {
          print(paste("before random forest filtering:",length(data)))
          
          tmp <- cbind("status"= outcome$status, data)
          rf_model <- randomForest(status ~ ., data = tmp, importance = TRUE)
          importance_scores <- randomForest::importance(rf_model, type = 2)
          featuresToTake <- 1 - (importance_scores / max(importance_scores))
          featuresToTake[is.na(featuresToTake)] <- 1.0
          
          data <- data[, featuresToTake < 1, drop = FALSE]
          featuresToTake <- featuresToTake[featuresToTake < 1, drop = FALSE]
          
          data <- data[, featuresToTake < self$threshold, drop = FALSE]
          private$calculatedFeatures <- data.frame(t(featuresToTake[featuresToTake < self$threshold]))
          colnames(private$calculatedFeatures) <- colnames(data)
          private$namesOfColsToKeep <- colnames(data)
          print(paste("after random forest filtering:",length(data)))
          return(data)
        },
        applyToTestSet = function(outcome, newData) {
          return(newData[private$namesOfColsToKeep])
        },
        getCalculatedFeatures = function() {
          return(private$calculatedFeatures)
        }
    ),
    private = list(
        namesOfColsToKeep = NULL,
        calculatedFeatures = NULL
    )
)

InformationGainReduction <- R6Class("InformationGainReduction",
    inherit = FeatureReductionRule,
    public = list(
        threshold = NULL,
        initialize = function(threshold = 1.0) {
          self$threshold <- threshold
        },
        apply = function(outcome, data) {
          print(paste("before info gain filtering:",length(data)))
          featuresToTake <- rep(100, ncol(data))
          
          for (featIdx in seq_len(ncol(data))) {
            hasZeroVariance <- nearZeroVar(data[, featIdx], saveMetrics = TRUE)
            if (!hasZeroVariance$zeroVar && !hasZeroVariance$nzv) {
              
              infoGain <- tryCatch(
                  {
                    ig <- attrEval(as.factor(outcome$status) ~ data[, featIdx], estimator = "InfGain")
                    1.0 - ig[[1]]
                  },
                  error=function(e) {
                    return(1.0)
                  })  
              featuresToTake[featIdx] <- infoGain
            }
          }
          
          featuresToTake[is.na(featuresToTake)] <- 1.0
          
          data <- data[, featuresToTake < 1, drop = FALSE]
          featuresToTake <- featuresToTake[featuresToTake < 1, drop = FALSE]
          
          data <- data[, featuresToTake < self$threshold, drop = FALSE]
          private$calculatedFeatures <- data.frame(t(featuresToTake[featuresToTake < self$threshold]))
          colnames(private$calculatedFeatures) <- colnames(data)
          private$namesOfColsToKeep <- colnames(data)
          print(paste("after info gain filtering:",length(data)))
          return(data)
        },
        applyToTestSet = function(outcome, newData) {
          return(newData[private$namesOfColsToKeep])
        },
        getCalculatedFeatures = function() {
          return(private$calculatedFeatures)
        }
    ),
    private = list(
        namesOfColsToKeep = NULL,
        calculatedFeatures = NULL
    )
)
