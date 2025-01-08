# TODO: Add comment
# 
# Author: fec
###############################################################################


library(R6)

TrackVariables <- R6Class("TrackVariables",
    public = list(
        initialize = function(pathToSave=NULL) {
          private$trackedVariables = list()
          if (!is.null(pathToSave)) {
            private$path = pathToSave
          }
        },
        trackVariable = function(variableToTrack) {
          variableName <- deparse(substitute(variableToTrack))
          self$trackVariableWithName(variableToTrack, variableName)
        },
        trackVariableWithName = function(variableToTrack, variableName) {
          if (!variableName %in% names(private$trackedVariables)) {
            private$trackedVariables[[variableName]] = c()
          }
          private$trackedVariables[[variableName]] = append(private$trackedVariables[[variableName]], variableToTrack)
        },
        getTrackedVariableValues = function(variableToTrack) {
          variableName <- deparse(substitute(variableToTrack))
          return(self$getTrackedVariableValuesByName(variableName))
        },
        getTrackedVariableValuesByName = function(variableName) {
          return(private$trackedVariables[[variableName]])
        },
        saveTrackedVariablesToRds = function() {
          for (variableName in names(private$trackedVariables)) {
            saveRDS(private$trackedVariables[[variableName]], file.path(private$path, paste(variableName,".rds",sep='')))
          }
        }
    ),
    private = list(
        trackedVariables = NULL,
        path = "."
    )
)
