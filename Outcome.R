# TODO: Add comment
# 
# Author: fec
###############################################################################

library(R6)

Outcome <- R6Class("Outcome", list(
        status = NULL,
        time = NULL,
        ids = NULL,
        center = NULL,
        metas = list(),
        initialize = function(status, ids=NULL, time = NULL, center = NULL) {
          self$status <- status
          self$time <- time
          self$ids <- ids
          self$center <- center
        },
        getAsList = function()
        {
          list("ids"=self$ids,"status"=self$status,"time"=self$time, "center" = self$center, "metas" = self$metas)
        },
        addMetaVariable = function(name, data) {
          self$metas[[name]]  <- data
        },
        getMetaVariable = function(name) {
          if (name %in% names(self$metas)) {
            return(self$metas[[name]])
          } else {
            return(list())
          }
        },
        getSubSet = function(idxs)
        {
          status = self$status[idxs]
          time = NULL
          ids = NULL
          center = NULL
          if (!is.null(self$time))
          {
            time = self$time[idxs]
          }
          if (!is.null(self$ids))
          {
            ids = self$ids[idxs]
          }
          if (!is.null(self$center))
          {
            center = self$center[idxs]
          }
          outcomeNew <- Outcome$new(status, ids, time, center)
          for (metaName in names(self$metas)) {
            outcomeNew$addMetaVariable(metaName, self$metas[[metaName]][idxs])
          }
          return(outcomeNew)
        }
    ))
