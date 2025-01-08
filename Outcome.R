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
        initialize = function(status, ids=NULL, time = NULL, center = NULL) {
          self$status <- status
          self$time <- time
          self$ids <- ids
          self$center <- center
        },
        getAsList = function()
        {
          list("ids"=self$ids,"status"=self$status,"time"=self$time, "center" = self$center)
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
          Outcome$new(status, ids, time, center)
        }
    ))
