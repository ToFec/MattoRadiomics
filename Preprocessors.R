# TODO: Add comment
# 
# Author: fec
###############################################################################

PolynomialPreprocessor <- R6Class(
    "PolynomialPreprocessor",
    public = list(
        initialize = function(degree) {
          private$degree = degree
        },
        apply = function(features)
        {
          polyCols <- c()
          num_cols <- features[sapply(features, is.numeric)]
          for (degree in seq(2,private$degree)) {
            polyCols <- append(polyCols, setNames(num_cols^degree, paste0(names(num_cols), "^",degree)))
          }
          result <- cbind(features, polyCols)
          return(result)
        }
    ),
    private = list(
      degree = NULL
    )
)
