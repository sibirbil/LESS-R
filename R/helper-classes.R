StandardScaler <- R6::R6Class(classname = "StandardScaler",
                              private = list(
                                mean = NULL,
                                stdev = NULL
                              ),
                              public = list(
                                fit = function(X) {
                                  check_matrix(X)
                                  # standart deviation function for internal use
                                  # the default stdev() function of R, divides by length(ln)-1
                                  standart_dev <- function(ln) {
                                    sqrt(sum((ln - mean(ln)) ^ 2 / length(ln)))
                                  }
                                  # assign mean and standart deviation parameters
                                  private$stdev <- apply(X, 2, standart_dev)
                                  private$mean <- colMeans(X)
                                  invisible(self)
                                },
                                transform = function(X) {
                                  check_matrix(X)
                                  # append mean and standart deviation values to the X matrix
                                  merged <- rbind(private$mean, private$stdev, X)
                                  # standardize each value by the corresponding mean and stdev values
                                  # using z = (x - u) / s formula
                                  merged <- apply(merged, 2, function(x) (x - x[1]) / x[2] )
                                  #return the standardized version of original X matrix, extract the mean and stdev rows (first 2 cols)
                                  if(nrow(merged) > 3){
                                    return(as.matrix(merged[3:nrow(merged),]))
                                  }else if(nrow(merged) == 3){
                                    return(t(matrix(merged[3:nrow(merged),])))
                                  }
                                },
                                fit_transform = function(X) {
                                  self$fit(X)$transform(X)
                                }
                              ))

RandomGenerator <- R6::R6Class(classname = "RandomGenerator",
                               private = list(
                                 random_state = NULL,
                                 index = NULL
                               ),
                               public = list(
                                 initialize = function(random_state){
                                   private$random_state = random_state
                                   private$index = 1
                                 },
                                 choice = function(range, size){
                                   # range: sampling takes place from 1:range
                                   # size: a non-negative integer giving the number of items to choose
                                   set.seed(private$random_state)
                                   permutation <- sample(range)

                                   # this part helps if the index go beyond range.
                                   if((private$index + size - 1) > range){
                                     set.seed(private$random_state)
                                     permutation <- c(permutation, sample(range, size=(private$index + size - 1 - range), replace = TRUE))
                                   }

                                   result <- permutation[private$index:(private$index+size-1)]
                                   private$index <- private$index + size
                                   return(result)
                                 },
                                 integers = function(range, size = 1) {
                                   set.seed(private$random_state)
                                   permutation <- sample.int(range)

                                   # this part helps if the index go beyond range.
                                   if((private$index + size - 1) > range){
                                     set.seed(private$random_state)
                                     permutation <- c(permutation, sample.int(range, size=(private$index + size - 1 - range), replace = TRUE))
                                   }

                                   result <- permutation[private$index:(private$index+size-1)]
                                   private$index <- private$index + size
                                   return(result)
                                 },
                                 get_random_state = function(){
                                   return(private$random_state)
                                 }
                               ))

LESSWarn <- R6::R6Class(classname = "LESSWarn",
                        public = list(
                          initialize = function(msg = "", flag = TRUE){
                            if(flag){
                              warning(msg)
                            }
                          }
                        ))

ConstantPredictor <- R6::R6Class(classname = "ConstantPredictor",
                                 inherit = SklearnEstimator,
                                 private = list(
                                   isFitted = FALSE,
                                   y = NULL
                                 ),
                                 public = list(
                                   fit = function(X, y){
                                     private$y <- unique(y)
                                     private$isFitted <- TRUE
                                     invisible(self)
                                   },
                                   predict = function(X0){
                                     return(rep(private$y, nrow(X0)))
                                   },
                                   predict_proba= function(X0){
                                     check_is_fitted(self)
                                     # Input validation
                                     check_matrix(X0)
                                     len_X0 <- nrow(X0)
                                     predprobs <- matrix(0, len_X0, 2)
                                     predprobs[, 1] <- rep(1 - private$y, len_X0)
                                     predprobs[, 2] <- rep(private$y, len_X0)
                                     return(predprobs)
                                   },
                                   get_isFitted = function(){
                                     return(private$isFitted)
                                   }
                                 ))
