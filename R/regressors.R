####################
# HELPER CLASSES
####################
#' @title BaseEstimator
#'
#' @description A dummy base R6 class that provides get_all_fields, get_attributes and set_random_state functionalities for estimators
#'
#' @return R6 Class of BaseEstimator
#' @export
BaseEstimator <- R6::R6Class(classname = "BaseEstimator",
                             private = list(
                               priv_fields = function(){
                                 classList <- class(self)[-length(class(self))]
                                 classNum <- length(classList)
                                 fieldList <- list()
                                 for (i in 1:classNum) {
                                   priv_fields <- get(class(self)[i])$private_fields
                                   fieldList <- append(fieldList, priv_fields)
                                 }
                                 return(names(fieldList))
                               },
                               public_fields = function(){
                                 classList <- class(self)[-length(class(self))]
                                 classNum <- length(classList)
                                 fieldList <- list()
                                 for (i in 1:classNum) {
                                   public_fields <- get(class(self)[i])$public_fields
                                   fieldList <- append(fieldList, public_fields)
                                 }
                                 return(names(fieldList))
                               }
                             ),
                             public = list(
                               #' @description Auxiliary function returning the name of all private and public fields of the self class
                               get_all_fields = function(){
                                 return(append(private$priv_fields(), private$public_fields()))
                               },
                               #' @description Auxiliary function returning the name and values of all private and public fields of the self class
                               get_attributes = function(){
                                 priv_values <- purrr::map(private$priv_fields(), ~.subset2(private, .x))
                                 public_values <- purrr::map(private$public_fields(), ~.subset2(self, .x))
                                 names(priv_values) <- private$priv_fields()
                                 names(public_values) <- private$public_fields()
                                 return(append(priv_values, public_values))
                               },
                               #' @description Auxiliary function that sets random state attribute of the self class
                               #'
                               #' @param random_state seed number to be set as random state
                               #' @return self
                               set_random_state = function(random_state){
                                 private$random_state <- random_state
                                 invisible(self)
                               }
                             ))
#' @title SklearnEstimator
#'
#' @description A dummy base R6 class that includes fit, predict functions for estimators
#'
#' @return R6 Class of SklearnEstimator
#' @export
SklearnEstimator <- R6::R6Class(classname = "SklearnEstimator",
                                inherit = BaseEstimator,
                                private = list(
                                  type = "estimator",
                                  isFitted = FALSE
                                ),
                                public = list(
                                  #' @description Dummy fit function
                                  fit = function() {
                                    stop("Needs to implement fit(X, y)")
                                  },
                                  #' @description Dummy predict function
                                  predict = function(){
                                    stop("Needs to implement predict(X, y)")
                                    invisible(self)
                                  },
                                  #' @description Auxiliary function returning the type of the class e.g 'estimator'
                                  get_type = function(){
                                    return(private$type)
                                  },
                                  #' @description Auxiliary function returning the isFitted flag
                                  get_isFitted = function(){
                                    return(private$isFitted)
                                  }
                                )
)

LocalModel <- R6::R6Class(classname = "LocalModel",
                          public = list(
                            estimator = NULL,
                            center = NULL,
                            initialize = function(estimator = NULL, center = NULL) {
                              self$estimator <- estimator
                              self$center <- center
                            }
                          ))

Replication <- R6::R6Class(classname = "Replication",
                           public = list(
                             sc_object = NULL,
                             global_estimator = NULL,
                             local_estimators = NULL,
                             initialize = function(sc_object = NULL, global_estimator = NULL, local_estimators = NULL) {
                               self$sc_object <- sc_object
                               self$global_estimator <- global_estimator
                               self$local_estimators <- local_estimators
                             }
                           ))

#' @title LinearRegression
#'
#' @description Wrapper R6 Class of stats::lm function that can be used for LESSRegressor and LESSClassifier
#'
#' @return R6 Class of LinearRegression
#' @importFrom stats lm
#' @seealso [stats::lm()]
#' @export
LinearRegression <- R6::R6Class(classname = "LinearRegression",
                                inherit = SklearnEstimator,
                                private = list(
                                  estimator_type = "regressor",
                                  model = NULL
                                ),
                                public = list(
                                  #' @description Fits a linear model (y ~ X)
                                  #'
                                  #' @param X 2D matrix or dataframe that includes predictors
                                  #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                  #'
                                  #' @return Fitted R6 Class of LinearRegression
                                  #'
                                  #' @examples
                                  #' data(abalone)
                                  #' split_list <- train_test_split(abalone, test_size =  0.3)
                                  #' X_train <- split_list[[1]]
                                  #' X_test <- split_list[[2]]
                                  #' y_train <- split_list[[3]]
                                  #' y_test <- split_list[[4]]
                                  #'
                                  #' lr <- LinearRegression$new()
                                  #' lr$fit(X_train, y_train)
                                  fit = function(X, y) {
                                    df <- prepareDataset(X, y)
                                    private$model <- lm(y ~ ., data = df)
                                    private$isFitted <- TRUE
                                    invisible(self)
                                  },
                                  #' @description Predict regression value for X.
                                  #'
                                  #' @param X0 2D matrix or dataframe that includes predictors
                                  #'
                                  #' @return The predict values.
                                  #'
                                  #' @examples
                                  #' lr <- LinearRegression$new()
                                  #' lr$fit(X_train, y_train)
                                  #' preds <- lr$predict(X_test)
                                  #'
                                  #' lr <- LinearRegression$new()
                                  #' preds <- lr$fit(X_train, y_train)$predict(X_test)
                                  #'
                                  #' preds <- LinearRegression$new()$fit(X_train, y_train)$predict(X_test)
                                  #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                  predict = function(X0) {
                                    check_is_fitted(self)
                                    data <- prepareXset(X0)
                                    # in case of rank deficiency, supress related warnings
                                    suppressWarnings(predict(private$model, newdata = data))
                                  },
                                  #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                  get_estimator_type = function() {
                                    return(private$estimator_type)
                                  }
                                )
)

#' @title DecisionTreeRegressor
#'
#' @description Wrapper R6 Class of rpart::rpart function that can be used for LESSRegressor and LESSClassifier
#'
#' @param min_samples_split The minimum number of observations that must exist in a node in order for a split to be attempted (defaults to 2).
#' @param min_samples_leaf The minimum number of observations in any terminal (leaf) node (defaults to 1).
#' @param cp Complexity Parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.
#' This means that the overall R-squared must increase by cp at each step. The main role of this parameter is
#' to save computing time by pruning off splits that are obviously not worthwhile. (defaults to 0.001)
#' @param xval Number of cross-validations (defaults to 10)
#' @param surrogate_style Controls the selection of a best surrogate. If set to 0 (default) the program uses the total number of correct
#' classification for a potential surrogate variable, if set to 1 it uses the percent correct, calculated over the non-missing values of the surrogate.
#' The first option more severely penalizes covariates with a large number of missing values.
#' @param max_depth The maximum depth of any node of the final tree, with the root node counted as depth 0.
#' Values greater than 30 will give nonsense results on 32-bit machines.
#'
#' @return R6 Class of DecisionTreeRegressor
#' @importFrom rpart rpart
#' @seealso [rpart::rpart()]
#' @export
DecisionTreeRegressor <- R6::R6Class(classname = "DecisionTreeRegressor",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "regressor",
                                       model = NULL,
                                       min_samples_split = NULL,
                                       min_samples_leaf = NULL,
                                       cp = NULL,
                                       xval = NULL,
                                       surrogate_style = NULL,
                                       max_depth = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of DecisionTreeRegressor
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt <- DecisionTreeRegressor$new(min_samples_split = 10)
                                       #' dt <- DecisionTreeRegressor$new(min_samples_leaf = 6, cp = 0.01)
                                       initialize = function(min_samples_split = 2, min_samples_leaf = 1, cp = 0.001, xval = 10, surrogate_style = 0, max_depth = 30){
                                         private$min_samples_split = min_samples_split
                                         private$min_samples_leaf = min_samples_leaf
                                         private$cp = cp
                                         private$xval = xval
                                         private$surrogate_style = surrogate_style
                                         private$max_depth = max_depth
                                       },
                                       #' @description Builds a decision tree regressor from the training set (X, y).
                                       #'
                                       #' @param X 2D matrix or dataframe that includes predictors
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                       #'
                                       #' @return Fitted R6 Class of DecisionTreeRegressor
                                       #'
                                       #' @examples
                                       #' data(abalone)
                                       #' split_list <- train_test_split(abalone, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt$fit(X_train, y_train)
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         private$model <- rpart::rpart(y ~ ., method = "anova", data = df,
                                                                       control = rpart::rpart.control(minsplit = private$min_samples_split,
                                                                                                      minbucket = private$min_samples_leaf,
                                                                                                      cp = private$cp, xval = private$xval,
                                                                                                      surrogatestyle = private$surrogate_style,
                                                                                                      maxdepth = private$max_depth))
                                         private$isFitted <- TRUE
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return The predict values.
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' dt$fit(X_train, y_train)
                                       #' preds <- dt$predict(X_test)
                                       #'
                                       #' dt <- DecisionTreeRegressor$new()
                                       #' preds <- dt$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- DecisionTreeRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                       predict = function(X0) {
                                         check_is_fitted(self)
                                         data <- prepareXset(X0)
                                         predict(private$model, data, method = "anova")
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     ))

#' @title RandomForestRegressor
#'
#' @description Wrapper R6 Class of randomForest::randomForest function that can be used for LESSRegressor and LESSClassifier
#'
#' @param n_estimators Number of trees to grow. This should not be set to too small a number,
#' to ensure that every input row gets predicted at least a few times (defaults to 100).
#' @param random_state Seed number to be used for fixing the randomness (default to NULL).
#' @param min_samples_leaf Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown
#' (and thus take less time) (defaults to 1)
#' @param max_leaf_nodes Maximum number of terminal nodes trees in the forest can have.
#' If not given, trees are grown to the maximum possible (subject to limits by nodesize).
#' If set larger than maximum possible, a warning is issued. (defaults to NULL)
#'
#' @return R6 Class of RandomForestRegressor
#' @importFrom randomForest randomForest
#' @seealso [randomForest::randomForest()]
#' @export
RandomForestRegressor <- R6::R6Class(classname = "RandomForestRegressor",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "regressor",
                                       model = NULL,
                                       n_estimators = NULL,
                                       random_state = NULL,
                                       min_samples_leaf = NULL,
                                       max_leaf_nodes = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of RandomForestRegressor
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestRegressor$new()
                                       #' rf <- RandomForestRegressor$new(n_estimators = 500)
                                       #' rf <- RandomForestRegressor$new(n_estimators = 500, random_state = 100)
                                       initialize = function(n_estimators = 100, random_state = NULL, min_samples_leaf = 1,
                                                             max_leaf_nodes = NULL){
                                         private$n_estimators = n_estimators
                                         private$random_state = random_state
                                         private$min_samples_leaf = min_samples_leaf
                                         private$max_leaf_nodes = max_leaf_nodes
                                       },
                                       #' @description Builds a random forest regressor from the training set (X, y).
                                       #'
                                       #' @param X 2D matrix or dataframe that includes predictors
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                       #'
                                       #' @return Fitted R6 Class of RandomForestRegressor
                                       #'
                                       #' @examples
                                       #' data(abalone)
                                       #' split_list <- train_test_split(abalone, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' rf <- RandomForestRegressor$new()
                                       #' rf$fit(X_train, y_train)
                                       fit = function(X, y){
                                         df <- prepareDataset(X, y)
                                         set.seed(private$random_state)
                                         private$model <- randomForest::randomForest(y ~ ., data = df, type = "regression", ntree = private$n_estimators,
                                                                                     nodesize = private$min_samples_leaf,
                                                                                     maxnodes = private$max_leaf_nodes)
                                         private$isFitted <- TRUE
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return The predict values.
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestRegressor$new()
                                       #' rf$fit(X_train, y_train)
                                       #' preds <- rf$predict(X_test)
                                       #'
                                       #' rf <- RandomForestRegressor$new()
                                       #' preds <- rf$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- RandomForestRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                       predict = function(X0){
                                         check_is_fitted(self)
                                         data <- prepareXset(X0)
                                         predict(private$model, data)
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     ))

#' @title KNeighborsRegressor
#'
#' @description Wrapper R6 Class of caret::knnreg function that can be used for LESSRegressor and LESSClassifier
#'
#' @param k Number of neighbors considered (defaults to 5).
#'
#' @return R6 Class of KNeighborsRegressor
#' @seealso [caret::knnreg()]
#' @importFrom caret knnreg
#' @export
KNeighborsRegressor <- R6::R6Class(classname = "KNeighborsRegressor",
                                   inherit = SklearnEstimator,
                                   private = list(
                                     estimator_type = "regressor",
                                     model = NULL,
                                     k = NULL
                                   ),
                                   public = list(
                                     #' @description Creates a new instance of R6 Class of KNeighborsRegressor
                                     #'
                                     #' @examples
                                     #' knr <- KNeighborsRegressor$new()
                                     #' knr <- KNeighborsRegressor$new(k = 5)
                                     initialize = function(k = 5){
                                       private$k = k
                                     },
                                     #' @description Fit the k-nearest neighbors regressor from the training set (X, y).
                                     #'
                                     #' @param X 2D matrix or dataframe that includes predictors
                                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                                     #'
                                     #' @return Fitted R6 Class of KNeighborsRegressor
                                     #'
                                     #' @examples
                                     #' data(abalone)
                                     #' split_list <- train_test_split(abalone, test_size =  0.3)
                                     #' X_train <- split_list[[1]]
                                     #' X_test <- split_list[[2]]
                                     #' y_train <- split_list[[3]]
                                     #' y_test <- split_list[[4]]
                                     #'
                                     #' knr <- KNeighborsRegressor$new()
                                     #' knr$fit(X_train, y_train)
                                     fit = function(X, y){
                                       df <- prepareDataset(X, y)
                                       private$model <- caret::knnreg(y ~ ., data = df, k=private$k)
                                       private$isFitted <- TRUE
                                       invisible(self)
                                     },
                                     #' @description Predict regression value for X0.
                                     #'
                                     #' @param X0 2D matrix or dataframe that includes predictors
                                     #'
                                     #' @return The predict values.
                                     #'
                                     #' @examples
                                     #' knr <- KNeighborsRegressor$new()
                                     #' knr$fit(X_train, y_train)
                                     #' preds <- knr$predict(X_test)
                                     #'
                                     #' knr <- KNeighborsRegressor$new()
                                     #' preds <- knr$fit(X_train, y_train)$predict(X_test)
                                     #'
                                     #' preds <- KNeighborsRegressor$new()$fit(X_train, y_train)$predict(X_test)
                                     #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                                     predict = function(X0){
                                       check_is_fitted(self)
                                       data <- prepareXset(X0)
                                       predict(private$model, data)
                                     },
                                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                     get_estimator_type = function() {
                                       return(private$estimator_type)
                                     }
                                   ))

#' @title Support Vector Regression
#'
#' @description Wrapper R6 Class of e1071::svm function that can be used for LESSRegressor and LESSClassifier
#'
#' @param scale A logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed.
#' Per default, data are scaled internally (both x and y variables) to zero mean and unit variance.
#' The center and scale values are returned and used for later predictions (default: TRUE)
#' @param kernel The kernel used in training and predicting. Possible values are: "linear", "polynomial", "radial", "sigmoid" (default is "radial")
#' @param degree Parameter needed for kernel of type polynomial (default: 3)
#' @param gamma Parameter needed for all kernels except linear (default: 1/(data dimension))
#' @param cost Cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation (default: 1)
#' @param cache_size Cache memory in MB (default: 40)
#' @param coef0 Parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param tolerance Tolerance of termination criterion (default: 0.001)
#' @param epsilon Epsilon in the insensitive-loss function (default: 0.1)
#' @param shrinking Option whether to use the shrinking-heuristics (default: TRUE)
#' @param cross If a integer value k>0 is specified, a k-fold cross validation on the training data is performed to assess the quality of the model:
#' the accuracy rate for classification and the Mean Squared Error for regression (default: 0)
#' @param fitted Logical indicating whether the fitted values should be computed and included in the model or not (default: TRUE)
#' @param probability Logical indicating whether the model should allow for probability predictions (default: FALSE)
#'
#' @return R6 Class of SVR
#' @seealso [e1071::svm()]
#' @importFrom e1071 svm
#' @export
SVR <- R6::R6Class(classname = "SVR",
                   inherit = SklearnEstimator,
                   private = list(
                     estimator_type = "regressor",
                     model = NULL,
                     scale = NULL,
                     kernel = NULL,
                     degree = NULL,
                     gamma = NULL,
                     coef0 = NULL,
                     cost = NULL,
                     cache_size = NULL,
                     tolerance = NULL,
                     epsilon = NULL,
                     shrinking = NULL,
                     cross = NULL,
                     probability = NULL,
                     fitted = NULL
                   ),
                   public = list(
                     #' @description Creates a new instance of R6 Class of SVR
                     #'
                     #' @examples
                     #' svr <- SVR$new()
                     #' svr <- SVR$new(kernel = "polynomial")
                     initialize = function(scale = TRUE, kernel = "radial", degree = 3, gamma = NULL, coef0 = 0, cost = 1,
                                           cache_size = 40, tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0,
                                           probability = FALSE, fitted = TRUE){
                       private$scale = scale
                       private$kernel = kernel
                       private$degree = degree
                       private$gamma = gamma
                       private$coef0 = coef0
                       private$cost = cost
                       private$cache_size = cache_size
                       private$tolerance = tolerance
                       private$epsilon = epsilon
                       private$shrinking = shrinking
                       private$cross = cross
                       private$probability = probability
                       private$fitted = fitted
                     },
                     #' @description Fit the SVM model from the training set (X, y).
                     #'
                     #' @param X 2D matrix or dataframe that includes predictors
                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes response variables
                     #'
                     #' @return Fitted R6 Class of SVR
                     #'
                     #' @examples
                     #' data(abalone)
                     #' split_list <- train_test_split(abalone, test_size =  0.3)
                     #' X_train <- split_list[[1]]
                     #' X_test <- split_list[[2]]
                     #' y_train <- split_list[[3]]
                     #' y_test <- split_list[[4]]
                     #'
                     #' svr <- SVR$new()
                     #' svr$fit(X_train, y_train)
                     fit = function(X, y){
                       df <- prepareDataset(X, y)
                       gamma = if (is.vector(X)) 1 else 1 / ncol(X)
                       private$model <- e1071::svm(y ~ ., data = df, scale = private$scale, kernel = private$kernel, degree = private$degree,
                                                   gamma = if (is.null(private$gamma)) gamma else private$gamma, coef0 = private$coef0,
                                                   cost = private$cost, cachesize = private$cache_size, tolerance = private$tolerance, epsilon = private$epsilon,
                                                   shrinking = private$shrinking, cross = private$cross, probability = private$probability,
                                                   fitted = private$fitted)
                       private$isFitted <- TRUE
                       invisible(self)
                     },
                     #' @description Predict regression value for X0.
                     #'
                     #' @param X0 2D matrix or dataframe that includes predictors
                     #'
                     #' @return The predict values.
                     #'
                     #' @examples
                     #' svr <- SVR$new()
                     #' svr$fit(X_train, y_train)
                     #' preds <- svr$predict(X_test)
                     #'
                     #' svr <- SVR$new()
                     #' preds <- svr$fit(X_train, y_train)$predict(X_test)
                     #'
                     #' preds <- SVR$new()$fit(X_train, y_train)$predict(X_test)
                     #' print(head(matrix(c(y_test, preds), ncol = 2, dimnames = (list(NULL, c("True", "Prediction"))))))
                     predict = function(X0){
                       check_is_fitted(self)
                       data <- prepareXset(X0)
                       predict(private$model, data)
                     },
                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                     get_estimator_type = function() {
                       return(private$estimator_type)
                     }
                   ))
