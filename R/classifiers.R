#' @title DecisionTreeClassifier
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
#' @return R6 Class of DecisionTreeClassifier
#' @importFrom rpart rpart
#' @seealso [rpart::rpart()]
#' @export
DecisionTreeClassifier <- R6::R6Class(classname = "DecisionTreeClassifier",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "classifier",
                                       model = NULL,
                                       min_samples_split = NULL,
                                       min_samples_leaf = NULL,
                                       cp = NULL,
                                       xval = NULL,
                                       surrogate_style = NULL,
                                       max_depth = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of DecisionTreeClassifier
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' dt <- DecisionTreeClassifier$new(min_samples_split = 10)
                                       #' dt <- DecisionTreeClassifier$new(min_samples_leaf = 6, cp = 0.01)
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
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
                                       #'
                                       #' @return Fitted R6 Class of DecisionTreeClassifier
                                       #'
                                       #' @examples
                                       #' data(iris)
                                       #' split_list <- train_test_split(iris, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' dt$fit(X_train, y_train)
                                       fit = function(X, y) {
                                         df <- prepareDataset(X, y)
                                         private$model <- rpart::rpart(y ~ ., data = df, method = "class", minsplit = private$min_samples_split,
                                                                       minbucket = private$min_samples_leaf, cp = private$cp, xval = private$xval,
                                                                       surrogatestyle = private$surrogate_style, maxdepth = private$max_depth)
                                         private$isFitted <- TRUE
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return Factor of the predict classes.
                                       #'
                                       #' @examples
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' dt$fit(X_train, y_train)
                                       #' preds <- dt$predict(X_test)
                                       #'
                                       #' dt <- DecisionTreeClassifier$new()
                                       #' preds <- dt$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- DecisionTreeClassifier$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(caret::confusionMatrix(data=preds, reference = factor(y_test)))
                                       predict = function(X0) {
                                         check_is_fitted(self)
                                         data <- prepareXset(X0)
                                         y_pred <- predict(private$model, data, type = "class")
                                         return(y_pred)
                                       },
                                       #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                       get_estimator_type = function() {
                                         return(private$estimator_type)
                                       }
                                     ))

#' @title Support Vector Classification
#'
#' @description Wrapper R6 Class of e1071::svm function that can be used for LESSRegressor and LESSClassifier
#'
#' @param scale A logical vector indicating the variables to be scaled. If scale is of length 1, the value is recycled as many times as needed.
#' Per default, data are scaled internally (both x and y variables) to zero mean and unit variance.
#' The center and scale values are returned and used for later predictions (default: TRUE)
#' @param kernel The kernel used in training and predicting. Possible values are: "linear", "polynomial", "radial", "sigmoid" (default is "radial")
#' @param degree Parameter needed for kernel of type polynomial (default: 3)
#' @param gamma Parameter needed for all kernels except linear (default: 1/(data dimension))
#' @param coef0 Parameter needed for kernels of type polynomial and sigmoid (default: 0)
#' @param cost Cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation (default: 1)
#' @param cache_size Cache memory in MB (default: 40)
#' @param tolerance Tolerance of termination criterion (default: 0.001)
#' @param epsilon Epsilon in the insensitive-loss function (default: 0.1)
#' @param shrinking Option whether to use the shrinking-heuristics (default: TRUE)
#' @param cross If a integer value k>0 is specified, a k-fold cross validation on the training data is performed to assess the quality of the model:
#' the accuracy rate for classification and the Mean Squared Error for regression (default: 0)
#' @param fitted Logical indicating whether the fitted values should be computed and included in the model or not (default: TRUE)
#' @param probability Logical indicating whether the model should allow for probability predictions (default: FALSE)
#'
#' @return R6 Class of SVC
#' @seealso [e1071::svm()]
#' @importFrom e1071 svm
#' @export
SVC <- R6::R6Class(classname = "SVC",
                   inherit = SklearnEstimator,
                   private = list(
                     estimator_type = "classifier",
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
                     #' @description Creates a new instance of R6 Class of SVC
                     #'
                     #' @examples
                     #' svc <- SVC$new()
                     #' svc <- SVC$new(kernel = "polynomial")
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
                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
                     #'
                     #' @return Fitted R6 Class of SVC
                     #'
                     #' @examples
                     #' data(iris)
                     #' split_list <- train_test_split(iris, test_size =  0.3)
                     #' X_train <- split_list[[1]]
                     #' X_test <- split_list[[2]]
                     #' y_train <- split_list[[3]]
                     #' y_test <- split_list[[4]]
                     #'
                     #' svc <- SVC$new()
                     #' svc$fit(X_train, y_train)
                     fit = function(X, y){
                       df <- prepareDataset(X, y)
                       gamma = if (is.vector(X)) 1 else 1 / ncol(X)
                       df$y <- as.factor(df$y)
                       private$model <- e1071::svm(y ~ ., data = df, type = 'C-classification', scale = private$scale, kernel = private$kernel,
                                                   degree = private$degree, gamma = if (is.null(private$gamma)) gamma else private$gamma, coef0 = private$coef0,
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
                     #' @return Factor of the predict classes.
                     #'
                     #' @examples
                     #' svc <- SVC$new()
                     #' svc$fit(X_train, y_train)
                     #' preds <- svc$predict(X_test)
                     #'
                     #' svc <- SVC$new()
                     #' preds <- svc$fit(X_train, y_train)$predict(X_test)
                     #'
                     #' preds <- SVC$new()$fit(X_train, y_train)$predict(X_test)
                     #' print(caret::confusionMatrix(data=preds, reference = factor(y_test)))
                     predict = function(X0){
                       check_is_fitted(self)
                       data <- prepareXset(X0)
                       y_pred <- predict(private$model, data)
                       return(y_pred)
                     },
                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                     get_estimator_type = function() {
                       return(private$estimator_type)
                     }
                   ))

#' @title RandomForestClassifier
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
#' @return R6 Class of RandomForestClassifier
#' @importFrom randomForest randomForest
#' @seealso [randomForest::randomForest()]
#' @export
RandomForestClassifier <- R6::R6Class(classname = "RandomForestClassifier",
                                     inherit = SklearnEstimator,
                                     private = list(
                                       estimator_type = "classifier",
                                       model = NULL,
                                       n_estimators = NULL,
                                       random_state = NULL,
                                       min_samples_leaf = NULL,
                                       max_leaf_nodes = NULL
                                     ),
                                     public = list(
                                       #' @description Creates a new instance of R6 Class of RandomForestClassifier
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestClassifier$new()
                                       #' rf <- RandomForestClassifier$new(n_estimators = 500)
                                       #' rf <- RandomForestClassifier$new(n_estimators = 500, random_state = 100)
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
                                       #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
                                       #'
                                       #' @return Fitted R6 Class of RandomForestClassifier
                                       #'
                                       #' @examples
                                       #' data(iris)
                                       #' split_list <- train_test_split(iris, test_size =  0.3)
                                       #' X_train <- split_list[[1]]
                                       #' X_test <- split_list[[2]]
                                       #' y_train <- split_list[[3]]
                                       #' y_test <- split_list[[4]]
                                       #'
                                       #' rf <- RandomForestClassifier$new()
                                       #' rf$fit(X_train, y_train)
                                       fit = function(X, y){
                                         df <- prepareDataset(X, y)
                                         df$y <- as.factor(df$y)
                                         set.seed(private$random_state)
                                         private$model <- randomForest::randomForest(y ~ ., data = df, ntree = private$n_estimators,
                                                                                     nodesize = private$min_samples_leaf,
                                                                                     maxnodes = private$max_leaf_nodes,
                                                                                     importance = TRUE,
                                                                                     proximity = TRUE)
                                         private$isFitted <- TRUE
                                         invisible(self)
                                       },
                                       #' @description Predict regression value for X0.
                                       #'
                                       #' @param X0 2D matrix or dataframe that includes predictors
                                       #'
                                       #' @return Factor of the predict classes.
                                       #'
                                       #' @examples
                                       #' rf <- RandomForestClassifier$new()
                                       #' rf$fit(X_train, y_train)
                                       #' preds <- rf$predict(X_test)
                                       #'
                                       #' rf <- RandomForestClassifier$new()
                                       #' preds <- rf$fit(X_train, y_train)$predict(X_test)
                                       #'
                                       #' preds <- RandomForestClassifier$new()$fit(X_train, y_train)$predict(X_test)
                                       #' print(caret::confusionMatrix(data=preds, reference = factor(y_test)))
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

#' @title KNeighborsClassifier
#'
#' @description Wrapper R6 Class of caret::knnreg function that can be used for LESSRegressor and LESSClassifier
#'
#' @param k Number of neighbors considered (defaults to 5).
#'
#' @return R6 Class of KNeighborsClassifier
#' @seealso [caret::knn3()]
#' @importFrom caret knn3
#' @export
KNeighborsClassifier <- R6::R6Class(classname = "KNeighborsClassifier",
                                   inherit = SklearnEstimator,
                                   private = list(
                                     estimator_type = "classifier",
                                     model = NULL,
                                     k = NULL
                                   ),
                                   public = list(
                                     #' @description Creates a new instance of R6 Class of KNeighborsClassifier
                                     #'
                                     #' @examples
                                     #' knc <- KNeighborsClassifier$new()
                                     #' knc <- KNeighborsClassifier$new(k = 5)
                                     initialize = function(k = 5){
                                       private$k = k
                                     },
                                     #' @description Fit the k-nearest neighbors regressor from the training set (X, y).
                                     #'
                                     #' @param X 2D matrix or dataframe that includes predictors
                                     #' @param y 1D vector or (n,1) dimensional matrix/dataframe that includes labels
                                     #'
                                     #' @return Fitted R6 Class of KNeighborsClassifier
                                     #'
                                     #' @examples
                                     #' data(iris)
                                     #' split_list <- train_test_split(iris, test_size =  0.3)
                                     #' X_train <- split_list[[1]]
                                     #' X_test <- split_list[[2]]
                                     #' y_train <- split_list[[3]]
                                     #' y_test <- split_list[[4]]
                                     #'
                                     #' knc <- KNeighborsClassifier$new()
                                     #' knc$fit(X_train, y_train)
                                     fit = function(X, y){
                                       df <- prepareDataset(X, y)
                                       df$y <- as.factor(df$y)
                                       private$model <- caret::knn3(y ~ ., data = df, k=private$k)
                                       private$isFitted <- TRUE
                                       invisible(self)
                                     },
                                     #' @description Predict regression value for X0.
                                     #'
                                     #' @param X0 2D matrix or dataframe that includes predictors
                                     #'
                                     #' @return Factor of the predict classes.
                                     #'
                                     #' @examples
                                     #' knc <- KNeighborsClassifier$new()
                                     #' knc$fit(X_train, y_train)
                                     #' preds <- knc$predict(X_test)
                                     #'
                                     #' knc <- KNeighborsClassifier$new()
                                     #' preds <- knc$fit(X_train, y_train)$predict(X_test)
                                     #'
                                     #' preds <- KNeighborsClassifier$new()$fit(X_train, y_train)$predict(X_test)
                                     #' print(caret::confusionMatrix(data=factor(preds), reference = factor(y_test)))
                                     predict = function(X0){
                                       check_is_fitted(self)
                                       data <- prepareXset(X0)
                                       y_pred <- predict(private$model, data, type = "class")
                                       return(factor(y_pred))
                                     },
                                     #' @description Auxiliary function returning the estimator type e.g 'regressor', 'classifier'
                                     get_estimator_type = function() {
                                       return(private$estimator_type)
                                     }
                                   ))

