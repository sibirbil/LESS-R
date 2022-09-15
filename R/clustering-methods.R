#' @title KMeans Clustering
#'
#' @description Wrapper R6 Class of stats::kmeans function that can be used for LESSRegressor and LESSClassifier
#'
#' @param n_clusters the number of clusters. A random set of (distinct) rows in X is chosen as the initial centres (default to 8)
#' @param n_init how many random sets should be chosen? (default to 10)
#' @param max_iter the maximum number of iterations allowed (default to 300).
#' @param random_state seed number to be used for fixing the randomness (default to NULL).
#'
#' @return R6 Class of KMeans
#' @importFrom stats kmeans
#' @seealso [stats::kmeans()]
#' @export
KMeans <- R6::R6Class(classname = "KMeans",
                      inherit = BaseEstimator,
                      private = list(
                        model = NULL,
                        n_clusters = NULL,
                        n_init = NULL,
                        max_iter = NULL,
                        cluster_centers = NULL,
                        labels = NULL,
                        random_state = NULL
                      ),
                      public = list(
                        #' @description Creates a new instance of R6 Class of KMeans
                        #'
                        #' @examples
                        #' km <- KMeans$new()
                        #' km <- KMeans$new(n_clusters = 10)
                        #' km <- KMeans$new(n_clusters = 10, random_state = 100)
                        initialize = function(n_clusters = 8, n_init = 10, max_iter = 300, random_state = NULL){
                          private$n_clusters = n_clusters
                          private$n_init = n_init
                          private$max_iter = max_iter
                          private$random_state = random_state
                        },
                        #' @description Perform k-means clustering on a data matrix.
                        #'
                        #' @param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
                        #'
                        #' @return Fitted R6 class of KMeans() that has 'cluster_centers' and 'labels' attributes
                        #'
                        #' @examples
                        #' data(abalone)
                        #' km <- KMeans$new()
                        #' km$fit(abalone)
                        fit = function(X){
                          set.seed(private$random_state)
                          private$model <- kmeans(X, centers = private$n_clusters, iter.max = private$max_iter, nstart = private$n_init)
                          private$cluster_centers <- private$model$centers
                          private$labels <- private$model$cluster
                          invisible(self)
                        },
                        #' @description Auxiliary function returning the cluster centers
                        #' @examples
                        #' print(km$get_cluster_centers())
                        get_cluster_centers = function(){
                          return(private$cluster_centers)
                        },
                        #' @description Auxiliary function returning a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
                        #' @examples
                        #' print(km$get_labels())
                        get_labels = function(){
                          return(private$labels)
                        }
                      ))

#' @title Hierarchical Clustering
#'
#' @description Wrapper R6 Class of stats::hclust function that can be used for LESSRegressor and LESSClassifier
#'
#' @param linkage the agglomeration method to be used. This should be (an unambiguous abbreviation of) one of
#' "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC)
#' (defaults to ward.D2).
#' @param n_clusters the number of clusters (defaults to 8).
#'
#' @return R6 Class of HierarchicalClustering
#' @importFrom stats hclust
#' @seealso [stats::hclust()]
#' @export
HierarchicalClustering <- R6::R6Class(classname = "HierarchicalClustering",
                                      inherit = BaseEstimator,
                                      private = list(
                                        model = NULL,
                                        n_clusters = NULL,
                                        cluster_centers = NULL,
                                        linkage = NULL,
                                        labels = NULL
                                      ),
                                      public = list(
                                        #' @description Creates a new instance of R6 Class of HierarchicalClustering
                                        #'
                                        #' @examples
                                        #' hc <- HierarchicalClustering$new()
                                        #' hc <- HierarchicalClustering$new(n_clusters = 10)
                                        #' hc <- HierarchicalClustering$new(n_clusters = 10, linkage = "complete")
                                        initialize = function(linkage = "ward.D2", n_clusters = 8){
                                          private$linkage <- linkage
                                          private$n_clusters <- n_clusters
                                        },
                                        #' @description Perform hierarchical clustering on a data matrix.
                                        #'
                                        #' @param X numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
                                        #'
                                        #' @return Fitted R6 class of HierarchicalClustering() that has 'labels' attribute
                                        #'
                                        #' @examples
                                        #' data(abalone)
                                        #' hc <- HierarchicalClustering$new()
                                        #' hc$fit(abalone)
                                        fit = function(X){
                                          private$model <- stats::hclust(dist(X), method = private$linkage)
                                          private$labels <- unname(cutree(private$model, k = private$n_clusters))
                                          invisible(self)
                                        },
                                        #' @description Auxiliary function returning the cluster centers
                                        #' @examples
                                        #' print(hc$get_cluster_centers())
                                        get_cluster_centers = function(){
                                          return(private$cluster_centers)
                                        },
                                        #' @description Auxiliary function returning a vector of integers (from 1:k) indicating the cluster to which each point is allocated.
                                        #' @examples
                                        #' print(hc$get_labels())
                                        get_labels = function(){
                                          return(private$labels)
                                        }
                                      ))
