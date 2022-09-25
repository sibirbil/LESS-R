#' @title KDTree - Nearest Neighbor Search
#'
#' @description Wrapper R6 Class of RANN::nn2 function that can be used for LESSRegressor and LESSClassifier
#'
#' @param X An \strong{M x d} data.frame or matrix, where each of the \strong{M} rows is a point or a (column) vector (where \strong{d=1}).
#'
#' @return R6 Class of KDTree
#' @importFrom RANN nn2
#' @seealso [RANN::nn2()]
#' @export
KDTree <- R6::R6Class(classname = "KDTree",
                      private = list(
                        X = NULL
                      ),
                      public = list(
                        #' @description Creates a new instance of R6 Class of KDTree
                        #'
                        #' @examples
                        #' data(abalone)
                        #' kdt <- KDTree$new(abalone[1:100,])
                        initialize = function(X = NULL) {
                          private$X = X
                        },
                        #' @description Finds the p number of near neighbours for each point in an input/output dataset. The advantage of the kd-tree is that it runs in O(M log M) time.
                        #'
                        #' @param query_X A set of \strong{N x d} points that will be queried against \code{X}. \strong{d}, the number of columns, must be the same as \code{X}.
                        #' If missing, defaults to  \code{X}.
                        #' @param k The maximum number of nearest neighbours to compute (deafults to 1).
                        #'
                        #' @return A \code{list} of length 2 with elements:\tabular{ll}{
                        #'    \code{nn.idx} \tab A \strong{N x k} integer matrix returning the near neighbour indices. \cr
                        #'    \tab \cr
                        #'    \code{nn.dists} \tab A \strong{N x k} matrix returning the near neighbour Euclidean distances \cr
                        #' }
                        #'
                        #' @examples
                        #' res <- kdt$query(abalone[1:3,], k=2)
                        #' print(res)
                        query = function(query_X = private$X, k=1){
                          # query the tree for the k nearest neighbors
                          query <- as.matrix(query_X)
                          RANN::nn2(data = private$X, query = query, k = k)
                        }
                      ))

#' @title CoverTree - Nearest Neighbor Search
#'
#' @description Wrapper R6 Class of FNN::get.knnx function that can be used for LESSRegressor and LESSClassifier
#'
#' @details The cover tree is O(n) space data structure which allows us to answer queries in the same O(log(n)) time as kd tree given a fixed intrinsic dimensionality.
#' Templated code from \url{https://hunch.net/~jl/projects/cover_tree/cover_tree.html} is used.
#'
#' @param X An \strong{M x d} data.frame or matrix, where each of the \strong{M} rows is a point or a (column) vector (where \strong{d=1}).
#'
#' @return R6 Class of CoverTree
#' @importFrom FNN get.knnx
#' @seealso [FNN::get.knnx()]
#' @export
CoverTree <- R6::R6Class(classname = "CoverTree",
                         private = list(
                           X = NULL
                         ),
                         public = list(
                           #' @description Creates a new instance of R6 Class of CoverTree
                           #'
                           #' @examples
                           #' data(abalone)
                           #' ct <- CoverTree$new(abalone[1:100,])
                           initialize = function(X = NULL) {
                             private$X = X
                           },
                           #' @description Finds the p number of near neighbours for each point in an input/output dataset.
                           #'
                           #' @param query_X A set of \strong{N x d} points that will be queried against \code{X}. \strong{d}, the number of columns, must be the same as \code{X}.
                           #' If missing, defaults to  \code{X}.
                           #' @param k The maximum number of nearest neighbours to compute (deafults to 1).
                           #'
                           #' @return A \code{list} of length 2 with elements:\tabular{ll}{
                           #'    \code{nn.idx} \tab A \strong{N x k} integer matrix returning the near neighbour indices. \cr
                           #'    \tab \cr
                           #'    \code{nn.dists} \tab A \strong{N x k} matrix returning the near neighbour Euclidean distances \cr
                           #' }
                           #'
                           #' @examples
                           #' res <- ct$query(abalone[1:3,], k=2)
                           #' print(res)
                           query = function(query_X = private$X, k=1){
                             # query the tree for the k nearest neighbors
                             query <- as.matrix(query_X)
                             FNN::get.knnx(data = private$X, query = query, k = k, algorithm = "cover_tree")
                           }
                         ))
