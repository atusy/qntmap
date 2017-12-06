#' Generate initial centroids for clustering randomly
#'
#' @param k number of clusters
#' @param given given centers
#' @param qltmap path to the .RDS file which compiles mapping data
#' @param wd working directory which contains mapping data
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table fwrite
#'
#' @export
qltmap_cls_centers_random <- function(k, given = NULL, qltmap = NULL, wd = NULL) {
  cd <- getwd()
  on.exit(setwd(cd))
  if(!is.null(wd)) setwd(wd)

  #qltmap: import mapping data
  if(!is.matrix(qltmap)) qltmap <- as.matrix(as.data.table((qltmap_load())))

  n <- nrow(qltmap) # number of data points
  centers <- numeric(k) # IDs of centers
  distances <- matrix(numeric(n * (k - 1)), ncol = k - 1) # distances[i, j]: The distance between x[i,] and x[centers[j],]
  pr <- rep(1, n) # probability for sampling centers
  for (i in 1:(k - 1)) {
    centers[i] <- sample.int(n, 1, prob = pr) # Pick up the ith center
    distances[, i] <- colSums((t(qltmap) - qltmap[centers[i], ])^2) # Compute (the square of) distances to the center
    pr <- distances[cbind(1:n, max.col(-distances[, 1:i, drop = FALSE]))] # Compute probaiblity for the next sampling
  }
  centers[k] <- sample.int(n, 1, prob = pr)

  fwrite(data.table(phase = centers, qltmap[centers, ]), 'centers_random0.csv')
}
















