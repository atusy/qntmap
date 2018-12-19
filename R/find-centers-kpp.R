#' Generate initial centroids for clustering randomly
#'
#' @param x 
#'   An object which can be coerced to [`matrix`], 
#'   typically [`matrix`] itself or [`data.frame`].
#' @param k A number of clusters.
#' @param given Given centers. Not yet implemented.
#' 
#' @importFrom matrixStats colSums2
#' 
#'
find_centers_kpp <- function(x, k, given = NULL) {
  # check parameters
  if(!is.null(given) && nrow(x) < k) 
    stop('Number of given centroids must be smaller than k')
  if(k < 2) stop('k must be a numeric >= 2')
  
  # transform parameters
  x <- as.matrix(x)
  x_trans <- t(x)

  # calculation
  n <- nrow(x) # number of data points
  n_seq <- seq(n)
  centers <- numeric(k) # IDs of centers
  distances <- matrix(numeric(n * (k - 1)), ncol = k - 1) 
    # distances[i, j]: The distance between x[i,] and x[centers[j],]
  pr <- rep(1, n) # probability for sampling centers
  for (i in seq(k - 1)) {
    centers[i] <- sample.int(n, 1, prob = pr) # Pick up the ith center
    distances[, i] <- colSums2((x_trans - x[centers[i], ]) ^ 2) 
      # Compute (the square of) distances to the center
    pr <- distances[cbind(n_seq, max.col(-distances[, 1:i, drop = FALSE]))] 
      # Compute probaiblity for the next sampling
  }
  centers[k] <- sample.int(n, 1, prob = pr)

  data.frame(phase = centers, x[centers, ])
}
