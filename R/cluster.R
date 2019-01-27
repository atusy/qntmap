#' Poisson distribution based custering based on [`PoiClaClu::Classify()`]
#' 
#' @param centers c-by-p matrix returned by [`find_centers()`] or by manually; 
#'   c clusters and p features. 
#'   Used to guess initial centers (or centroids) of clusters. 
#'   A value returned by , typically [`data.frame`] or [`matrix`], 
#'   indicating initial guess centers (or centroids) or clusters. 
#'   See [`find_centers()`].
#' @inheritParams PoiClaClu::Classify
#' @inheritDotParams PoiClaClu::Classify -x -y -xte
#' @inherit PoiClaClu::Classify return references
#' @seealso [PoiClaClu::Classify()], [find_centers()]
#'
#' @importFrom PoiClaClu Classify
#' @importFrom matrixStats colSums2
#' @export
cluster <- function(x, centers, xte = x, ...) {
  x_trans <- t(x)
  y <- centers %>>%
    apply(1L, function(y) colSums2(square(x_trans - y))) %>>%
    apply(1L, which.min)
  rm(x_trans)
  Classify(x, y, xte, ...)
}



#' Cluster mapping data into mineral species
#'
#' @inheritParams cluster
#' @param xmap 
#'   A `qm_xmap` class object returned by [`read_xmap()`]
#' @param elements 
#'   A character vector to chose elements to be utilized in cluster analysis. 
#'   `NULL` (default) selects as much elements as possible.
#' @param saving 
#'   `TRUE` or `FALSE` to save result.
#' @param group_cluster 
#'   `FALSE` (default) or `TRUE` to integrate same phase subgrouped using suffix.
#'   For example, 
#'   clusters named "Pl_NaRich" and "Pl_NaPoor" are integrated to "Pl" cluster .
#' @inheritDotParams PoiClaClu::Classify -x -y
#'
#' @importFrom dplyr group_by mutate mutate_if summarize ungroup
#' @importFrom tidyr gather spread
#' @importFrom matrixStats rowMaxs rowSums2
#'
#' @export
cluster_xmap <- function(
  xmap,
  centers,
  elements = intersect(names(xmap), colnames(centers)),
  saving = TRUE,
  group_cluster = FALSE,
  ...
) {
  dir_map <- attr(xmap, 'dir_map')
  centers$phase <- as.character(centers$phase)
  dims <- dim(xmap[[1]])

  x <- as.data.frame(lapply(xmap[elements], unlist, use.names = FALSE))

  rm(xmap)

  # Classify by PoiClaClu 
  result <- cluster(
      x, centers = centers[, elements], ...
    )[c('ytehat', 'discriminant', "xte")]

  # give phase names to result$ytehat
  names(result$ytehat) <- result$cluster <- centers$phase[result$ytehat]
  
  # Find representative values of each clusters (~ centers)
  result$center <- as.data.frame(lapply(result$xte, as.double)) %>>%
    mutate(phase = result$cluster) %>>%
    gather(elm, val, -phase) %>>%
    group_by(phase, elm) %>>%
    summarise(val = median(val)) %>>%
    ungroup %>>%
    spread(elm, val)
  
  result$xte <- NULL

  # estimate membership of each clusters
  result$membership <- result$discriminant %>>%
    `-`(rowMaxs(.)) %>>%
    exp %>>%
    `/`(rowSums2(.))

  result$discriminant <- NULL

  if(nrow(centers) == ncol(result$membership)) {
    colnames(result$membership) <- centers$phase
  } else {
    if(ncol(result$membership) == 1L) {
      colnames(result$membership) <- result$cluster[1L]
    } else {
      TF <- !duplicated(result$cluster)
      colnames(result$membership)[apply(result$membership[TF, ], 1L, which.max)] <-
        result$cluster[TF]
      rm(TF)
    }
    missings <- setdiff(centers$phase, colnames(result$membership))
    result$membership <- cbind(
        result$membership,
        matrix(
          0L,
          nrow = nrow(result$membership),
          ncol = length(missings),
          dimnames = list(NULL, missings)
        )
      )[, centers$phase]
    rm(missings)
  }
  
  # additional informations
  class(result) <- c('qm_cluster', 'list')
  result$date <- format(Sys.time(), "%y%m%d_%H%M")
  result$dims <- dims
  result$elements <- elements
  result$dir_map <- dir_map

  if(group_cluster && any(grepl('_', colnames(result$membership))))
      return(group_cluster(result, saving = saving))

  save4qm(result, 'pois', saving)
}
