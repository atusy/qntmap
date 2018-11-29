#' Poisson distribution based custering based on [`PoiClaClu::Classify()`]
#' 
#' @param centers c-by-p matrix returned by [`find_centers()`] or by manually; 
#' c clusters and p features. 
#' Used to guess initial centers (or centroids) of clusters. 
#' A value returned by , typically [`data.frame`] or [`matrix`], 
#' indicating initial guess centers (or centroids) or clusters. 
#' See [`find_centers()`].
#' @inheritParams PoiClaClu::Classify
#' @inheritDotParams PoiClaClu::Classify -x -y -xte
#' @inherit PoiClaClu::Classify return references
#' @seealso [PoiClaClu::Classify()], [find_centers()]
#'
#' @importFrom PoiClaClu Classify
#' @export
cluster <- function(x, centers, xte = NULL, ...) {
  x_trans <- t(x)
  y <- pipeline({
    centers
    apply(1L, function(y) colSums((x_trans - y) ^ 2L))
    apply(1L, which.min)
  })
  rm(x_trans)
  Classify(x, y, `if`(is.null(xte), x, xte), ...)
}



#' Cluster mapping data into mineral species
#'
#' @inheritParams cluster
#' @param xmap a `qm_xmap` class object returned by [`read_xmap()`]
#' @param elements 
#' A character vector to chose elements to be utilized in cluster analysis. 
#' `NULL` (default) selects as much elements as possible.
#' @param saving `TRUE` or `FALSE` to save result.
#' @param group_cluster 
#' `FALSE` (default) or `TRUE` to integrate same phase subgrouped using suffix.
#' For example, 
#' clusters named "Pl_NaRich" and "Pl_NaPoor" are integrated to "Pl" cluster .
#'
#' @importFrom dplyr group_by ungroup
#' @importFrom pipeR pipeline
#' @importFrom tidyr gather spread
#' @importFrom matrixStats rowMaxs
#'
#' @export
cluster_xmap <- function(
  xmap,
  centers,
  elements = intersect(names(xmap), colnames(centers)),
  saving = TRUE,
  group_cluster = FALSE
) {
  dir_map <- attr(xmap, 'dir_map')
  centers$phase <- as.character(centers$phase)
  dims <- dim(xmap[[1]])

  x <- as.data.frame(lapply(xmap[elements], unlist, use.names = FALSE))

  rm(xmap)

  # Classify by PoiClaClu 
  result <- cluster(x, centers = centers[, elements])[c('ytehat', 'discriminant')]

  # give phase names to result$ytehat
  names(result$ytehat) <- result$cluster <- centers$phase[result$ytehat]
  
  # Find representative values of each clusters (~ centers)
  result$center <- pipeline({
    x
    lapply(as.double)
    as.data.frame
    mutate(phase = result$cluster)
    gather(elm, val, -phase)
    group_by(phase, elm)
    summarise(val = median(val))
    ungroup
    spread(elm, val)
  })

  # estimate membership of each clusters
  result$membership <- pipeline({
    result$discriminant
    `-`(rowMaxs(.))
    exp
    `/`(rowSums(.))
    # as.matrix # maybe not needed
  })
  
  result$discriminant <-NULL

  if(nrow(centers) == ncol(result$membership)) {
    colnames(result$membership) <- centers$phase
  } else {
    if(ncol(result$membership) == 1L) {
      colnames(result$membership) <- names(result$ytehat[1L])
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



#' (DEPRECATED) Use `cluster_xmap()`
#' 
#' @param centers_initial 
#' Equivalent to `centers_initial` parameter of `cluster_xmap`
#' @param qltmap Equivalent to `xmap` parameter of `cluster_xmap`
#' @param wd Path to the working directory.
#' @param integration Equivalent to `group_cluster` parameter of `cluster_xmap`
#' @inheritParams cluster_xmap
#' @inheritParams save4qm
#' 
#' @export
qntmap_cls_pois <- function(
  centers_initial,
  qltmap = NULL,
  elements = NULL,
  wd = '.',
  saving = TRUE,
  integration = TRUE
) {
  .Deprecated('cluster_xmap')
  
  cd <- getwd()
  on.exit(setwd(cd))
  setwd(wd)
  
  # load data
  if(is.character(qltmap)) qltmap <- read_xmap(qltmap)
  if(is.character(centers_initial)) 
    centers_initial <- fread(centers_initial)
  if(is.null(elements)) elements <- 
    intersect(names(qltmap), colnames(centers_initial))
  
  cluster_xmap(
    qltmap,
    centers_initial,
    elements,
    saving,
    integration
  )
}