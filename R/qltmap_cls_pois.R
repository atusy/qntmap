#' Poisson distribution based custering based on `PoiClaClu:Classify()`
#' @param x data frame or matrix
#' @param center data frame or matrix
#' @importFrom PoiClaClu Classify
#' @export
cluster <- function(x, centers) {
  x_trans <- t(x)
  y <- pipeline({
    centers
    apply(1, function(y) colSums(x_trans - y) ^ 2)
    apply(1, which.min)
  })
  rm(x_trans)
  Classify(x, y, x)
}

#' cluster mapping data into mineral species
#'
#' @param centers a path to csv file telling initial centers of clusters
#' @param xmap default to NULL
#' @param elements If NULL, all mapped elements are used for clustering. Specifiyng elements may reduce analytical time.
#' @param saving TRUE or FALSE to save result
#' @param integration TRUE or FALSE to integrate same phase with obiously different compositions. For example, when there are clusters named as Pl_NaRich and Pl_NaPoor, they are integrated to Pl.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom pipeR pipeline
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#'
#'
#' @export
cluster_xmap <- function(
  xmap,
  centers,
  dir_map,
  elements = intersect(colnames(xmap), colnames(centers)),
  saving = TRUE,
  integration = TRUE
) {

  centers$phase <- as.character(centers$phase)
  dims <- dim(xmap[[1]])

  x <- pipeline({
    xmap[elements]
    lapply(unlist, use.names = FALSE)
    as.data.frame
  })
  
  rm(xmap)

  # Classify by PoiClaClu 
  result <- cluster(x, centers = centers[, elements])

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
    `-`(apply(., 1, max))
    exp
    `/`(rowSums(.))
    as.matrix
  })

  if(nrow(centers) == ncol(result$membership)) {
    colnames(result$membership) <- centers$phase
  } else {
    if(ncol(result$membership) == 1) {
      colnames(result$membership) <- names(result$ytehat[1])
    } else {
      TF <- !duplicated(result$cluster)
      colnames(result$membership)[apply(result$membership[TF, ], 1, which.max)] <- result$cluster[TF]
      rm(TF)
    }
    missings <- setdiff(centers$phase, colnames(result$membership))
    result$membership <- cbind(
        result$membership,
        matrix(
          0,
          nrow(.),
          ncol = length(missings),
          dimnames = list(NULL, missings)
        )
      )[, centers$phase]
    rm(missings)
  }
  
  # additional informations
  class(result) <- c('xmap_cls', 'list')
  result$date <- format(Sys.time(), "%y%m%d_%H%M")
  result$dims <- dims
  result$elements <- elements

  components <- c('ytehat', 'cluster', 'center', 'membership', 'date', 'dims', 'elements')
  if(saving) qltmap_cls_save(result, 'pois', components)

  if(integration && any(grepl('_', colnames(result$membership))))
      result <- cluster_group(result, saving = saving)

  result
}

