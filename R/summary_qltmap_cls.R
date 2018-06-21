#' summary qltmap_cls data
#'
#' Returns abundance ratios of clusters.
#'
#' @param object qntmap class data
#' @param ... other arguments are discarded
#'
#' @importFrom pipeline 
#'
#'@export
#'
summary.qltmap_cls <- function(object, ...) {pipeline({
  object$membership 
    colSums 
    `/`(sum(.))
})}
