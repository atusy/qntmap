#' summary qltmap_cls data
#'
#' Returns abundance ratios of clusters.
#'
#' @param object qntmap class data
#' @param ... other arguments are discarded
#'
#' @importFrom pipeR %>>%
#'
#'@export
#'
summary.qltmap_cls <- function(object, ...) {
  object$membership %>>%
    colSums %>>%
    `/`(sum(.))
}
