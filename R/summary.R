#' @name summary
#' @inherit base::summary
NULL

#' @rdname summary
#' @aliases summary.qm_cluster
#' @section summary.qm_cluster: Summarize abundance ratios of clusters.
#' @importFrom matrixStats colSums2
#' @export
#' @inheritParams summary
summary.qm_cluster <- function(object, ...) {
  object$membership %>>%
    colSums2 %>>%
    `/`(sum(.))
}

#' @rdname summary
#' @aliases summary.qntmap
#' @section summary.qntmap: Summarize qntmap class data.
#' @inheritParams base::round
#' @export
summary.qntmap <- function(object, digits = 2L, ...) {
  object %>>%
    lapply(function(x) as.list(round(summary(unlist(x[["wt"]])), digits))) %>>%
    bind_rows(.id = "Element") %>>%
    as.data.frame %>>%
    print
}
