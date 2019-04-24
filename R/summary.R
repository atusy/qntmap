#' @name summary
#' @inherit base::summary
NULL

#' @rdname summary
#' @aliases summary.qm_cluster
#' @section summary.qm_cluster: Returns abundance ratios of clusters.
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
#' @section summary.qntmap: summary qntmap class data.
#' @importFrom dplyr bind_rows
#' @inheritParams round
#' @export
summary.qntmap <- function(object, digits = 2L, ...) {
  on.exit(message('\n', 'Note that Total is not sum each column'))
  object %>>%
    lapply(function(x) as.list(round(summary(unlist(x[['wt']])), digits))) %>>%
    bind_rows(.id = 'Element') %>>%
    as.data.frame %>>%
    print
}
