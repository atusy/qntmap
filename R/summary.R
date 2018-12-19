#' @name summary_methods
#' @title summary methods
#' @param object object to be summarized
#' @param ... other arguments to control summary results
NULL

#' @rdname summary_methods
#' @aliases summary.qm_cluster
#' @section summary.qm_cluster: Returns abundance ratios of clusters.
#' @importFrom matrixStats colSums2
#' @export
summary.qm_cluster <- function(object, ...) {
  object$membership %>>%
    colSums2 %>>%
    `/`(sum(.))
}

#' @section summary.qntmap: summary qntmap class data.
#' @importFrom dplyr bind_rows
#' @export
summary.qntmap <- function(object, ...) {
  on.exit(message('\n', 'Note that Total is not sum each column'))
  object %>>%
    lapply(`[[`, 'wt') %>>%
    lapply(unlist) %>>%
    lapply(summary) %>>%
    lapply(round, 2L) %>>%
    lapply(as.list) %>>%
    bind_rows(.id = 'Element') %>>%
    as.data.frame() %>>%
    print()
}
