#' summary qntmap class data.
#'
#' @param object qntmap class data
#' @param ... other arguments are discarded
#'
#' @importFrom pipeR %>>%
#' @importFrom dplyr bind_rows
#'
#'@export
#'
summary.qntmap <- function(object, ...) {
  object %>>%
    lapply(`[[`, 'wt') %>>%
    lapply(unlist) %>>%
    lapply(summary) %>>%
    lapply(as.list) %>>%
    bind_rows(.id = 'element') %>>%
    return()
}
