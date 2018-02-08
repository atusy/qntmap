#' summary qntmap class data.
#'
#' @param x qntmap class data
#'
#' @importFrom pipeR `%>>%`
#' @importFrom dplyr bind_rows
#'
#'@export
#'
summary.qntmap <- function(x) {
  x %>>%
    lapply(`[[`, 'wt') %>>%
    lapply(unlist) %>>%
    lapply(summary) %>>%
    lapply(as.list) %>>%
    bind_rows(.id = 'element') %>>%
    return()
}
