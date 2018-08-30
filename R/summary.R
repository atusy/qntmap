#' summary qltmap_cls data
#'
#' Returns abundance ratios of clusters.
#'
#' @param object qntmap class data
#' @param ... other arguments are discarded
#'
#' @importFrom pipeR pipeline 
#'
#' @export
#'
summary.xmap_cls <- function(object, ...) {pipeline({
  object$membership 
  colSums 
  `/`(sum(.))
})}

#' summary qntmap class data.
#'
#' @param object qntmap class data
#' @param ... other arguments are discarded
#'
#' @importFrom pipeR pipeline
#' @importFrom dplyr bind_rows
#'
#'@export
summary.qntmap <- function(object, ...) {pipeline({
  object 
  lapply(`[[`, 'wt') 
  lapply(unlist) 
  lapply(summary) 
  lapply(round, 2L) 
  lapply(as.list) 
  bind_rows(.id = 'element') 
  as.data.frame()
})}
