#' group clusters who share same prefix
#'
#' When data points are assigned to clusters A_1 and A_2, 
#' their clusters are renamed to be A by matching regular expressions.
#'
#' @param result result of qltmap_cls_pois
#' @param saving TRUE or FALSE to save result
#'
#' @importFrom stringr str_replace
#'
#' @export
cluster_group <- function(result, saving = TRUE) {

  # integrate clusters on demand
  cls <- as.factor(str_replace(colnames(result$membership), '_.*$', ''))

  # modify result
  result$ytehat <- result$ytehat %>>%
    names %>>%
    str_replace('_.*$', '') %>>%
    as.factor %>>%
    (setNames(as.integer(.), as.character(.)))
  
  result$membership <- sapply(
    levels(cls),
    function(i) rowSums(result$membership[, cls == i, drop = FALSE])
  )

  if(saving) save4qm(result, 'grouped')

  class(result) <- c(class(result), 'integrated')
  result
}
