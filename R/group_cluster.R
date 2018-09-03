#' group clusters who share same prefix
#'
#' When data points are assigned to clusters A_1 and A_2, 
#' their clusters are renamed to be A by matching regular expressions.
#'
#' @param x `qm_cluster` class object returned by `qltmap_cls_pois()`.
#' @param saving `TRUE` or `FALSE` to save result (default: `TRUE`).
#' @param suffix A regular expression of suffix (default: `'_.*'`)
#'
#' @importFrom stringr str_replace
#'
#' @export
group_cluster <- function(x, saving = TRUE, suffix = '_.*') {

  # group clusters
  colnames(x$membership) <- .cn <- 
    str_replace(colnames(x$membership), suffix, '')
  x$cluster <- str_replace(x$cluster, suffix, '')
  .cls <- as.factor(x$cluster)
  x$ytehat <- setNames(as.integer(.cls), as.character(.cls))
  
  # modify membership
  x$membership <- sapply(
    levels(.cls),
    function(.cls) rowSums(x$membership[, .cn == .cls, drop = FALSE])
  )

  class(x) <- c(class(x), 'integrated')
  save4qm(x, 'grouped', saving, components = components)
}
