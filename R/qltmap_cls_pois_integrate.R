#' integrate clusters named with same prefix (e.g., A_1 and A_2 are integrated to be A)
#'
#' @param result result of qltmap_cls_pois
#' @param wd Directory containing mapping data. If NULL, current directory is wd.
#' @param saving TRUE or FALSE to save result
#'
#' @importFrom stringr str_replace
#'
#' @export
qltmap_cls_pois_integrate <- function(result, wd = NULL, saving = TRUE) {
  cd <- getwd()
  on.exit(setwd(cd))

  #####set working directory
  if(!is.null(wd)) setwd(wd)

  #####integrate clusters on demand
  cls <- as.factor(str_replace(colnames(result$membership), '_.*$', ''))

  #####modify result
  result$ytehat <- result$ytehat %>>%
    names %>>%
    str_replace('_.*$', '') %>>%
    as.factor %>>%
    (setNames(as.numeric(.), as.character(.)))
  result$membership <- sapply(
    levels(cls),
    function(i) rowSums(result$membership[, cls == i, drop = FALSE])
  )

  if(saving) qltmap_cls_save(result, 'pois_integrated')

  result
}
