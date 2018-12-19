#' Fix parameters alpha, beta, and gamma based on given chemical compositions.
#' @noRd
#' 
#' @param xmap `qm_xmap` class object returned by [`read_xmap()`]
#' @param cls `qm_cluster` class object returned by [`cluster_xmap()`]
#' @param csv 
#'   A file path to the csv file with columns `phase`, `element` and `wt`.
#'   
#' @importFrom matrixStats rowMaxs weightedMedian
#' @importFrom dplyr 
#'   filter group_by mutate right_join summarize
#' @importFrom tidyr gather
fix_params_by_wt <- function(xmap, cls, params) {
  xmap %>>%
    `[`(unique(params$element)) %>>%
    lapply(unlist, use.names = FALSE) %>>%
    c(list(
      phase = cls$cluster,
      w = rowMaxs(cls$membership)
    )) %>>%
    as.data.frame(stringsAsFactors = FALSE) %>>%
    filter(phase %in% (!!params$phase)) %>>%
    gather(element, mapint, -phase, -w) %>>%
    group_by(phase, element) %>>%
    summarize(mapint = weightedMedian(mapint, w)) %>>%
    ungroup %>>%
    right_join(params, by = c("phase", "element")) %>>%
    mutate(ab = wt / mapint, mapint = NULL, wt = NULL)
}

# csv <- (
#   'phase element wt
#   Qtz Si 100'
#   )
