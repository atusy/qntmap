#' find XAG
#' @noRd
#' @param X X
#' @param AG AG
#' @importFrom dplyr transmute_at
#' @importFrom matrixStats colSums2
#' @importFrom stats setNames
#' @importFrom purrr map_at
find_XAG <- function(X, AG, se = TRUE) {
  X_trans <- t(X)
  nm <- c("val", "se"[se])
  AG %>>%
    transmute_at(c("ag", "ag_se"[se]), setNames, .$phase3) %>>%
    split(AG$elm) %>>%
    lapply(function(x) {
      x %>>%
        lapply(`*`, X_trans) %>>%
        setNames(nm) %>>%
        map_at("se", square) %>>%
        lapply(colSums2, na.rm = TRUE) %>>%
        map_at("se", sqrt)
    })
}
