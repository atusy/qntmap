#' find XAG
#' @noRd
#' @param X X
#' @param AG AG
#' @importFrom dplyr transmute_at
#' @importFrom matrixStats colSums2
#' @importFrom stats setNames
#' @importFrom purrr map map_at
find_XAG <- function(X, AG, se = TRUE) {
  AG %>>%
    transmute_at(c('ag', 'ag_se'[se]), setNames, .$phase3) %>>%
    split(AG$elm) %>>%
    map(map, `*`, t(X)) %>>%
    map(setNames, c('val', 'se'[se])) %>>%
    map(map_at, 'se', square) %>>%
    map(map, colSums2, na.rm = TRUE) %>>%
    map(map_at, 'se', sqrt)
}
