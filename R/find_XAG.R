#' find XAG
#' @param X X
#' @param AG AG
#' @importFrom dplyr transmute_at
#' @importFrom stats setNames
#' @importFrom purrr map map_at
#' @noRd
find_XAG <- function(X, AG) {pipeline({
  AG 
    transmute_at(c('ag', 'ag_se'), setNames, .$phase3) 
    split(AG$elm)
    map(map, `*`, t(X)) 
    map(setNames, c('val', 'se')) 
    map(map_at, 'se', square) 
    map(map, colSums, na.rm = TRUE)
    map(map_at, 'se', sqrt)
})}
