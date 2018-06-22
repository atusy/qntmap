#' find XAG
#' @param X X
#' @param AB AB
#' @importFrom dplyr select
#' @importFrom tidyr nest
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom purrr map_at
qntmap_XAG <- function(X, AG) {pipeline({
  AG 
    select(phase3, elm, ag, ag_se) 
    nest(-elm) 
    (.x ~ setNames(.x$data, .x$elm)) 
    map(function(x) map(select(x, -phase3), setNames, x$phase3)) 
    map(map, `*`, t(X)) 
    map(setNames, c('val', 'se')) 
    map(map_at, 'se', `^`, 2) 
    map(map, colSums)
    map(map_at, 'se', sqrt)
})}
