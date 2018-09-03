#' read mapping positions
#' @param x input
#' @param ... other arguments passed to methods
#' @noRd
read_map_pos <- function(x, ...) UseMethod('read_map_pos')

#' a default method for read_map_pos
#' @inheritParams read_map_pos
#' @noRd
read_map_pos.default <- function(x, ...) {
  read_map_pos(read_cnd(x), ...)
}

#' a method for read_map_pos
#' @inheritParams read_map_pos
read_map_pos.map_cnd <- function(x, ...) {pipeline({
  rbind(
    x[['XM_AP_SA_STAGE_POS']][2, 1:2],
    x[['XM_AP_SA_PIXELS']][1, 1:2],
    x[['XM_AP_SA_PIXEL_SIZE']][1, 1:2]
  )
  t
  as.data.frame
  setNames(c('start', 'px', 'step'))
})}
#' read mapping stage information from 0.cnd
#' @inheritParams read_cnd.0_cnd
#' @noRd
read_map_pos.0_cnd <- function(
  x,
  pattern = c(
    'Measurement Start Position X',
    'Measurement Start Position Y',
    'X(-axis)? Step Number',
    'Y(-axis)? Step Number',
    'X(-axis)? Step Size',
    'Y(-axis)? Step Size'
  ),
  n = c(27, 28, 30:33)
) {
  pipeline({
    read_cnd(x, pattern, n)
    str_replace('[:blank:].*', '')
    as.numeric
    matrix(ncol = 3, nrow = 2, dimnames = list(NULL, c('start', 'px', 'step')))
    as.data.frame
  })
}
