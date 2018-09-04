#' read mapping beam conditions
#' @param x input
#' @param ... other arguments passed to generics
#' @noRd
read_map_beam <- function(x, ...) {
  UseMethod('read_map_beam')
}

#' a method for read_map_beam
#' @inheritDotParams read_map_beam
#' @noRd
read_map_beam.default <- function(x, ...) {
  read_map_beam(read_cnd(x), ...)
}

#' a method for read_map_beam
#' @inheritDotParams read_map_beam
#' @noRd
read_map_beam.map_cnd <- function(x, ...) {pipeline({
  x[c(
    'XM_DATA_PROBE_CURRENT',
    'XM_AP_SA_DWELL_TIME'
  )]
  lapply(`[[`, 1)
  unlist(use.names = FALSE)
  setNames(c('beam', 'dwell'))
})}

#' read mapping beam conditions
#' @param x x
#' @param pattern patterns to be matched
#' @param n nth lines
#' @inheritDotParams read_map_beam ...
#' @noRd
read_map_beam.0_cnd <- function(
  x,
  pattern = c(
    dwell = 'Dwell Time \\[msec\\]',
    beam_map = 'Probe Current (Avg, Before After )?\\[A\\]'
  ),
  n = c(39, 17),
  ...
) {
  pipeline({
    read_cnd(x, pattern, n)
      str_replace('[:blank:].*', '')
      as.numeric
      setNames(names(pattern))
  })
}
