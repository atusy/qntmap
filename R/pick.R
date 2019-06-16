# © 2018 JAMSTEC
#' Pick values from mapping data
#'
#' Pick values from mapping data based on coordinates of x and y.
#'
#' @param .df
#'   Mapping data. Currentl version only supports `qm_xmap` object.
#' @param x,y
#'   Integer vectors specifying x- and y-coordinates of pixels of mapping data.
#'   In other words, x- and y- corresponds to n-th column and row of a matrix.
#' @param phase
#'   Name of phases comprising the pixels to be picked.
#'   If not manually supplied `phase` is supplied by `c('P1', 'P2', ...)`.
#' @param i
#'   A data frame with clumns `x`, `y`, and `phase`.
#'   `i` have priority to `x`, `y`, and `phase`.`
#' @param ...
#'   Ignored
#'
#' @return `data.frame`
#'
#' @seealso [add_centers()]
#'
#' @export
pick <- function(
                 .df,
                 x = NULL,
                 y = NULL,
                 phase = `if`(is.null(x), NULL, paste0("P", seq_along(x))),
                 i = data.frame(x, y, phase),
                 ...
) {
  if (length(unique(lengths(list(x, y, phase)))) != 1L)
    stop("Lengths of x, y, and phase must be same")

  .df[(.df$x == i$x) & (.df$y == i$y), ] %>>%
    mutate(phase = !!i$phase, x = NULL, y = NULL) %>>%
    select("phase", everything())
}
