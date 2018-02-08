#' return integer as character flagged with 0
#'
#' @param x integer
#' @export
flag0 <- function(x) {
  formatC(x, width = floor(log10(max(x, na.rm = TRUE))) + 1, flag = '0')
}
