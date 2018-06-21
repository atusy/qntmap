#' return integer as character flagged with 0
#'
#' @param ... list of integer vectors of same length
flag0 <- function(...) {
  do.call(
    'paste0',
    lapply(
      unname(list(...)),
      function(x) {
        formatC(
          x,
          width = floor(log10(max(x, na.rm = TRUE))) + 1, 
          flag = '0'
        )
      }
    )
  )
}
