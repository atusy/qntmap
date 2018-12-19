#' `print` method for `qntmap` class object
#' 
#' @param x 
#'   A `qntmap` class object returned by [`quantify()`] or [`qntmap()`].
#' @param summarizing 
#'   `TRUE` or `FALSE` to summarize x (default: `TRUE`. 
#' @param ... 
#'   Discarded.
#'
#'@export
#'
print.qntmap <- function(x, summarizing = TRUE, ...) {
  if(!summarizing) return(x)
  cat(
    'Summary of', 
    paste(dim(x[[1]][[1]]), collapse = ' * '), 
    ' mass concentration map\n',
    sep = ' '
  )
  print(summary(x))
  cat(
    '',
    'This is a list object',
    'x$CaO$wt returns CaO mass concentration map, and', 
    'x$CaO$se returns CaO standard error map', 
    '', 
    'The data are also accessible as csv files', 
    'in "qntmap" directory below your mapping data directory', 
    'e.g., example/.map/1/qntmap/CaO_wt.csv',
    sep = '\n'
  )
  invisible(x)
}
