#' print qntmap class data.
#'
#' @param x qntmap class data
#' @param summarizing TRUE in default will visibly return summary of x and invisibly return x. FALSE will just return x.
#' @param ... other arguments are discarded
#'
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
  print(summary.qntmap(x))
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
