#' print qntmap class data.
#'
#' @param x qntmap class data
#' @param summarizing TRUE in default will visibly return summary of x and invisibly return x. FALSE will just return x.
#' @param ... other arguments are discarded
#'
#' @importFrom pipeR %>>%
#'
#'@export
#'
print.qntmap <- function(x, summarizing = TRUE, ...) {
  if(!summarizing) return(x)
  cat(paste(
    'Summary of', 
    paste(dim(x[[1]][[1]]), collapse = ' * '), 
    ' mass concentration map\n'
  ))
  print(summary.qntmap(x))
  cat(
    '',
    'This is a list object',
    'x$CaO$wt returns CaO mass concentration map, and', 
    'x$CaO$se returns CaO standard error map', 
    '', 
    'You can also access to the above data in csv files', 
    'saved in "qntmap" directory below your mapping data directory', 
    'e.g., example/.map/1/qntmap/CaO_wt.csv',
    sep = '\n'
  )
  invisible(x)
}
