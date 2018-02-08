#' print qntmap class data.
#'
#' @param x qntmap class data
#' @param summarizing TRUE in default will visibly return summary of x and invisibly return x. FALSE will just return x.
#'
#' @importFrom pipeR `%>>%`
#'
#'@export
#'
print.qntmap <- function(x, summarizing = TRUE) {
  if(!summarizing) return(x)
  cat(paste(
    'Summary of', 
    paste(dim(x[[1]][[1]]), collapse = ' * '), 
    ' mass concentration map\n'
  ))
  print(summary.qntmap(x))
  cat('\n')
  cat('This is a list object\n')
  cat('x$CaO$wt returns CaO mass concentration map, and\n')
  cat('x$CaO$se returns CaO standard error map\n')
  cat('\n')
  cat('You can also access to the above data in csv files\n')
  cat('saved in "qntmap" directory below your mapping data directory\n')
  cat('e.g., example/.map/1/qntmap/CaO_wt.csv')
  invisible(x)
}
