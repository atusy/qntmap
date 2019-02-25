#' Save objects created by qntmap package.
#' @return `invisible(x)`
#' @param x      An object to save.
#' @param nm     A name of object to be saved as.
#' @param saving `TRUE` (default) or `FALSE` to save.
#' @param ...    Other arguments passed to class methods
#' @importFrom data.table fwrite
#' @importFrom ggplot2    ggsave labs
#' @importFrom pipeR      %>>%
#' @importFrom png        writePNG
#' @importFrom purrr      walk2
#' @export
save4qm <- function(x, nm = '', saving = TRUE, ...) {
  if(!saving) return(invisible(x))
  if(!is.character(nm)) stop('nm must be a string')
  UseMethod('save4qm')
}

#' @rdname save4qm
#' @section `save4qm.default`: 
#'   A default method saves an object as a binary RDS file.
save4qm.default <- function(x, nm, saving, ...) {
  saveRDS(x, nm)
  invisible(x)
}

#' @rdname save4qm
#' @section `save4qm.data.frame`:
#'   A data frame object is saved as a csv. 
save4qm.data.frame <- function(x, nm, saving, ...) {
  fwrite(x, nm)
  invisible(x)
}

#' @rdname save4qm
#' @section `save4qm.qm_cluster`: 
#'  A value returned by [`cluster_xmap()`] or [`group_cluster()`] is
#'  saved as binary RDS file, dot-by-dot png file, and as svg file with a legend.
#'  The png and svg files show distribution of phases among a mapped area.
save4qm.qm_cluster <- function(x, nm, saving, ...) {
  #setting for output
  dir_out <- paste0(x$dir_map, '/clustering')
  dir.create(dir_out, showWarnings = FALSE)
  k <- ncol(x$membership)
  nm <- paste0(
      dir_out, '/', x$date, '_', nm, '_k', k, '_', 
      paste(x$elements, collapse ='')
    )

  .cls <- as.factor(x$cluster)
  .img <- as_img(lookup$discrete(.cls), x$dims[1], x$dims[2])
  
  # Save as binary
  saveRDS(x, paste0(nm, "_result.RDS"))
  
  # Save as dot-by-dot png
  writePNG(image = .img, target = paste0(nm, "_map.png"))

  # Save plot with legend as svg
  A4 <- c(210, 297)[order(x$dims)] # 1st = H and 2nd = W in millimeters
  ggsave(
    paste0(nm, "_legend.svg"), 
    gg_img(.img, zlim = levels(.cls)) + labs(y = "Rows", x = "Columns"),
    height = A4[[1]], width = A4[[2]], units = "mm"
  )

  invisible(x)
}

#' @rdname save4qm
#' @section `save4qm.qntmap`: 
#'  A value returned by `qntmap()` or `quantify()` is saved as
#'  a binary RDS file, and csv files.
save4qm.qntmap <- function(x, nm, saving, ...) {
  saveRDS(x, file.path(nm, 'qntmap.RDS'))
  unlist(x, recursive = FALSE) %>>%
    walk2(
      file.path(nm, paste0(str_replace(names(.), '\\.', '_'), '.csv')),
      fwrite
    )
  invisible(x)
}
