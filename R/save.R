#' S3 generics to save returned values from functions in QntMap package.
#' @return `invisible(x)`
#' @param x object to save
#' @param nm name of object to save
#' @param saving `TRUE` or `FALSE` to save result file(s).
#' @param ... other arguments passed to class methods
#' @export
save4qm <- function(x, nm = '', saving = TRUE, ...) {
  if(!saving) return(invisible(x))
  if(!is.character(nm)) stop('nm must be a string')
  UseMethod('save4qm')
}

#' @rdname save4qm
#' @section `save4qm.default`: 
#'   A default method.
#'   This is a wrapper of `saveRDS` together with invisible return of `x`.
save4qm.default <- function(x, nm, saving, ...) {
  saveRDS(x, nm)
  invisible(x)
}

#' @rdname save4qm
#' @section `save4qm.data.frame`:
#'   A method for `data.frame` class. 
#'   This is a wrapper of `data.table::fwrite` 
#'   together with invisible return of `x`.
#' 
#' @importFrom data.table fwrite
save4qm.data.frame <- function(x, nm, saving, ...) {
  fwrite(x, nm)
  invisible(x)
}

#' @rdname save4qm
#' @section `save4qm.qm_cluster`: 
#' A method for `qm_cluster` class returned by 
#' [`cluster_xmap()`] or [`group_cluster()`].
#' Object is saved as RDS file and png file.
#' The latter shows distribution of phases among a mapped area.
#' @importFrom png writePNG
#' @importFrom ggplot2 ggsave labs
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
  
  #save modal map
  writePNG(image = .img, target = paste0(nm, "_map.png"))

  #save result of classification
  saveRDS(x, paste0(nm, "_result.RDS"))

  #save legend
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
#' A method for `qntmap` class returned by `qntmap()` or `quantify()`
#' 
#' @importFrom data.table fwrite
#' @importFrom purrr walk2
save4qm.qntmap <- function(x, nm, saving, ...) {
  saveRDS(x, file.path(nm, 'qntmap.RDS'))
  unlist(x, recursive = FALSE) %>>%
    walk2(
      file.path(nm, paste0(str_replace(names(.), '\\.', '_'), '.csv')),
      fwrite
    )
  invisible(x)
}
