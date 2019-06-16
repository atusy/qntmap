#' Save objects created by qntmap package.
#' @return `invisible(x)`
#' @param x      An object to save.
#' @param nm     A name of object to be saved as.
#' @param saving `TRUE` (default) or `FALSE` to save.
#' @param ...    Other arguments passed to class methods
#' @importFrom data.table fwrite
#' @importFrom png        writePNG
#' @export
save4qm <- function(x, nm = "", saving = TRUE, ...) {
  if (!saving) return(invisible(x))
  if (!is.character(nm)) stop("nm must be a string")
  UseMethod("save4qm")
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
save4qm.qm_cluster <- function(
  x, nm, saving, dir_out = ".", suffix = "_.*", elements = "", ...
) {
  # setting for output
  dir_out <- file.path(dir_out, "clustering")
  dir.create(dir_out, showWarnings = FALSE)

  date <- format(Sys.time(), "%y%m%d-%H%M")
  
  save4qm_qm_cluster(x, dir_out, date = date, elements = elements)
  
  if (!any(grepl(suffix, x$cluster))) return(invisible(x))
  
  save4qm_qm_cluster(
    group_subclusters(x, suffix = suffix), 
    dir_out = dir_out, rds = FALSE, date = date, elements = elements
  )

  invisible(x)
}

save4qm_qm_cluster <- function(
  x, dir_out = ".", rds = TRUE, date = "", elements = ""
) {
  k <- ncol(x) - 4L # length(c("x", "y", "membership", "cluster"))
  nm <- paste0(
    dir_out, "/", 
    paste(date, paste0("k", k), paste(elements, collapse = ""), sep = "_")
  )

  .dims <- c(max(x$y), max(x$x))
  .cls <- as.factor(x$cluster)
  .img <- as_img(lookup$discrete(.cls), .dims[[1L]], .dims[[2L]])
  
  # Save as binary
  if (rds) saveRDS(x, paste0(nm, "_result.RDS"))
  
  # Save as dot-by-dot png
  writePNG(image = .img, target = paste0(nm, "_map.png"))
  
  # Save plot with legend as svg
  A4 <- c(210, 297)[order(.dims)] # 1st = H and 2nd = W in millimeters
  ggsave(
    paste0(nm, "_legend.svg"),
    gg_img(.img, zlim = levels(.cls)) + labs(y = "Rows", x = "Columns"),
    height = A4[[1L]], width = A4[[2L]], units = "mm"
  )
}

#' @rdname save4qm
#' @section `save4qm.qntmap`:
#'  A value returned by `qntmap()` or `quantify()` is saved as
#'  a binary RDS file, and csv files.
save4qm.qntmap <- function(x, nm, saving, ...) {
  saveRDS(x, file.path(nm, "qntmap.RDS"))
  
  .Dim <- c(max(x$y), max(x$x))
  
  x %>>%
    select(-"x", -"y") %>>%
    lapply(function(x) as.data.frame(structure(x, .Dim = .Dim))) %>>% 
    walk2(
      file.path(nm, paste0(str_replace(names(.), "\\.", "_"), ".csv")),
      fwrite, col.names = FALSE
    )
  
  invisible(x)
}
