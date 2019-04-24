#' Defunct functions
#' @name defunct
#' @param ... Ignored
NULL

#' @rdname defunct
#' @export
qntmap_quantify <- function(...) {
  .Defunct(new = "quantify")
}

#' @rdname defunct
#' @export
qltmap_cls_centers <- function(...) {
  .Defunct(new = "find_centers")
}

#' @rdname defunct
#' @export
qltmap_load <- function(...) {
  .Defunct(new = "read_xmap")
}

#' @rdname defunct
#' @export
qntmap_cls_pois <- function(...) {
  .Defunct(new = "cluster_xmap")
}

#' @rdname defunct
#' @export
qnt_load <- function(...) {
  .Defunct(new = "read_qnt")
}
