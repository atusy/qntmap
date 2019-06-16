#' @name summary
#' @inherit base::summary
NULL

#' @rdname summary
#' @importFrom matrixStats colSums2
#' @export
#' @inheritParams summary
summary.qm_cluster <- function(object, ...) {
  object %>>%
    select(-"x", -"y", -"cluster", -"membership") %>>%
    summarize_all(mean) %>>%
    as.data.frame
}

#' @rdname summary
#' @inheritParams base::round
#' @export
summary.qntmap <- function(object, digits = 2L, ...) {
  object %>>%
    select(-"x", -"y") %>>%
    lapply(function(x) as.list(round(summary(x), digits))) %>>%
    bind_rows(.id = "Element") %>>%
    as.data.frame
}
