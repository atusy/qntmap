autoplot.qm_qnt <- function(object, x = 1, y = 2, ..., .filter = TRUE) {
  nm <- setdiff(names(object), c("x", "y"))
  x <- tidyselect::vars_select(nm, !!enquo(x))
  y <- tidyselect::vars_select(nm, !!enquo(y))
  dplyr::bind_cols(
    object$cnd[c("id", "phase", "use")],
    object$cmp$wt[c(x, y)]
  ) %>>%
    dplyr::filter(.data$use) %>>%
    autoplot_qnt(object, x, y, ...)
}

autoplot_qnt <- function(object, x, y, ...) {
  ggplot2::ggplot(object, ggplot2::aes(
    x = !!sym(x), y = !!sym(y),
    id = .data$id, color = .data$phase
  )) +
    ggplot2::labs(colour = "Phase") +
    ggplot2::geom_point(...)
}