priors_summary <- c("Area", "x", "y", "size_x", "size_y", "cluster")

summarize_whole <- function(data, summary, id) {
  data %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(
      Area = "Whole", size_x = max(data$x), size_y = max(data$y),
      membership = NULL
    ) %>>%
    prioritize(priors_summary) %>>%
    bind_rows(summary[[id]])
}

summarize_box <- function(data, box, summary, id, ...) {
  UseMethod("summarize_box")
}

summarize_box.default <- function(data, box, summary, id, ...) {
  mutate(summarize_box.qntmap(data, box, summary, id), density = NULL)
}

summarize_box.qntmap <- function(data, box, summary, id, density = NULL, ...) {
  X <- round(c(box$xmin, box$xmax))
  Y <- round(c(box$ymin, box$ymax))

  .rows <- X[[1L]] <= data$x & data$x <= X[[2L]] &
    Y[[1L]] <= data$y & data$y <= Y[[2L]]
  .n_rows <- sum(.rows)
  density <- if (is.null(density)) numeric(.n_rows) + 1 else density[.rows]
  density_sum <- sum(density)
  xy <- c("x", "y")
  wt <- setdiff(names(data), xy)
  data[.rows, ] %>>%
    purrr::modify_at(wt, `*`, density) %>>%
    dplyr::summarize_if(is.numeric, sum) %>>%
    purrr::modify_at(wt, `/`, density_sum) %>>%
    purrr::modify_at(xy, `/`, .n_rows) %>>%
    dplyr::mutate(
      Area = "Box", membership = NULL,
      size_x = X[[2L]] - X[[1L]] + 1L,
      size_y = Y[[2L]] - Y[[1L]] + 1L,
      density = if (all(!!density == 1)) NA_real_ else mean(!!density)
    ) %>>%
    dplyr::filter(is.finite(.data$x), is.finite(.data$y)) %>>%
    dplyr::bind_rows(summary[[id]]) %>>%
    dplyr::mutate(ID = dplyr::row_number()) %>>%
    prioritize(priors_summary)
}

summarize_click <- function(data, click, summary, id) {
  x <- round(click$x)
  y <- round(click$y)
  
  data[data$x == x & data$y == y, ] %>>%
    mutate(Area = "Click", membership = NULL, size_x = 1L, size_y = 1L) %>>%
    bind_rows(summary[[id]]) %>>%
    mutate(ID = row_number()) %>>%
    prioritize(priors_summary)
}

#' @importFrom utils head
#' @noRd
summarize_latest <- function(df) {
  if (is.null(df)) return(NULL)
  df %>>%
    head(3) %>>%
    modify_if(is.numeric, function(x) sprintf("%.2f", x)) %>>%
    mutate(ID = row_number()) %>>%
    gather("Var", "Val", -"ID", factor_key = TRUE) %>>%
    spread("ID", "Val") %>>%
    rename(ID = "Var")
}
