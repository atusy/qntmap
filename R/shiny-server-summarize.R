priors_summary <- c("Area", "x", "y", "size_x", "size_y", "cluster")

summarize_whole <- function(data, summary, id) {
  data() %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(
      Area = "Whole", size_x = max(data()$x), size_y = max(data()$y),
      membership = NULL
    ) %>>%
    prioritize(priors_summary) %>>%
    bind_rows(summary[[id]])
}

summarize_box <- function(data, box, summary, id) {
  X <- round(c(box$xmin, box$xmax))
  Y <- round(c(box$ymin, box$ymax))
  data()[
    X[[1L]] <= data()$x & data()$x <= X[[2L]] &
      Y[[1L]] <= data()$y & data()$y <= Y[[2L]],
    ] %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(
      Area = "Box", membership = NULL,
      size_x = X[[2L]] - X[[1L]] + 1L,
      size_y = Y[[2L]] - Y[[1L]] + 1L
    ) %>>%
    filter(is.finite(.data$x), is.finite(.data$y)) %>>%
    bind_rows(summary[[id]]) %>>%
    mutate(ID = row_number()) %>>%
    prioritize(priors_summary)
}


summarize_click <- function(data, click, summary, id) {
  x <- round(click$x)
  y <- round(click$y)
  
  data()[data()$x == x & data()$y == y, ] %>>%
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
