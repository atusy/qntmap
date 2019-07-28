summarize_whole <- function(data, summary, id) {
  data() %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(Area = "Whole") %>>%
    select("Area", "x", "y", everything()) %>>%
    bind_rows(summary[[id]])
}

summarize_box <- function(data, box, summary, id) {
  data()[
    box$xmin <= data()$x & data()$x <= box$xmax &
      box$ymin <= data()$y & data()$y <= box$ymax,
    ] %>>%
    summarize_if(is.numeric, mean) %>>%
    mutate(Area = "Box", membership = NULL) %>>%
    filter(is.finite(.data$x), is.finite(.data$y)) %>>%
    bind_rows(summary[[id]]) %>>%
    mutate(ID = row_number()) %>>%
    select("ID", "Area", "x", "y", everything())
}


summarize_click <- function(data, click, summary, id) {
  x <- round(click$x)
  y <- round(click$y)
  
  data()[data()$x == x & data()$y == y, ] %>>%
    mutate(Area = "Click", membership = NULL) %>>%
    bind_rows(summary[[id]]) %>>%
    mutate(ID = row_number()) %>>%
    select("ID", "Area", "x", "y", everything())
}

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
