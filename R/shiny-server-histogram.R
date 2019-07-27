hist_react <- function(id, map_reactive, input) {reactive({
  req(map_reactive())
  ids <- paste0(id, "_", c("elem", "min", "max", "color"))
  # hist(unlist(map_reactive()[[input[[ids[[1L]]]]]], use.names = FALSE))
  gghist(
    unlist(map_reactive()[[input[[ids[[1L]]]]]], use.names = FALSE),
    .min = input[[ids[[2L]]]], .max = input[[ids[[3L]]]],
    colors = input[[ids[[4L]]]],
    base_size = 16
  )
})}