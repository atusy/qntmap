zoom <- function(.axis, .input, .range) {
  if (is.null(.input)) return(.range)
  squish(
    c(.input[[paste0(.axis, "min")]], .input[[paste0(.axis, "max")]]), 
    .range
  )
}

move <- function(.axis, .input, .from, .to) {
  .range <- .from + c(1, -1) * (.to[2L] - .to[1L]) / 2
  .to - mean(.to) + squish(.input[[.axis]], .range)
}

observe_action <- function(id, input, ranges, range_x, range_y, summary, data) {
  ids <- paste0(id, "_", c("click", "brush", "action"))
  observeEvent(input[[ids[[3L]]]], {
    if (is.null(summary[[id]]) && !is.null(data())) {
      summary[[id]] <- summarize_whole(data, summary, id)
    }   
  })
  observeEvent(input[[ids[[1L]]]], {
    if (input[[ids[[3L]]]] == "Zoom") {
      ranges$x <- zoom("x", input[[ids[[2L]]]], range_x())
      ranges$y <- zoom("y", input[[ids[[2L]]]], range_y())
    }
    if (input[[ids[[3L]]]] == "Move" && !is.null(ranges$x)) {
      ranges$x <- move("x", input[[ids[[1L]]]], range_x(), ranges$x)
      ranges$y <- move("y", input[[ids[[1L]]]], range_y(), ranges$y)
    }
    if (input[[ids[[3L]]]] == "Summarize" && !is.null(data())) {
      if (!is.null(input[[ids[[1L]]]]) && is.null(input[[ids[[2L]]]]))
        summary[[id]] <- summarize_click(data, input[[ids[[1L]]]], summary, id)
      if (!is.null(input[[ids[[2L]]]]))
        summary[[id]] <- summarize_box(data, input[[ids[[2L]]]], summary, id)
    }
  })
}
