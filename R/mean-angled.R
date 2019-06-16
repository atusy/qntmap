#' Calculate means for horizontal and vertical directions
#'
#' Calculate means for horizontal and vertical directions for mapping data.
#' 
#' @inheritParams mean.qntmap
#' @param .by Just for internal use. "x" for `hmean`, and "y" for `vmean`.
#'   
#' @seealso mean.qntmap
#' 
#' @export
hmean <- function(x, cluster = NULL, density = NULL, ..., .by = "x") {
  step <- attributes(x)$step[[1L]]
  if (!is.numeric(step)) step <- NA_real_
  
  mean(x, index = x[[.by]], cluster = NULL, density = NULL, ...) %>>%
    gather("px", "val", -"Element", convert = TRUE) %>>%
    spread("Element", "val") %>>%
    mutate(um = (.data$px - 1L) * !!step) %>>%
    select("px", "um", everything())
}

#' @rdname hmean
#' @export
vmean <- hmean
formals(vmean)$.by <- "y"
