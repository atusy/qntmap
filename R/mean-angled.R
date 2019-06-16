#' Calculate means for horizontal and vertical directions
#'
#' Calculate means for horizontal and vertical directions for mapping data.
#' 
#' @inheritParams mean.qntmap
#' @param .by 
#'   Internal use. Do not change.
#'   "y" for `hmean` to calculate mean from pixels with same `y`-coordinates,
#'   and "x" for `vmean` to calculate mean from pixels with same `x`-coordinates.
#'   
#' @seealso mean.qntmap
#' 
#' @export
hmean <- function(x, cluster = NULL, density = NULL, ..., .by = "y") {
  step <- attributes(x)$step[[1L]]
  if (!is.numeric(step)) step <- NA_real_
  
  mean(x, index = x[[.by]], cluster = cluster, density = density, ...) %>>%
    gather("px", "val", -"Element", convert = TRUE) %>>%
    spread("Element", "val") %>>%
    mutate(um = (.data$px - 1L) * !!step) %>>%
    select("px", "um", everything()) %>>%
    as.data.frame %>>%
    structure(class = c("qm_profile", class(.)))
}

#' @rdname hmean
#' @export
vmean <- hmean
formals(vmean)$.by <- "x"
