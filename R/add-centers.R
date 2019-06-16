#' @include pick.R
NULL

# © 2018 JAMSTEC
#' Add centroids manually
#'
#' Add centroids manually by picking X-ray counts of selected pixels in X-ray maps.
#'
#' @seealso [pick()], [find_centers()]
#'
#' @inheritParams find_centers
#' @inheritParams cluster
#' @inheritParams pick
#'
#' @export
add_centers <- function(centers, xmap) {
  save4qm(
    structure(
      bind_rows(
        as.data.frame(centers),
        pick(xmap, x = x, y = y, phase = phase, i = i)[names(centers)]
      ),
      class = class(centers)
    ),
    nm = saveas, saving = is.character(saveas)
  )
}
formals(add_centers) <- c(
  formals(add_centers),
  formals(pick)[-1],
  alist(saveas = "center_add.csv")
)
