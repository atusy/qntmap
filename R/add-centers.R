#' @include pick.R
NULL

# © 2018 JAMSTEC
#' Add centroids manually
#'
#' Add centroids manually by picking X-ray counts of selected pixels in X-ray maps.
#'
#' @seealso [pick()], [find_centers()]
#'
#' @examples
#' centers <- data.frame(phase = "a", A = 1, B = 1)
#' xmap <- list(A = data.frame(1:2), B = data.frame(1:2))
#' class(xmap) <- c("qm_xmap", "list")
#' add_centers(centers, xmap, x = 1, y = 2, saveas = FALSE)
#' @inheritParams find_centers
#' @inheritParams cluster
#' @inheritParams pick
#'
#' @importFrom dplyr bind_rows
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
