# © 2019 JAMSTEC
retrieve_params <- function(x, xmap) {
  shiny::req(x)
  y <- attr(x, "params", exact = TRUE)
  y$beta <- y$beta *
    attr(xmap, "dwell") * 1e-3 *
    attr(xmap, "current") * 1e+6
  y
}
