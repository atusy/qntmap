retrieve_params <- function(x) {
  shiny::req(x)
  y <- attr(x, "params", exact = TRUE)
  y$beta <- y$beta / 100
  y
}
