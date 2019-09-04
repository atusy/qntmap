params_output_plots <- function(output, ...) {
  output$params_alpha <- shiny::renderPlot(autoplot.tidy_epma(..., type = "alpha"))
  output$params_beta <- shiny::renderPlot(autoplot.tidy_epma(..., type = "beta"))
  output$params_gamma <- shiny::renderPlot(autoplot.tidy_epma(..., type = "gamma"))
}

