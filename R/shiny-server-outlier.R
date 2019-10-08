#' @importFrom ggAtusy stat_err
#' @noRd
outlier_gg <- function(
  epma, input, percentile = .99, interval = "prediction", coords = NULL
) {
  .phase_all <- unique(epma$phase)
  .phase <- setdiff(.phase_all, input$outlier_phase)
  bind_rows(
    epma %>>%
      filter(.data$elint == !!input$outlier_elem) %>>%
      find_poisson_prediction_intervals(
        percentile = percentile, phase = .phase_all
      ) %>>%
      mutate(facet = "All"),
    epma %>>%
      find_outlier(
        phase = !!.phase, percentile = percentile, interval = interval
      ) %>>% 
      filter(
        .data$elint == !!input$outlier_elem, !.data$outlier, .data$phase %in% !!.phase
      ) %>>%
      find_poisson_prediction_intervals(
        percentile = percentile, phase = .phase_all
      ) %>>%
      mutate(facet = "Filtered")
    ) %>>%
    filter(is.finite(.data$mapint * .data$pkint)) %>>%
    ggplot(aes(.data$mapint, .data$pkint)) +
    geom_ribbon(
      aes(ymin = .data$pkint.L_est, ymax = .data$pkint.H_est),
      color = "transparent", fill = "gray80"
    ) +
    stat_err(
      aes(
        xmin = .data$mapint.L, xmax = .data$mapint.H,
        ymin = .data$pkint.L, ymax = .data$pkint.H,
        color = .data$phase
      ),
      size = .8
    ) +
    geom_smooth(formula = y ~ 0 + x, se = FALSE, color = "red", method = "lm") +
    geom_quantile(formula = y ~ 0 + x, quantiles = .5, color = "blue") +
    facet_wrap(vars(.data$facet), scales = 'fixed') +
    coords +
    scale_color_discrete(name = "Phase") +
    scale_alpha_identity() +
    theme_bw(base_size = 16) +
    labs(x = "Mapped peak intensity [cps/uA]", y = "Spot peak intensity [cps/uA]")
}
