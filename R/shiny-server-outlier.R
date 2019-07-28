outlier_gg_react <- function(
  epma, input, percentile = .95, interval = "prediction"
) {reactive({
  .phase_all <- unique(epma()$phase)
  .phase <- setdiff(.phase_all, input$outlier_phase)
  bind_rows(
    epma() %>>%
      filter(elint == !!input$outlier_elem) %>>%
      find_poisson_prediction_intervals(
        percentile = percentile, phase = .phase_all
      ) %>>%
      mutate(facet = "All"),
    epma() %>>%
      find_outlier(
        phase = !!.phase, percentile = percentile, interval = interval
      ) %>>% 
      filter(
        .data$elint == !!input$outlier_elem, !.data$outlier, .data$phase %in% !!.phase
      ) %>>% 
      mutate(facet = "Filtered")
    ) %>>%
    filter(is.finite(.data$mapint * .data$pkint)) %>>%
    ggplot(aes(mapint, pkint)) +
    geom_ribbon(
      aes(ymin = .data$pkint.L_est, ymax = .data$pkint.H_est),
      color = "transparent", fill = "gray80"
    ) +
    ggAtusy::stat_err(aes(
      xmin = .data$mapint.L, xmax = .data$mapint.H,
      ymin = .data$pkint.L, ymax = .data$pkint.H,
      color = .data$phase
    )) +
    geom_smooth(formula = y ~ 0 + x, se = FALSE, color = "red", method = "lm") +
    geom_quantile(formula = y ~ 0 + x, quantiles = .5, color = "blue") +
    facet_wrap(
      vars(.data$facet), 
      scales = if (input$outlier_scales == "Shared") "fixed" else "free"
    ) +
    scale_color_discrete(name = "Phase") +
    scale_alpha_identity() +
    theme_bw(base_size = 16) +
    labs(x = "Mapped peak intensity [cps/uA]", y = "Spot peak intensity [cps/uA]")
})}
