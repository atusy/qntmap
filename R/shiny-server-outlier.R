outlier_gg_react <- function(
  epma, input, percentile = .95, interval = "prediction"
) {reactive({
  .phase <- setdiff(unique(epma()$phase), input$outlier_phase)
  epma() %>>% 
    mutate(fine_phase = phase %in% !!input$outlier_phase) %>>%
    find_outlier(
      phase = !!.phase, element = everything(),
      percentile = percentile, interval = interval
    ) %>>%
    filter(
      elint == !!input$outlier_elem, is.finite(.data$mapint * .data$pkint)
    ) %>>%
    mutate(facet = "All") %>>%
    bind_rows(
      filter(., !.data$outlier & !.data$fine_phase) %>>% 
        find_outlier(
          percentile = percentile, interval = interval
        ) %>>% 
        mutate(facet = "Filtered")
    ) %>>%
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
