outlier_gg_layers <- list(
  aes(
    .data$mapint, .data$pkint, color = .data$phase,
    xmin = .data$mapint.L, xmax = .data$mapint.H,
    ymin = .data$pkint.L, ymax = .data$pkint.H,
    weight = .data$weight, alpha = .data$alpha
  ),
  geom_smooth(formula = y ~ 0 + x, color = "gray10", se = FALSE, method = "lm"),
  ggAtusy::stat_err(),
  labs(
    x = "Map Peak [cps/uA]", y = "Spot Peak [cps/uA]"
  ),
  scale_color_discrete(name = "Phase"),
  scale_alpha_identity(),
  theme_minimal(base_size = 16)
)

outlier_gg_react <- function(epma, input) {reactive({
  epma() %>>%
    filter(elint == !!input$outlier_elem) %>>%
    mutate(
      good = !(.data$phase %in% !!input$outlier_phase),
      weight = as.integer(.data$good),
      alpha = .data$weight * .7 + .3
    ) %>>%
    filter((!!input$outlier_action != "Filter") | .data$good) %>>%
    ggplot() + 
    ggtitle(input$outlier_elem) +
    outlier_gg_layers
})}
