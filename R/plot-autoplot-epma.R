autoplot.tidy_epma <- function(
  object, type = c('alpha', 'beta', 'gamma'), ...
) {
  list(
    "alpha" = autoplot_tidy_epma_alpha,
    "beta"  = autoplot_tidy_epma_beta,
    "gamma" = autoplot_tidy_epma_gamma
  )[match.arg(type)](object, params = params, ...)
}

autoplot_tidy_epma_alpha <- function(
  object, params = NULL, origin = FALSE, ...
) {
  
  fit <- if (is.null(params)) {
    geom_smooth(formula = y ~ 0 + x, se = FALSE, fullrange = TRUE, method = 'lm')
  } else {
    ggplot2::geom_abline(
      ggplot2::aes(slope = .data$alpha, intercept = 0, color = .data$phase),
      data = params
    )
  }
  
  ggplot2::ggplot(
    object,
    ggplot2::aes(.data$net, .data$wt, color = .data$phase)
  ) +
    fit +
    ggplot2::geom_point() +
    add_origin(origin) +
    ggplot2::facet_wrap(~ elm, scales = 'free') +
    ggplot2::labs(
      x = "Spot net intensity [cpx/uA]", y = "Concentration [wt%]", colour = "Phase"
    )
}

autoplot_tidy_epma_beta <- function() {}

autoplot_tidy_epma_gamma <- function() {}

add_origin <- function(origin = TRUE) {
  if (origin) geom_point(
    data = function(x) {
      x %>>%
        dplyr::group_by(.data$elm, .data$phase) %>>%
        dplyr::slice(1L) %>>%
        mutate(
          wt = 0,
          net = 0,
          pkint = 0,
          mapint = 0
        )
    },
    colour = "transparent"
  )
}