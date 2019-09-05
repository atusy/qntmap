autoplot.tidy_epma <- function(
  object, type = c('alpha', 'beta', 'gamma'), phase = NULL, element = NULL, ...
) {
  list(
    "alpha" = autoplot_params_alpha,
    "beta"  = autoplot_params_beta,
    "gamma" = autoplot_params_gamma
  )[[match.arg(type)]](object, phase = phase, element = element, ...)
}

autoplot_params_alpha <- function(
  object, params = NULL, origin = FALSE, phase = NULL, element = NULL,  ...
) {
  
  fit <- if (is.null(params)) {
    ggplot2::geom_smooth(formula = y ~ 0 + x, se = FALSE, fullrange = TRUE, method = 'lm')
  } else {
    ggplot2::geom_abline(
      ggplot2::aes(slope = .data$alpha, intercept = 0, color = .data$phase),
      data = filter_tidy_epma(params, phase = phase, element = element)
    )
  }
  
  ggplot2::ggplot(
    filter_tidy_epma(object, phase = phase, element = element),
    ggplot2::aes(.data$net, .data$wt, color = .data$phase)
  ) +
    fit +
    ggplot2::geom_point() +
    add_origin(origin) +
    ggplot2::facet_wrap(~ elm, scales = 'free') +
    ggplot2::labs(
      x = "Spot net intensity [cpx/uA]", y = "Concentration [wt%]", colour = "Phase"
    ) +
    ggplot2::scale_x_continuous(labels = function(x) x * 1e-2)
}

autoplot_params_beta <- function(
  object, params = NULL, origin = FALSE, phase = NULL, element = NULL,  ...
) {
  
  fit <- if (is.null(params)) {
    ggplot2::geom_smooth(formula = y ~ 0 + x, se = FALSE, fullrange = TRUE, method = 'lm')
  } else {
    ggplot2::geom_abline(
      ggplot2::aes(slope = .data$beta, intercept = 0, color = .data$phase),
      data = filter_tidy_epma(params, phase = phase, element = element)
    )
  }
  
  ggplot2::ggplot(
    filter_tidy_epma(object, phase = phase, element = element),
    ggplot2::aes(.data$mapint, .data$pkint, color = .data$phase)
  ) +
    fit +
    ggplot2::geom_point() +
    add_origin(origin) +
    ggplot2::facet_wrap(~ elm, scales = 'free') +
    ggplot2::labs(
      x = "Map peak intensity [cpx/uA]", y = "Spot peak intensity [cpx/uA]", colour = "Phase"
    ) +
    ggplot2::scale_x_continuous(labels = function(x) x * 1e-2) +
    ggplot2::scale_y_continuous(labels = function(x) x * 1e-2)
}

autoplot_params_gamma <- function(
  object, params = NULL, origin = FALSE, phase = NULL, element = NULL,  ...
) {
  
  fit <- if (is.null(params)) {
    ggplot2::geom_smooth(formula = y ~ 1, se = FALSE, fullrange = TRUE, method = 'lm')
  } else {
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = .data$gamma, color = .data$phase),
      data = filter_tidy_epma(params, phase = phase, element = element)
    )
  }
  
  ggplot2::ggplot(
    filter_tidy_epma(object, phase = phase, element = element),
    ggplot2::aes(.data$wt, .data$bgint, color = .data$phase)
  ) +
    fit +
    ggplot2::geom_point() +
    add_origin(origin) +
    ggplot2::facet_wrap(~ elm, scales = 'free') +
    ggplot2::labs(
      x = "Concentration [wt%]", y = "Spot background intensity [cpx/uA]", colour = "Phase"
    ) +
    ggplot2::scale_x_continuous(labels = function(x) x * 1e-2)
}

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
          mapint = 0,
          bgint = 0
        )
    },
    colour = "transparent"
  )
}

filter_tidy_epma <- function(x, phase = NULL, element = NULL) {
  if (is.null(x$elm)) x$elm <- x$oxide
  if (is.null(x$elint)) x$elint <- x$element
  filter(
    x,
    `if`(is.null(!!element), TRUE, .data$elm %in% !!element | .data$elint %in% !!element),
    `if`(is.null(!!phase), TRUE, .data$phase %in% !!phase)
  )
}

geom_point_qnt <- function(data) {
  ggplot2::geom_point(
    ggplot2::aes(.data$y_px, .data$x_px), inherit.aes = FALSE, color = "green",
    dplyr::filter(data, .data$elint == .data$elint[[1L]])[c("x_px", "y_px")]
  )
}