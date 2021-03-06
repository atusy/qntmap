#' @include find-outlier.R
NULL

#' Initialize centroids for [`cluster_xmap()`]
#'
#' Initial centroids are determined by comparing data points from the quantified
#' spots and mapped pixels from the same coordinates.
#' First, mapping intensities counted from multi-phase pixels are removed from
#' the dataset (See "Multi-phase pixels" section).
#' Second, if the removal reduces sample size of some phases to less than 10,
#' then the mapping intensities are restored from signal intensities from spot
#' analysis.
#' Third, for each phase, [`median`] values of signal intensities are utilized
#' as initial centroids.
#' Forth, if there are missing values especially for intensities of electrons
#' (secondary, backscatter, ...), imputation is performed for them
#' (See "Impute missings" section).
#'
#' @param xmap An object generated by [`read_xmap()`]
#' @param qnt An object generated by [`read_qnt()`]
#' @inheritParams find_outlier
#' @inheritDotParams find_outlier interval method percentile
#' @param saveas File name to save result. `FALSE` if not saving.
#'
#' @section Multi-phase pixels:
#'   X-ray mapping inevitably involves multi-phase pixels, especially for
#'   fine-grained phases. When the multi-phase pixels are member of training
#'   data set, initial centroids can be biased. This is why guessing
#'   centroids is based on [`median()`] instead of [`mean()`]. However, if pixels
#'   to guess centroids for cetrtain phase are comprising a large number of or
#'   even a full of multi-phase pixels, [`median()`] is still vulnerable. For
#'   example, refer to a reprinted figure 5 from
#'   [Yasumoto et al. (2018)](https://doi.org/10.2138/am-2018-6323CCBY), and
#'   captions in original article. Phases such as Amp, Di, and Pl are outlying
#'   the regression curves due to multi-phase pixels. For these phases,
#'   median values of peak X-ray intensities from X-ray mapping are unreliable
#'   values for initial centroids. Thus, data points from multi-phase pixels
#'   should be regarded as outliers, and imputetions should be performed on them.
#'   In qntmap, mapping intensities are simply substituted by spot intensities.
#'   
#'   Outliers are identified by examining if confidence intervals data points
#'   overlap with predictive intervals or conditional whiskers of Tukey's choice.
#'   
#' @section Impute missings:
#'   When spot analysis on some phases are performed outside the mapping area,
#'   X-ray intensities under mapping conditions can be calculated from 
#'   quantified peak intensities. However, intensities of secondary and backscatter
#'   electrons cannot be calculated because electrons are generally not
#'   analyzed in spot analysis. Thus, imputation is performed. First, calculate
#'   median values of mapping X-ray intensities. Second, find a pixel in the map
#'   which have closest value to the median values. Third, retrive electron
#'   intensities from that pixel and utilize as a centroid.
#'   
#'   Note that missing values may remain if certain elements are analyzed in
#'   spot analysis but not in mapping analysis.
#'
#' @inherit qntmap-package references
#'
#' @importFrom matrixStats colSums2
#' @importFrom stats median
#'
#' @export
find_centers <- function(
                         xmap,
                         qnt,
                         phase = everything(),
                         element = everything(),
                         saveas = "centers0.csv",
                         epma = tidy_epma(qnt, xmap),
                         fine_phase = NULL,
                         ...
) {

  quantified <- names(xmap) %in% c("x", "y", qnt$elm$elint)
  
  missings <- if (!all(quantified)) {
    epma[!is.na(epma$nr), c("id", "phase", "nr"), drop = FALSE] %>>%
      distinct_all() %>>%
      bind_cols(xmap[.$nr, !quantified, drop = FALSE]) %>>%
      select(-"nr") %>>%
      gather("elint", "map", -"id", -"phase")
  }

  .phase <- enquo(phase)
  .element <- enquo(element)
  
  epma %>>%
    select(
      "id", "phase", "elint", "beam", "pk_t", "beam_map", "dwell", "map",
      starts_with("mapint"), starts_with("pkint")
    ) %>>%
    # Find outliers
    find_outlier(
      element = !!.element, phase = !!.phase, fine_phase = fine_phase, ...
    ) %>>%
    mutate(kept = .data$outlier %in% TRUE) %>>%
    # Add elint not quantified
    bind_rows(missings) %>>%
    group_by(.data$id) %>>%
    mutate(kept = `if`(isTRUE(any(.data$kept)), TRUE, FALSE)) %>>%
    # Impute map from pkint if mapping multi-phase pixels
    ## Which
    group_by(.data$phase, .data$elint) %>>%
    mutate(
      imputed = (sum(.data$kept) < 10) & !.data$kept,
      kept = .data$kept | .data$imputed
    ) %>>%
    filter(.data$kept | all(!.data$kept)) %>>%
    ungroup() %>>%
    ## Do
    transmute(
      map = ifelse(
        .data$imputed,
        .data$pkint * .data$dwell * .data$beam_map * 1e6,
        .data$map
      ),
      .data$phase, .data$elint, .data$id
    ) %>>%
    # Find centers
    # filter(is.finite(.data$map)) %>>%
    group_by(.data$phase, .data$elint) %>>%
    summarize(map = median(map, na.rm = TRUE)) %>>%
    spread("elint", "map") %>>%
    # Impute missing values caused by phase not being quantified within map
    impute_centers_from_xmap(xmap) %>>%
    # Finalize
    arrange(.data$phase) %>>%
    prioritize(c("phase", .component)) %>>%
    as.data.frame %>>%
    save4qm(nm = saveas, saving = is.character(saveas)) %>>%
    identity
}

impute_centers_from_xmap <- function(centers, xmap) {
  missings <- Reduce(`|`, lapply(centers, is.na))
  if (all(!missings)) return(centers)
  centers[missings, ] %>>%
    group_by(.data$phase) %>>%
    group_modify(~ {
      .x_finite <- select_if(.x, is.finite)
      nm <- intersect(names(xmap), names(.x_finite))
      xmap[
        which.min(reduce_add(map2(.x_finite[nm], xmap[nm], ~square(.x - .y)))), ,
        drop = FALSE
      ] %>>%
        mutate(!!!as.list(.x_finite))
    }) %>>%
    ungroup %>>%
    select(-"x", -"y") %>>%
    bind_rows(centers[!missings, ])
}
