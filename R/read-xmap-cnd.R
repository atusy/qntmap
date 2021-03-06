# read .cnd files of X-ray mapping data

#' A list of patterns to look for contents in cnd files
#' @noRd
patterns_xmap_cnd <-
  list(
    jxa8800 = list(
      elm = "XM_ELEMENT",
      dwell = "XM_DWELL_TIME",
      current = "CM_CURRENT",
      start = "XM_MAP_START",
      pixel = "XM_POINTS",
      step = "XM_STEP_SIZE",
      instrument = "CM_INSTRUMENT"
    ),
    jxa8230 = list( # on windows
      elm = c("XM_ELEM_NAME%0", "XM_ELEM_IMS_SIGNAL_TYPE%0"),
      # 'XM_ELEM_IMS_SIGNAL_TYPE' for SEI, COMPO, TOPO
      dwell = "XM_AP_SA_DWELL_TIME%0",
      current = "XM_DATA_PROBE_CURRENT",
      start = "XM_AP_SA_STAGE_POS%0_1",
      pixel = "XM_AP_SA_PIXELS%0",
      step = "XM_AP_SA_PIXEL_SIZE%0",
      instrument = "XM_ANALYSIS_INSTRUMENT"
    )
  ) %>>%
  (~ if (length(unique(lapply(., names))) != 1L) stop("check list")) %>>%
  pmap(c, use.names = FALSE) %>>%
  lapply(paste, collapse = "|") %>>%
  unlist

#' Read cnd files of X-ray mapping data
#' 
#' @param x path to the cnd file
#' @param patterns list of patterns
#' 
#' @importFrom stats setNames
#'
#' @noRd
read_xmap_cnd <- function(x, patterns = patterns_xmap_cnd) {
  y <- readLines(x)
  if (length(patterns) == 0L) return(y)
  y <- strsplit(
    str_replace_all(
      str_subset(y, collapse_patterns(patterns)),
      pattern = c("^\\$" = "", "[:blank:]+" = " ")
    ),
    " "
  )
  y <- y[(lengths(y) > 1L)]
  names(y) <-
    names(patterns)[vapply(y, function(.p) which(str_detect(patterns, .p[1])), 1L)]
  lapply(y, `[`, -1L)
}

collapse_patterns <- function(x) {
  paste0(
    "^\\$(",
    paste(unlist(x, use.names = FALSE), collapse = "|"),
    ")[:blank:]"
  )
}
