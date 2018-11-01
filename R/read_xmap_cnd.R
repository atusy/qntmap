# read .cnd files of X-ray mapping data

#' list of patterns in cnd files
#' @noRd
patterns_xmap_cnd <- pipeR::pipeline({
  list(
    jxa8800 = list(
      elm = 'XM_ELEMENT', 
      dwell = 'XM_DWELL_TIME', 
      current = 'CM_CURRENT', 
      start = 'XM_MAP_START', 
      pixel = 'XM_POINTS', 
      step = 'XM_STEP_SIZE' ,
      instrument = 'CM_INSTRUMENT'
    ),
    jxa8230 = list( # on windows
      elm = c('XM_ELEM_NAME%0', 'XM_ELEM_IMS_SIGNAL_TYPE%0'), 
      # 'XM_ELEM_IMS_SIGNAL_TYPE' for SEI, COMPO, TOPO
      dwell = 'XM_AP_SA_DWELL_TIME%0',
      current = 'XM_DATA_PROBE_CURRENT',
      start = 'XM_AP_SA_STAGE_POS%0_1',
      pixel = 'XM_AP_SA_PIXELS%0',
      step = 'XM_AP_SA_PIXEL_SIZE%0',
      instrument = 'XM_ANALYSIS_INSTRUMENT'
    )
  )
  (~ if(length(unique(lapply(., names))) != 1) stop('check list'))
  pmap(c, use.names = FALSE)
  lapply(paste, collapse = '|')
  unlist
})

#' Read cnd files of X-ray mapping data
#' @param x path to the cnd file
#' @param patterns list of patterns
#' @noRd
read_xmap_cnd <- function(x, patterns = patterns_xmap_cnd) {
  if(is.null(patterns)) readLines(x)
  pipeline({
    x
    readLines
    str_subset(pipeline({
      patterns
      unlist(use.names = FALSE)
      paste(collapse = '|')
      (function(x) paste0('^\\$(', x, ')[:blank:]'))()
    }))
    str_replace('^\\$', '')
    str_replace_all('[:blank:]+', ' ')
    strsplit(' ')
    `[`(map_int(., length) > 1)
    setNames(pipeline({ # set names based on names(patterns_map_cnd)
      map(., `[`, 1)
      map(function(pattern) str_detect(patterns, pattern))
      map_int(which)
      (function(i) names(patterns)[i])()
    }))
    lapply(`[`, -1)
  })
}
