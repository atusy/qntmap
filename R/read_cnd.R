#' read .cnd file of JEOL EPMA (0.cnd, 1.cnd, ...)
#'
#' @param path a path to the .cnd file
#' @param pattern character vector. Used to extract rows which contains phrase matching pattern.
#' @param n integer vector of same length as pattern. Used to extract nth row of .cnd file in case pattern did not match any phrase.
#'
read_cnd <- function(path = '0.cnd', pattern = '.', n = NULL) {
  cnd <- readLines(path)
  if(is.null(pattern)) return(cnd)
  
  detection <- lapply(
    pattern,
    function(pattern) {
      which(str_detect(cnd, pattern))
    }
  )
  
  # number of detections
  detection_n <- unlist(lapply(detection, length), use.names = FALSE)
  
  # error if any pattern matched more than 1 phrase
  too_many <- detection_n > 1
  if(any(too_many)) {
    stop(
      paste0(
        'Some of the regular expression patterns matched more than 1 lines in "', path, '"\n',
        paste(
          paste0('"', pattern[too_many], '"', " matched lines ", lapply(detection[too_many], paste, collapse = ', ')),
          collapse = '\n'
        )
      )
    )
  }
  
  # warn if any pattern did not match any phrase
  mismatch <- detection_n == 0
  if(any(mismatch)) {
    warning(
      paste0(
        'Some of the regular expression patterns matched 0 phrases in "', path, '".\n',
        'Such patterns as follows are assumed to be in lines specified by a parameter n.\n',
        paste(
          paste0('"', pattern[mismatch], '"', ' is considered to be in line ', n[mismatch]),
          collapse = '\n'
        )
      )
    )
    
    if(length(pattern) != length(n)) stop('pattern and n must have same length')

    return(cnd[unlist(ifelse(mismatch, n, detection), use.names = FALSE)])
  }

  return(cnd[unlist(detection, use.names = FALSE)])
}


#' read mapping stage information from 0.cnd
#' @inheritParams read_cnd
read_map_pos <- function(
  path,
  pattern = c(
    'Measurement Start Position X',
    'Measurement Start Position Y',
    'X(-axis)? Step Number',
    'Y(-axis)? Step Number',
    'X(-axis)? Step Size',
    'Y(-axis)? Step Size'
  ),
  n = c(27, 28, 30:33)
) {
  pipeline({
    read_cnd(path, pattern, n)
    str_replace('[:blank:].*', '')
    as.numeric
    matrix(ncol = 3, nrow = 2, dimnames = list(NULL, c('start', 'px', 'step')))
    as.data.table
  })
}

#' read mapping beam information from 0.cnd
#' @inheritParams read_cnd
read_map_beam <- function(
  path,
  pattern = c(
    dwell = 'Dwell Time \\[msec\\]',
    beam_map = 'Probe Current (Avg, Before After )?\\[A\\]'
  ),
  n = c(39, 17)
) {
  pipeline({
    read_cnd(path, pattern, n)
    str_replace('[:blank:].*', '')
    as.numeric
    setNames(names(pattern))
  })
}


