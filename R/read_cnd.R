#' read .cnd files
#' e.g., '0.cnd', 'map.cnd', ...
#' @param x path to the file
#' @param ... other arguments passed to methods
read_cnd <- function(x, ...) {
  UseMethod("read_cnd")
}

#' default method for read_cnd
#' @inheritParams read_cnd
read_cnd.default <- function(x, ...) {
  cnd <- readLines(x)
  class(cnd) <- `if`(all(grepl('^\\$', cnd)), 'map_cnd', '0_cnd')
  read_cnd(cnd, ...)
}

#' method for read_cnd
#' @param x returned value from readLines. each elements starts with "$"
#' @param ... other arguments passed to methods
#' @importFrom tidyr separate
#' @importFrom utils type.convert
#' @importFrom dplyr arrange
#' @importFrom stringr str_extract
read_cnd.map_cnd <- function(x, ...) {pipeline({
  x
    strsplit(' ')
    lapply(`[`, seq(max(map_int(., length))))
    .x ~ Reduce(rbind, .x)
    as.data.frame
    setNames(paste0('V', seq(0, by = 1, length.out = length(.))))
    mutate(V0 = str_replace(V0, '\\$', ''))
    separate('V0', into = c('id', 'no'), sep = '%', fill = 'right', convert = TRUE)
    split(.$id)
    map(select, -id)
    map(map, type.convert, as.is = TRUE)
    map(map, unname)
    map(as.data.frame)
    map(arrange, no)
    map(select, -no)
    `class<-`('map_cnd')
})}

#' method for read_cnd
#'
#' @param x input
#' @param pattern character vector. Used to extract rows which contains phrase matching pattern.
#' @param n integer vector of same length as pattern. Used to extract nth row of .cnd file in case pattern did not match any phrase.
#'
read_cnd.0_cnd <- function(x, pattern = NULL, n = NULL) {
  if(is.null(pattern)) return(x)
  
  detection <- lapply(pattern, function(i) which(str_detect(x, i)))
  
  # number of detections
  detection_n <- unlist(lapply(detection, length), use.names = FALSE)
  
  # error if any pattern matched more than 1 phrase
  if(any(too_many <- detection_n > 1)) {
    stop(
      'Some of the regular expression patterns matched more than 1 lines in "',
      path,
      '"\n',
      paste(
        paste0('"', pattern[too_many], '"\n matched lines ', lapply(detection[too_many], paste, collapse = ', ')),
        collapse = '\n'
      )
    )
  }
  
  # warn if any pattern did not match any phrase
  if(any(mismatch <- detection_n == 0)) {
    warning(
      'Some of the regular expression patterns matched 0 phrases in "', path, '".\n',
      'Such patterns as follows are assumed to be in lines specified by a parameter n.\n',
      paste(
        paste0('"', pattern[mismatch], '"', ' is considered to be in line ', n[mismatch]),
        collapse = '\n'
      )
    )
    
    if(length(pattern) != length(n)) stop('pattern and n must have same length')
    
    return(x[unlist(ifelse(mismatch, n, detection), use.names = FALSE)])
  }
  
  structure(
    x[unlist(detection, use.names = FALSE)],
    class = '0_cnd'
  )
}

