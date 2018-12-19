#' Read .cnd files
#' 
#' @name read_cnd
#' @param x A path to the file (e.g., '0.cnd', 'map.cnd', ...)
#' @param pattern pattern to substract
#' @param ... Other arguments passed to methods
#' @export
read_cnd <- function(x, pattern = NULL, ...) UseMethod("read_cnd")

#' @rdname read_cnd
#' @importFrom stringr str_detect str_subset
#' @section .default: 
#' A default method which returns a result of [`readLines()`] 
#' with additional class according to the content of the file.
#' @export
read_cnd.default <- function(x, pattern = NULL, ...) {
  cnd <- readLines(x)
  if(!is.null(pattern)) cnd <- str_subset(cnd, pattern)
  class(cnd) <- `if`(all(str_detect(cnd, '^\\$')), 'map_cnd', '0_cnd')
  read_cnd(cnd, pattern = pattern, ...)
}

#' @rdname read_cnd
#' @section read_cnd.map_cnd: A method for `map_cnd` class object.
#' @importFrom tidyr separate
#' @importFrom utils type.convert
#' @importFrom dplyr arrange
#' @importFrom stringr str_extract
#' @export
read_cnd.map_cnd <- function(x, pattern = NULL, ...) {
  x %>>%
    str_replace_all('[:blank:]+', ' ') %>>%
    strsplit(' ') %>>%
    lapply(`[`, seq(max(lengths(.)))) %>>%
    .x ~ Reduce(rbind, .x) %>>%
    as.data.frame %>>%
    setNames(paste0('V', seq(0, by = 1, length.out = length(.)))) %>>%
    mutate(V0 = str_replace(V0, '\\$', '')) %>>%
    separate(
      'V0', into = c('id', 'no'), 
      sep = '%', fill = 'right', convert = TRUE
    ) %>>%
    split(.$id) %>>%
    lapply(select, -id) %>>%
    lapply(lapply, type.convert, as.is = TRUE) %>>%
    lapply(lapply, unname) %>>%
    lapply(as.data.frame) %>>%
    lapply(arrange, no) %>>%
    lapply(select, -no) %>>%
    `class<-`('map_cnd')
}

#' @rdname read_cnd
#' @section read_cnd.0_cnd: 
#'   A method for `0_cnd` class object.
#'   Used to extract rows which contains phrase matching pattern.
#' @param n 
#'   integer vector of same length as pattern. 
#'   Used to extract nth row of .cnd file if no phrases match the `pattern`.
#' @export
read_cnd.0_cnd <- function(x, pattern = NULL, n = NULL, ...) {
  if(is.null(pattern)) return(x)
  
  detection <- lapply(pattern, function(i) which(str_detect(x, i)))
  
  # number of detections
  detection_n <- lengths(detection)
  
  # error if any pattern matched more than 1 phrase
  if(any(too_many <- detection_n > 1)) {
    stop(
      'Some of the regular expression patterns matched more than 1 lines in "',
      path,
      '"\n',
      paste(
        paste0(
          '"', pattern[too_many], '"\n matched lines ', 
          lapply(detection[too_many], paste, collapse = ', ')
        ),
        collapse = '\n'
      )
    )
  }
  
  # warn if any pattern did not match any phrase
  if(any(mismatch <- detection_n == 0)) {
    warning(
      'Some of the regular expression patterns matched 0 phrases in "', 
      path, '".\n',
      'Such patterns as follows are assumed to be in lines specified',
      'by a parameter n.\n',
      paste(
        paste0(
          '"', pattern[mismatch], '"', 
          ' is considered to be in line ', n[mismatch]
        ),
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

