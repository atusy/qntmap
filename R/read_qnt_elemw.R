#' @noRd
read_qnt_elemw <- function(x, ...) UseMethod('read_qnt_elemw')

#' @noRd
read_qnt_elemw.default <- function(x, ...) {
  read_qnt_elemw(structure(read_cnd(x), path = x), ...)
}

#' @noRd
read_qnt_elemw.map_cnd <- function(
  x,
  ...
) {
  data.frame(
    bgp_pos = x[['XM_ELEM_WDS_BACK_PLUS']][[1]],
    bgm_pos = x[['XM_ELEM_WDS_BACK_MINUS']][[1]],
    pk_t = x[['XM_ELEM_WDS_QNT_PEAK_TIME']][[1]],
    bg_t = x[['XM_ELEM_WDS_QNT_BACK_TIME']][[1]]
  )
}

#' read all .cnd files under the specified directory
#' @inheritParams read_cnd
#' @param pattern named character vectors
#' @param n number of rows
#' @param each n appears in each rows
#' @noRd
read_qnt_elemw.0_cnd <- function(
  x,
  pattern = c(
    bgp_pos = '(Back |BG)\\+\\[mm\\]',
    bgm_pos = '(Back |BG)-\\[mm\\]',
    pk_t = '(Peak|Pk)( Meas\\.)? (Time|t)',
    bg_t = '(Back|BG)( Meas\\.)? (Time|t)'
  ),
  n = c(14, 15, 17, 18),
  each = 21
) {
  # read cnd files and transform to numeric
  val <- str_replace(x, '[:blank:].*', '')
  
  # match pattern and return values
  matched <- lapply(pattern, grepl, x)
  
  if(length(unique(lapply(matched, sum))) == 1) return(
    as.data.frame(lapply(matched, function(i) as.numeric(val[i])))
  )
  
  # if pattern did not match well, return values by guess
  names(n) <- names(pattern)
  guessed <- lapply(n, seq, length(x), each)
  warning(
    'Following variables are not detected by regular expressions, but by guessing which line contains them.\n',
    paste0(names(guessed), ': ', lapply(guessed, paste, collapse = ' '), '\n'),
    'Check a following file if values are in correct lines\n', 
    normalizePath(attributes(x)[['path']]),
    '\n'
  )
  if(length(n) != length(pattern)) stop('length of pattern and n must be same')
  
  print(as.data.frame(lapply(guessed, function(i) val[i])))
}


      