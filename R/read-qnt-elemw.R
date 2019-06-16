#' @noRd
read_qnt_elemw <- function(x, ...) UseMethod("read_qnt_elemw")

#' @noRd
read_qnt_elemw.default <- function(x, recursive = TRUE, ...) {
  cnd <- structure(read_cnd(x), path = x)
  if (!recursive) return(cnd)
  read_qnt_elemw(cnd, ...)
}

#' @noRd
read_qnt_elemw.map_cnd <- function(x, ...) {
  data.frame(
    bgp_pos = x[["XM_ELEM_WDS_BACK_PLUS"]][[1L]],
    bgm_pos = x[["XM_ELEM_WDS_BACK_MINUS"]][[1L]],
    pk_t = x[["XM_ELEM_WDS_QNT_PEAK_TIME"]][[1L]],
    bg_t = x[["XM_ELEM_WDS_QNT_BACK_TIME"]][[1L]]
  )
}

#' read all .cnd files under the specified directory
#' @inheritParams read_cnd
#'
#' @param pattern named character vectors
#' @param n number of rows
#' @param each n appears in each rows
#' @noRd
read_qnt_elemw.0_cnd <- function(
                                 x,
                                 pattern = c(
                                   bgp_pos = "(Back |BG)\\+\\[mm\\]",
                                   bgm_pos = "(Back |BG)-\\[mm\\]",
                                   pk_t = "(Peak|Pk)( Meas\\.)? (Time|t)",
                                   bg_t = "(Back|BG)( Meas\\.)? (Time|t)"
                                 ),
                                 n = c(14L, 15L, 17L, 18L),
                                 each = 21L
) {
  # read cnd files and transform to numeric
  val <- str_replace(x, "[:blank:].*", "")

  # match pattern and return values
  matched <- lapply(pattern, function(pattern) str_detect(x, pattern))

  if (
    any(unlist(matched, use.names = FALSE)) &&
      length(unique(lapply(matched, sum))) == 1L
  ) return(
      as.data.frame(lapply(matched, function(i) as.numeric(val[i])))
    )

  # if pattern did not match well, return values by guess
  names(n) <- names(pattern)
  guessed <- lapply(n, seq, length(x), each)
  warning(
    "Following variables are not detected by regular expressions, ",
    "but by guessing which line contains them.\n",
    paste0(names(guessed), ": ", lapply(guessed, paste, collapse = " "), "\n"),
    "Check a following file if values are in correct lines\n",
    normalizePath(attributes(x)[["path"]]),
    "\n"
  )
  if (length(n) != length(pattern)) stop("length of pattern and n must be same")

  print(as.data.frame(lapply(guessed, function(i) as.numeric(val[i]))))
}
