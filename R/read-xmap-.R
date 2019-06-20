#' Read X-ray map data
#'
#' @param wd directory path containing mapping data (e.g., ./.map/.1)
#' @param DT dead time in nano seconds (0 nsec in default)
#' @param conditions
#'   A path to a csv file which records file paths and analytical conditions of
#'   mapping data.
#'   Specifying `conditions` discards other parameters except `saving`.
#' @param .map,.cnd
#'   Regular expressions to match file names of
#'   ASCII converted maps (`.map`) and condition files (`.cnd`)
#'
#' @export
#'
read_xmap <- function(
                      wd = ".map/1",
                      DT = 0,
                      conditions = NULL,
                      .map = "(data[0-9]+\\.csv)|([1-9][0-9]*_map\\.txt)",
                      .cnd = "(data[0-9]+|[1-9][0-9]*)\\.cnd"
) {

  if (is.character(conditions)) # © 2018 JAMSTEC
    return(read_xmap_by_conditions(conditions)) # © 2018 JAMSTEC

  cd <- getwd()
  on.exit(setwd(cd))
  wd <- normalizePath(wd)
  setwd(wd)

  files_xmap <- dir(pattern = .map, full.names = TRUE)
  files_cnd <- dir(pattern = .cnd, full.names = TRUE)
  stop_if_inequal_xmap_and_cnd(files_xmap, files_cnd)

  cnd <- lapply(files_cnd, read_xmap_cnd, patterns = patterns_xmap_cnd)

  construct_qm_xmap(
    files_xmap = files_xmap,
    deadtime   = DT,
    dir_map    = wd,
    elm        = unlist(lapply(cnd, `[[`, "elm"), use.names = FALSE),
    dwell      = as.numeric(cnd[[1L]][["dwell"]][1L]),
    current    = as.numeric(cnd[[1L]][["current"]][1L]),
    start      = as.numeric(cnd[[1L]][["start"]][1:3]),
    pixel      = as.integer(cnd[[1L]][["pixel"]][1:2]),
    step       = as.numeric(cnd[[1L]][["step"]][1:2]),
    instrument =            cnd[[1L]][["instrument"]][1L]
  )
}

#' Stop if lengths of files_xmap and files_cnd are different
#' @param files_xmap File names of mapping data
#' @param files_cnd File names of mapping conditions
#' @noRd
stop_if_inequal_xmap_and_cnd <- function(
                                         files_xmap, files_cnd
) {
  if (length(files_xmap) != length(files_cnd)) {
    message(
      "File names of mapping data:", files_xmap, "\n",
      "File names of mapping conditions:", files_cnd
    )
    stop (
      "Length of files of xmap and cnd are different.",
      "Check parameters .map and .cnd"
    )
  }
}

#' Construct qm_xmap class object
#'
#' @param files_xmap File paths of X-ray mapping data
#' @param elm Names of elements for `files_xmap`
#' @param dwell A numeric value of a dwell time
#' @param deadtime A numeric value of a dead time
#' @param dir_map A directory which contains mapping data
#' @param ...
#'   Attributes for a returning value:
#'   `current` for probe current,
#'   `start` for starting x-y coordinates,
#'   `pixel` for number of pixels along x- and y-axes,
#'   `step` for step size as an integer,
#'   `instrument` for name of the instrument.
#'
#' @importFrom stats setNames
#' @noRd
construct_qm_xmap <- function(files_xmap, elm, dwell, deadtime, dir_map, ...) {
  files_xmap %>>%
    lapply(fread) %>>%
    setNames(elm) %>>%
    correct_deadtime(deadtime = deadtime, dwell = dwell) %>>%
    c(expand.grid(y = seq(NROW(.[[1L]])), x = seq(NCOL(.[[1L]])))) %>>%
    lapply(unlist, use.names = FALSE, recursive = FALSE) %>>%
    prioritize(c("x", "y", .component)) %>>%
    as.data.frame %>>%
    structure(
      class = c("qm_xmap", class(.)),
      deadtime = deadtime,
      dwell = dwell,
      ...,
      dir_map = dir_map,
      ver = ver # Version of qntmap defined internally
    )
}

#' Dead time corrections except for electron signals (e.g., BSE)
#' 
#' @noRd
#' 
#' @param x input data
#' @param deadtime nano-sec
#' @param dwell milli-sec
correct_deadtime <- function(x, deadtime = 0, dwell = 0) {
  old <- attributes(x)$deadtime
  
  if (is.null(old)) {
    old <- 0
  } else {
    dwell <- attributes(x)$dwell
  }
  
  if (deadtime == 0 && old == 0) return(structure(x, deadtime = 0))

  structure(
    modify_at(
      x,
      setdiff(names(x), c("x", "y", .electron)),
      function(x) (x / (1 - (deadtime - old) / dwell * 1e-6 * x))
    ),
    deadtime = deadtime
  )
}

# © 2018 JAMSTEC
#' Read X-ray map data based on a `conditions` csv file
#' @inheritParams read_xmap
#' @noRd
#'
read_xmap_by_conditions <- function(conditions) {
  .cnd <- fread(conditions)
  construct_qm_xmap(
    files_xmap = .cnd[["File path"]],
    elm        = .cnd[["Element"]],
    dwell      = .cnd[["Dwell [msec]"]][1L],
    deadtime   = .cnd[["dead time [ns]"]][1L],
    dir_map    = normalizePath(dirname(.cnd[["File path"]][1L])),
    current    = .cnd[["Probe current [A]"]][1L],
    start      = unlist(.cnd[1L, c("Start X [mm]", "Start Y [mm]")], use.names = FALSE),
    pixel      = unlist(.cnd[1L, c("Steps X", "Steps Y")], use.names = FALSE),
    step       = rep(.cnd[["Step size [um]"]][1L], 2L),
    instrument = .cnd[["Instrument"]][1L]
  )
}
