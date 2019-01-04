#' Read X-ray map data
#'
#' @param wd directory path containing mapping data (e.g., ./.map/.1)
#' @param DT dead time in nano seconds (0 nsec in default)
#' @param renew 
#'   `TRUE` (default) read data from original files.
#'   `FALSE` tries to read `xmap.RDS` in `wd`.
#' @param saving 
#'   `TRUE` (default) or `FALSE` to save the result as `xmap.RDS` file in `wd`.
#' @param .map,.cnd 
#'   Regular expressions to match file names of 
#'   ASCII converted maps (`.map`) and condition files (`.cnd`)
#'
#' @importFrom purrr map_at
#' @importFrom stats setNames
#' @export
#'
read_xmap <- function(
  wd = '.map/1',
  DT = 0,
  renew = FALSE,
  saving = TRUE,
  .map = '(data[0-9]+\\.csv)|([1-9][0-9]*_map\\.txt)', 
  .cnd = '(data[0-9]+|[1-9][0-9]*)\\.cnd'
) {
  # when the argument "wd" is assigned, 
  # `setwd()` to the argument on start and to the current `wd` on exit.
  
  cd <- getwd()
  on.exit(setwd(cd))
  wd <- normalizePath(wd)
  setwd(wd)

  # Read old file with version check
  rds <- "xmap.RDS"
  if((!renew) && file.exists(rds)) {
    xmap <- readRDS(rds)
    ver_old <- attr(xmap, 'ver')
    deadtime <- attr(xmap, "deadtime")
    if (!is.null(ver_old)) 
      if (ver == ver_old && deadtime == DT)
        return(structure(xmap, dir_map = wd))
    rm(xmap, ver_old, deadtime)
  }
  
  files_xmap <- dir(pattern = .map, full.names = TRUE)
  files_cnd <- dir(pattern = .cnd, full.names = TRUE)
  stop_if_files_xmap_and_cnd_have_different_size(files_xmap, files_cnd)
  
  cnd <- lapply(files_cnd, read_xmap_cnd, patterns = patterns_xmap_cnd)
  elm <- unlist(lapply(cnd, `[[`, 'elm'), use.names = FALSE)
  dwell <- as.integer(cnd[[1]][['dwell']][1])

  construct_qm_xmap(
    files_xmap = files_xmap, elm = elm, dwell = dwell, deadtime = DT, dir_map = wd,
    current = as.numeric(cnd[[1]][['current']][1]),
    start = as.numeric(cnd[[1]][['start']][1:3]),
    pixel = as.integer(cnd[[1]][['pixel']][1:2]),
    step = as.numeric(cnd[[1]][['step']][1:2]),
    instrument = cnd[[1]][['instrument']][1]
  ) %>>%
    save4qm(rds, saving)
}

#' Stop if lengths of files_xmap and files_cnd are different
#' @noRd
stop_if_files_xmap_and_cnd_have_different_size <- function (
  files_xmap, files_cnd
) {
  if(length(files_xmap) != length(files_cnd)) {
    cat(
      'file names of mapping data:', files_xmap, '\n',
      'file names of mapping conditions:', files_cnd
    )
    stop(
      'Length of files of xmap and cnd are different.' , 
      'Check parameters .map and .cnd'
    )
  }
}

#' Construct qm_xmap class object
#' @importFrom purrr map_at
#' @importFrom stats setNames
#' @noRd
construct_qm_xmap <- function(files_xmap, elm, dwell, deadtime, dir_map, ...) {
  files_xmap %>>%
    lapply(fread) %>>%
    setNames(elm) %>>%
    prioritize(.component) %>>%
    map_at( # Dead time corrections except for electron signals (e.g., BSE)
      setdiff(names(.), .electron),
      function(x) dwell * x / (dwell - deadtime * 1e-9 * x)
    ) %>>%
    lapply(round) %>>%
    lapply(lapply, as.integer) %>>%
    lapply(as.data.frame) %>>%
    structure(
      class = c('qm_xmap', class(.)),
      deadtime = deadtime,
      dwell = dwell,
      ...,
      dir_map = dir_map,
      ver = ver # Version of qntmap defined internally
    )
}
