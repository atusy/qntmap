#' read X-ray map data
#'
#' @param wd directory path containing mapping data (e.g., ./.map/.1)
#' @param DT dead time in nano seconds (0 nsec in default)
#' @param renew if TRUE and the file specified by RDS exists, that file will be loaded
#' @param saving whether or not to save the data as RDS file
#' @param .map,.cnd regular expressions to match ASCII converted mapping results (`.map`) and condition files (`.cnd`)
#'
#' @importFrom pipeR pipeline
#' @importFrom purrr map_at
#' @importFrom stringr str_replace
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
  #when the argument "wd" is assigned, setwd to the argument on start and to the current wd on exit.
  
  cd <- getwd()
  on.exit(setwd(cd))
  wd <- normalizePath(wd)
  setwd(wd)

  # Read old file with version check
  rds <- "xmap.RDS"
  if(!renew && file.exists(rds)) {
    xmap <- readRDS(rds)
    ver_old <- attr(xmap, 'ver')
    deadtime <- attr(xmap, "deadtime")
    if (!is.null(ver_old)) 
      if (ver == ver_old && deadtime == DT)
        return(structure(xmap, dir_map = wd))
    rm(xmap, ver_old, deadtime)
  }
  
  files_xmap <- dir(pattern = .map)
  files_cnd <- dir(pattern = .cnd)
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
  
  cnd <- lapply(files_cnd, read_xmap_cnd)

  elm <- unlist(lapply(cnd, `[[`, 'elm'), use.names = FALSE)
  
  dwell <- as.integer(cnd[[1]][['dwell']][1])

  #####load, save, and return map files
  # load qltmap from RDS file when qltmap_load() has already been done
  # load qltmap from text images when the RDS file does not exist,
  # there is something wrong with RDS file, or renew = TRUE
  pipeline({
    files_xmap
    lapply(fread)
      setNames(elm) 
      prioritize(.component)
      map_at( # Dead time corrections except for electron signals (e.g., BSE)
        setdiff(names(.), .electron),
        function(x) dwell * x / (dwell - DT * 1e-9 * x)
      ) 
      lapply(round) 
      lapply(lapply, as.integer) 
      lapply(as.data.frame) 
      structure(
        class = c('qm_xmap', class(.)),
        deadtime = DT,
        dir_map = wd,
        dwell = dwell,
        current = as.numeric(cnd[[1]][['current']][1]),
        start = as.numeric(cnd[[1]][['start']][1:3]),
        pixel = as.integer(cnd[[1]][['pixel']][1:2]),
        step = as.numeric(cnd[[1]][['step']][1:2]),
        instrument = cnd[[1]][['instrument']][1],
        ver = ver
      )
      save4qm(rds, saving)
  })
}

#' (DEPRECATED) Use read_xmap
#' @inheritParams read_xmap
#' @param RDS ignored.
#' @export
qltmap_load <- function(
  wd = '.',
  DT = 0,
  RDS, # ignored
  renew = FALSE,
  saving = TRUE
) {
  .Deprecated(new = 'read_xmap')
  read_xmap(wd = wd, DT = DT, renew = renew, saving = saving)
}
