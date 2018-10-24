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
#' @importFrom data.table fread
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
  
  if(!renew && file.exists('xmap.RDS')) {
    xmap <- readRDS('xmap.RDS')
    return(structure(xmap, dir_map = wd))
  }
  
  files_xmap <- dir(pattern = .map)
  files_cnd <- dir(pattern = .cnd)
  if(length(files_xmap) != length(files_cnd)) {
    cat('file names of mapping data:', files_xmap)
    cat('file names of mapping conditions:', files_cnd)
    stop(
      'Length of files of xmap and cnd are different.' , 
      'Check parameters .map and .cnd'
    )
  }

  pattern_cnd <- paste(
      c(
        old = c('XM_ELEMENT', 'XM_DWELL_TIME', 'CM_CURRENT'),
        new = c(
          'XM_ELEM_NAME', 'XM_ELEM_IMS_SIGNAL_TYPE', 
          'XM_AP_SA_DWELL_TIME', 'XM_DATA_PROBE_CURRENT'
        )
      ),
      collapse = '|'
    )
  
  cnd <- lapply(files_cnd, read_cnd, pattern = pattern_cnd)

  elm <- pipeline({
    cnd
      map(`[`, c('XM_ELEM_NAME', 'XM_ELEM_IMS_SIGNAL_TYPE', 'XM_ELEMENT'))
      map(map, map, levels)
      unlist(use.names = FALSE)
  })
  
  dwell <- read_map_beam(cnd[[1]])['dwell'] * 1e-3

  #####load, save, and return map files
  # load qltmap from RDS file when qltmap_load() has already been done
  # load qltmap from text images when the RDS file does not exist,
  # there is something wrong with RDS file, or renew = TRUE
  pipeline({
    lapply(files_xmap, fread)
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
        dwell = dwell
      )
      save4qm('xmap.RDS', saving)
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
