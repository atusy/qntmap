#' read X-ray map data
#'
#' @param wd working directory which contains mapping data
#' @param DT dead time in nano seconds (0 nsec in default)
#' @param RDS name of RDS file to be saved/readed
#' @param renew if TRUE and the file specified by RDS exists, that file will be loaded
#' @param saving whether or not to save the data as RDS file
#'
#' @importFrom pipeR pipeline
#' @importFrom purrr map_if
#' @importFrom stringr str_replace
#' @importFrom data.table fread
#' @importFrom stats setNames
#' @export
#'
read_xmap <- function(
  wd = '.',
  DT = 0,
  RDS = 'xmap.RDS',
  renew = FALSE,
  saving = TRUE,
  .map = '(data)?[0-9]*[1-9](\\.csv|_map\\.txt)', 
  .cnd = '(data)?[0-9]*[1-9]\\.cnd'
) {
  #when the argument "wd" is assigned, setwd to the argument on start and to the current wd on exit.
  
  cd <- getwd()
  on.exit(setwd(cd))
  setwd(wd)
  
  if(renew) return(readRDS(RDS))
  
  dwell <- read_map_beam(dir(pattern = '^(0|map)\\.cnd$'))['dwell'] * 1e-3

  #file name patterns of required files
  patterns <- list(
    pm = '\\.[[:alpha:]]+\\.(pm|bmp)',
    map = '(_map\\.txt)|(data.*\\.csv)'
  )

  #required files
  filenames <- lapply(patterns, function(x) dir(pattern = x))
  n <- pipeline({
    filenames
    lapply(str_extract, '[:number:]+')
    lapply(as.integer)
    lapply(order)
  })
    
  filenames <- Map(`[`, filenames, n)
  filenames$elm <- pipeline({
    filenames$pm
      str_replace('^[0-9]+\\.', '')
      str_replace('\\.(pm|bmp)$', '')
  })

  #####load, save, and return map files
  #load qltmap from RDS file when qltmap_load() has already been done
  # load qltmap from text images when the RDS file does not exist,
  # there is something wrong with RDS file, or renew = TRUE
  pipeline({
    lapply(filenames$map, fread)
      setNames(filenames$elm) 
      map_if(
        names(.) %nin% c('CP', 'TP', 'SL'),
        function(x) dwell * x / (dwell - DT * 1e-9 * x)
      ) 
      lapply(round) 
      lapply(lapply, as.integer) 
      lapply(as.data.frame) 
      structure(
        class = c('xmap', class(.)),
        deadtime = DT
      )
      ~ if(saving) saveRDS(., 'xmap.RDS')
  })
}

#' DEPRECATED!! use read_xmap
#' @inheritParams read_xmap
#' @export
qltmap_load <- function() {
  warning('qltmap_load is deprecated. Use read_xmap instead.')
  read_xmap(wd, DT, RDS, renew, saving)
}

formals(qltmap_load) <- formals(read_xmap)

