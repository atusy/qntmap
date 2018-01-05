#' load mapping data
#'
#' @param wd working directory which contains mapping data
#' @param DT dead time in nano seconds (1100 nsec in default)
#' @param RDS name of RDS file to be saved/readed
#' @param renew if TRUE and the file specified by RDS exists, that file will be loaded
#' @param saving whether or not to save the data as RDS file
#'
#' @importFrom pipeR %>>%
#' @importFrom stringr str_replace
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @export
#'
qltmap_load <- function(
  wd = NULL,
  DT = 1100,
  RDS = 'qltmap.RDS',
  renew = FALSE,
  saving = TRUE
) {
  #when the argument "wd" is assigned, setwd to the argument on start and to the current wd on exit.
  if(!is.null(wd)) {
    cd <- getwd()
    on.exit(setwd(cd))
    setwd(wd)
  }

  dwell <- '0.cnd' %>>%
    readLines %>>%
    `[`(str_detect(., 'Dwell Time \\[msec\\]')) %>>%
    str_replace('[:blank:].*$', '') %>>%
    as.numeric %>>%
    `*`(1e-3)

  #file name patterns of required files
  patterns <- list(
    pm = '\\.[[:alpha:]]+\\.pm',
    map = '_map\\.txt'
  )

  #required files
  filenames <- lapply(patterns, function(x) dir(pattern = x))

  #stop when number of pm files and map files differ or when file names of pm files and map files are not correspondant
  test <- mapply(
      function(x, y) str_replace(x, y, ''),
      filenames,
      patterns
    )
  if(!is.matrix(test) || any(test[, "pm"] != test[ , "map"]))
    stop('There are some wrong or missing files of *.pm and/or *_map.txt')
  rm(test, patterns)

  #which element is which filenames$map?
  names(filenames$map) <- filenames$pm %>>%
    str_replace('^[0-9]+\\.', '') %>>%
    str_replace('\\.pm$', '')

  #####load, save, and return map files
  #load qltmap from RDS file when qltmap_load() has already been done
  if(renew && file.exists(RDS)) {
    qltmap <- readRDS(RDS)
  } else {
  # load qltmap from text images when the RDS file does not exist,
  # there is something wrong with RDS file, or renew = TRUE
    qltmap <- lapply(filenames$map, fread) %>>%
      lapply(
        function(x, .DT = DT) x / (dwell - DT * 1e-9 * x)
      ) %>>%
      lapply(round) %>>%
      lapply(lapply, as.integer) %>>%
      lapply(as.data.table)
    class(qltmap) <- c('list', 'qltmap')
    if(saving) saveRDS(qltmap, 'qltmap.RDS')
  }

  return(qltmap)
}

