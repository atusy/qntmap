#' load mapping data
#'
#' @param wd working directory which contains mapping data
#' @param RDS name of RDS file to be saved/readed
#' @param renew if TRUE and the file specified by RDS exists, that file will be loaded
#' @param saving whether or not to save the data as RDS file
#' @importFrom pipeR %>>%
#' @importFrom stringr str_replace
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @export
#'
qltmap_load <- function(wd = NULL, RDS = 'qltmap.RDS', renew = FALSE, saving = TRUE) {
  #when the argument "wd" is assigned, setwd to the argument on start and to the current wd on exit.
  if(!is.null(wd)) {
    cd <- getwd()
    on.exit(setwd(cd))
    setwd(wd)
  }

  #file name patterns of required files
  pattern <- list(
    pm = '\\.[[:alpha:]]+\\.pm',
    map = '_map\\.txt'
  )

  #required files
  filenames <- lapply(pattern, function(x) dir(pattern = x))

  #stop when number of pm files and map files differ or when file names of pm files and map files are not correspondant
  test <- mapply(function(x, y) str_replace(x, y, ''), filenames, pattern)
  if(!is.matrix(test) || any(test[, "pm"] != test[ , "map"])) stop('There are some wrong or missing files of *.pm and/or *_map.txt')
  rm(test)

  #which element is which filenames$map?
  names(filenames$map) <- filenames$pm %>>%
    str_replace('^[0-9]+\\.', '') %>>%
    str_replace('\\.pm$', '')

  #####load, save, and return map files
  qltmap <- NULL
  #load qltmap from RDS file when qltmap_load() has already been done
  if(!renew && file.exists(RDS)) qltmap <- readRDS(RDS)
  #load qltmap from text images when the RDS file does not exist, there is something wrong with RDS file, or renew = TRUE
  if(is.null(qltmap) || any(names(qltmap) != names(filenames$map))) {
    qltmap <- lapply(filenames$map, fread)
    class(qltmap) <- c('list', 'qltmap')
    if(saving) saveRDS(qltmap, 'qltmap.RDS')
  }

  return(qltmap)
}

