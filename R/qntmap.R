#' quantify qualtitative mapping data interactively
#'
#' @importFrom easycsv choose_dir
#' @importFrom utils select.list
#' @export
qntmap <- function() {
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  cat('(1) select any file in the directory which contains .map and .qnt directory\n')
  setwd(wd <- easycsv::choose_dir())
  if(!all(c('.map', '.qnt') %in% list.files(all.files = TRUE, include.dirs = TRUE)))
    while(!all(c('.map', '.qnt') %in% list.files(all.files = TRUE, include.dirs = TRUE))) {
      cat('(1) Selected directory does not contain .map and .qnt directory. Select again\n')
      setwd(wd <- easycsv::choose_dir())
    }
  cat('working directory is settled to\n')
  cat(wd)
  cat('\n\n')
  
  cat('(2) select directory which contains *_map.txt files to be clustered or quantified\n')
  dir_map <- select.list(list.dirs('.map', recursive = FALSE))
  cat('*_map.txt in a following directory will be clustered or quantified')
  cat(dir_map)
  cat('\n\n')
  
  if(!length(list.files(dir_map, pattern = '_map\\.txt')))
    stop('Selected directory does not contain *_map.txt files. Did you converted mapping data to ASCII files from Utility menu in JEOL EPMA?')
  
  cat(
    'Input dead time in nano seconds\n',
    '0 if no corrections required)\n',
    '1100 is the default value for JXA-8105\n',
    sep = ''
  )
  DT <- as.numeric(readline())
  cat('Dead time is ', DT, ' nano seconds\n\n')
  
  cat('Identify phase names of quantified points based on')
  selection <- menu(
    c(
      'comments input during quantification',
      'an external csv file'
    )
  )
  cat('\n')
  if(selection == 1) phase_list <- NULL
  if(selection == 2) {
      cat('Choose an external csv file\n')
      phase_list <- file.choose()
      cat('Chosen')
      cat(phase_list)
      cat('\n\n')
    }
  
  cat('Loading mapping data\n')
  xmap <- read_xmap(dir_map, DT = DT, renew = TRUE)
  cat('Loading quantified data\n')
  qnt <- read_qnt(wd, phase_list, renew = TRUE)
  cat('Peforming cluster analysis\n')
  centers <- cluster_centers(qnt = qnt, qltmap = xmap, dir_map = dir_map)
  cls <- cluster_xmap(centers, xmap, wd = dir_map)
  cat('Finished cluster analysis. Result is in ')
  cat(dir_map)
  cat('/clustering\n\n')
  
  cat('Select phases which tend to be fine grains compared to mapping probe diameter.\n')
  fine_phase <- select.list(colnames(cls$membership))
  cat('\n')
  
  cat('Quantifying mapping data\n')
  qmap <- qmap_quantify(
    wd = wd,
    dir_map = dir_map,
    qnt = qnt,
    xmap = xmap,
    cluster = cls,
    fine_phase = if(length(fine_phase)) fine_phase else NULL
  )
  
  cat('Finished quantification.\n')
  cat('Results are saved under')
  cat(paste0(dir_map, '/qntmap\n\n'))
  
  cat('Summary of quantified mapping data')
  print(summary(qmap))
  
  invisible(qmap)
}
