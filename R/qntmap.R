#' interactively quantify X-ray maps
#' 
#' @importFrom easycsv choose_dir
#' @importFrom utils menu select.list
#' @export
qntmap <- function() {
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  cat('(1) select any file in the directory which contains .map and .qnt directory\n')
  setwd(wd <- choose_dir()) # easycsv::choose_dir
  if(!all(file.exists(c('.map', '.qnt'))))
    while(!all(file.exists(c('.map', '.qnt')))) {
      cat(
        '(1) Selected directory does not contain ',
        '.map and .qnt directory. Select again\n'
      )
      setwd(wd <- choose_dir()) # easycsv::choose_dir
    }
  cat('working directory is settled to\n', wd, '\n\n')
  
  cat('(2) select directory which contains *_map.txt files to be clustered or quantified\n')
  dir_map <- select.list(list.dirs('.map', recursive = FALSE))
  cat('*_map.txt in a following directory will be clustered or quantified\n', dir_map, '\n\n')

  if(!length(list.files(dir_map, pattern = '_map\\.txt')))
    stop(
      'Selected directory does not contain *_map.txt files. ',
      'Did you converted mapping data to ASCII files from Utility menu in JEOL EPMA?'
    )
  
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
  qnt <- read_qnt('.qnt', phase_list, renew = TRUE)
  cat('Peforming cluster analysis\n')
  centers <- find_centers(xmap = xmap, qnt = qnt)
  cls <- cluster_xmap(xmap = xmap, centers = centers)
  cat('Finished cluster analysis. Result is in ', dir_map, '/clustering\n\n')
  
  cat(
    'Specify phases tending to be ',
    'finer than mapping probe diameter'
  )
  fine_phase <- select.list(
    colnames(cls$membership), multiple = TRUE
  )
  cat('\n')
  
  cat('Quantifying mapping data\n')
  qmap <- quantify(
    xmap = xmap,
    qnt = qnt,
    cluster = cls,
    fine_phase = if(length(fine_phase)) fine_phase else NULL
  )
  
  cat(
    'Finished quantification.\n', 
    'Results are saved under',
    paste0(dir_map, '/qntmap\n\n')
  )
  
  cat('Summary of quantified mapping data\n')
  print(summary(qmap))
  
  invisible(qmap)
}
