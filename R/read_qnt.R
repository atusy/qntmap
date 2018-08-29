#' compile quantitative data
#'
#' @param wd path to the directory containing .qnt directory
#' @param RDS name of RDS file to be saved/readed
#' @param phase_list path to the csv file containing columns indicating phase of each analysis and true or false to use it for quantifying.
#' @param renew if TRUE and the file specified by RDS exists, that file will be loaded
#' @param saving whether or not to save the data as RDS file
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom pipeR %>>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table fwrite
#'
#' @export
#'
#'
read_qnt <- function(
  wd = '.',
  RDS = 'qnt.RDS',
  phase_list = NULL,
  renew = FALSE,
  saving = TRUE
) {

  cd <- getwd()
  on.exit(setwd(cd))
  setwd(wd)

  if(!renew && file.exists(RDS)) return(readRDS(RDS))

  dir_qnt <- dir(
      wd, pattern = ('(.*_QNT|^\\.qnt)$'), all.files = TRUE
    )[1]
  if(is.na(dir_qnt)) stop(
    'wd must be a path where .qnt or *_QNT directory exists'
  )

  #load .qnt files
  qnt <- c(
      'bgm',
      'bgp',
      'elem',
      'elint',
      # 'krat',
      # 'kraw',
      'mes',
      'net',
      'pkint',
      'peak',
      # 'sigma',
      'stg',
      'wt'
    ) %>>%
    (paste0(dir_qnt, '/', ., '.qnt')) %>>%
    `[`(file.exists(.)) %>>%
    setNames(str_replace_all(., '(^.*/)|(\\.qnt$)', '')) %>>%
    lapply(fread)

  elemw <- paste(
      dir_qnt, 
      c('.cnd/elemw.cnd', 'Pos_0001/data001.cnd'), 
      sep = '/'
    )
  
  #extract elemental data
  elm <- data.table(
      elem = unlist(qnt$elem[1, -c(1, 2)], use.names = FALSE),
      elint = unlist(qnt$elint[1, -c(1, 2)], use.names = FALSE),
      read_qnt_elemw(elemw[file.exists(elemw)][1])
    )
  
  rm(elemw)

  cnd <- qnt$stg[, c(1, 5, 6, 7, 10)] %>>% 
    setNames(c('id', 'x', 'y', 'z', 'comment')) %>>%
    mutate(
      beam = qnt$mes$V3,
      phase =
        if(is.null(phase_list)) {
          comment %>>%
            str_replace_all('[:blank:]{2,}', ' ') %>>%
            str_replace(' $', '') %>>%
            str_replace('^ ', '')
        } else {
          phase_list %>>%
            fread %>>%
            mutate(use = if(exists('use')) use else TRUE) %>>%
            mutate(phase = ifelse(use, phase, NA)) %>>%
            (phase)
        }
    )


  #extract compositional data
  #bgm, bgp, pkint, bgint [cps/uA]
  cmp <- qnt[names(qnt) %in% c('bgm', 'bgp', 'net', 'pkint', 'wt')] %>>%
    lapply(setNames, c('id', 'num', elm$elem, 'sum')) %>>%
    lapply(select, one_of(elm$elem))

  QNT <- structure(
      list(elm = elm, cnd = cnd, cmp = cmp), #, raw = list(cnd = cnd0, qnt = qnt)),
      class = c('qnt', 'list')
    )

  if(is.null(phase_list) && !file.exists('phase_list0.csv')) pipeline({
    cnd
    select(id, phase)
    mutate(use = TRUE)
    fwrite('phase_list0.csv')
  })
  
  save4qm(QNT, 'qnt.RDS', saving)

}

#' DEPRECATED!! Use read_qnt
#' @inheritParams read_qnt
#' @export
qnt_load <- function(
  wd = '.', RDS = 'qnt.RDS', phase_list = NULL, renew = FALSE, saving = TRUE
) {
  warning('qnt_load is deprecated use read_xmap')
  read_qnt(wd, RDS, phase_list, renew, saving)
}
