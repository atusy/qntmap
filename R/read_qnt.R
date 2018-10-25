#' compile quantitative data
#'
#' @param wd directory path containing .qnt files
#' @param phase_list path to the csv file containing columns indicating phase of each analysis and true or false to use it for quantifying.
#' @param renew if TRUE and the file specified by RDS exists, that file will be loaded
#' @param saving whether or not to save the data as RDS file
#'
#' @importFrom dplyr select
#' @importFrom dplyr one_of
#' @importFrom dplyr mutate
#' @importFrom pipeR %>>%
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom data.table fwrite
#'
#' @export
read_qnt <- function(
  wd = dir(pattern = ('(.*_QNT|^\\.qnt)$'), all.files = TRUE)[1],
  phase_list = NULL,
  renew = FALSE,
  saving = TRUE
) {

  cd <- getwd()
  on.exit(setwd(cd))

  wd <- normalizePath(wd)
  setwd(wd)

  if(!renew && file.exists('qnt.RDS')) {
    QNT <- readRDS('qnt.RDS')
    attr(QNT, 'dir_qnt') <- wd
    return(QNT)
  }
  
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
    (setNames(paste0(., '.qnt'), .)) %>>%
    `[`(file.exists(.)) %>>%
    lapply(fread)

  elemw <- c('.cnd/elemw.cnd', 'Pos_0001/data001.cnd')
  
  #extract elemental data
  elm <- data.frame(
      elem = unlist(qnt$elem[1, -c(1, 2)], use.names = FALSE),
      elint = unlist(qnt$elint[1, -c(1, 2)], use.names = FALSE),
      read_qnt_elemw(elemw[file.exists(elemw)][1])
    )
  
  rm(elemw)

  if(!file.exists(phase_list) && !grepl('/', phase_list)) {
    phase_list2 <- file.path(cd, phase_list)
    if(!file.exists(phase_list2)) stop('"', phase_list, '" not found')
    warning(
      '\n',
      'Found phase_list in current directory: ',
      cd,
      '\n',
      'From qntmap v0.2.0, ', 
      'phase_list parameter needs be an absolute path, ', 
      'or a relative path from parameter wd', 
      '\n\n'
    )
    phase_list <- phase_list2
    rm(phase_list2)
  }
  
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

  if(is.null(phase_list) && !file.exists('phase_list0.csv')) 
    fwrite(cbind(cnd[c('id', 'phase')], use =TRUE), 'phase_list0.csv')
  
  save4qm(
    structure(
      list(elm = elm, cnd = cnd, cmp = cmp), #, raw = list(cnd = cnd0, qnt = qnt)),
      dir_qnt = wd,
      class = c('qm_qnt', 'list')
    ),
    'qnt.RDS',
    saving
  )

}

#' @rdname read_qnt
#' @inheritParams read_qnt
#' @param RDS ignored
#' @export
qnt_load <- function(
  wd = NULL, RDS, phase_list = NULL, renew = FALSE, saving = TRUE
) {
  .Deprecated(new = 'read_qnt')
  read_qnt(wd = wd, phase_list = phase_list, renew = renew, saving = saving)
}
