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
#' @importFrom data.table fread
#'
#' @export
#'
#'
read_qnt <- function(
  wd = NULL,
  RDS = 'qnt.RDS',
  phase_list = NULL,
  renew = FALSE,
  saving = TRUE
) {

  cd <- getwd()
  on.exit(setwd(cd))

  if(!is.null(wd)) setwd(wd)

  if(!renew && file.exists(RDS))
    return(readRDS(RDS))

  if(!file.exists('.qnt'))
    stop('wd must be a path where .qnt directory exists')

  #load .cnd files
  cnd0 <- './.qnt/.cnd/' %>>%
    list.files(full.names = TRUE) %>>%
    setNames(str_replace_all(., '(^.*/)|(\\.cnd$)', '')) %>>%
    lapply(readLines)

  #load .qnt files
  qnt <- c(
      'bgm',
      'bgp',
      'elem',
      'elint',
      'krat',
      'kraw',
      'mes',
      'net',
      'pkint',
      'sigma',
      'stg',
      'wt'
    ) %>>%
    (paste0('./.qnt/', ., '.qnt')) %>>%
    `[`(file.exists(.)) %>>%
    setNames(str_replace_all(., '(^.*/)|(\\.qnt$)', '')) %>>%
    lapply(fread)

  #extract elemental data
  elm <- data.table(
      elem = qnt$elem[1, -(1:2)] %>>% unlist(use.names = FALSE),
      elint = qnt$elint[1, -(1:2)] %>>% unlist(use.names = FALSE),
      cnd0$elemw %>>%
        `[`(str_detect(
          .,
          '((Back |BG)[\\+-])|(Meas. Time \\[sec\\])|((Pk|BG) t)')
        ) %>>%
        str_replace('[:blank:].*$', '') %>>%
        as.double %>>%
        matrix(ncol = 4, byrow = TRUE) %>>%
        as.data.table %>>%
        setNames(c('bgp_pos', 'bgm_pos', 'pk_t', 'bg_t'))
    )

  cnd <- qnt$stg[c(1, 5, 6, 7, 10)] %>>% 
    setNames(c('id', 'x', 'y', 'z', 'comment')) %>>%
    mutate(
      beam = qnt$mes$V3,
      phase =
        if(is.null(phase_list)) {
          comment
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
  cmp <- qnt[
      names(qnt) %in% 
        c('bgm', 'bgp', 'krat', 'kraw', 'net', 'pkint', 'sigma', 'wt')
    ] %>>%
    lapply(setNames, c('id', 'num', elm$elem, 'sum')) %>>%
    lapply(select, one_of(elm$elem))

  QNT <- structure(
      list(elm = elm, cnd = cnd, cmp = cmp), #, raw = list(cnd = cnd0, qnt = qnt)),
      class = c('qnt', 'list')
    )

  if(saving) saveRDS(QNT, RDS)

  return(QNT)

}

#' DEPRECATED!! Use read_qnt
#' @inheritParams read_qnt
#' @export
qnt_load <- function(
  wd = NULL, RDS = 'qnt.RDS', phase_list = NULL, renew = FALSE, saving = TRUE
) {
  warning('qnt_load is deprecated use read_xmap')
  read_qnt(wd, RDS, phase_list, renew, saving)
}
