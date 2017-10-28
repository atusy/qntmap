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
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom stringr str_c
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
qnt_load <- function(wd = NULL, RDS = 'qnt.RDS', phase_list = NULL, renew = FALSE, saving = TRUE) {
  cd <- getwd()
  on.exit(setwd(cd))

  if(!is.null(wd)) setwd(wd)

  if(!renew && file.exists(RDS)) return(readRDS(RDS))

  if(!file.exists('.qnt')) stop('wd must be a path where .qnt directory exists')

  #load .cnd files
  cnd0 <- './.qnt/.cnd/' %>>%
    list.files(full.names = TRUE) %>>%
    set_names(str_replace_all(., '(^.*/)|(\\.cnd$)', '')) %>>%
    map(readLines)

  #load .qnt files
  qnt <- c('bgm', 'bgp', 'elem', 'elint', 'krat', 'kraw', 'mes', 'net', 'pkint', 'sigma', 'stg', 'wt') %>>%
    (str_c('./.qnt/', ., '.qnt')) %>>%
    set_names(str_replace_all(., '(^.*/)|(\\.qnt$)', '')) %>>%
    map(fread)

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
        set_names(c('bgp_pos', 'bgm_pos', 'pk_t', 'bg_t'))
    )

  #extract analytical conditions of each analysis
  cnd <- qnt$stg %>>%
    setNames(c('id', 'group', 'sample', 'id2', 'x', 'y', 'z', 'aux1', 'aux2', 'comment', 'aux3')) %>>%
    select(id, x, y, z, comment) %>>%
    mutate(
      beam = qnt$mes$V3,
      phase =
        if(is.null(phase_list)) {
          comment
        } else {
          phase_list %>>%
            fread %>>%
            mutate(use = ifelse(rep('use' %in% names(.), nrow(.)), use, TRUE)) %>>%
            mutate(phase = ifelse(use, phase, NA)) %>>%
            (phase)
        }
    )


  #extract compositional data
  #bgm, bgp, pkint [cps/uA]
  cmp <- qnt %>>%
    `[`(c('bgm', 'bgp', 'krat', 'kraw', 'net', 'pkint', 'sigma', 'wt')) %>>%
    map(set_names, c('id', 'num', elm$elem, 'sum')) %>>%
    map(select, one_of(elm$elem)) %>>%
    c(list(
      bgint = t((t(.$bgm) * elm$bgp_pos + t(.$bgp) * elm$bgm_pos) / (elm$bgp_pos + elm$bgm_pos)) %>>% as.data.table
    )) %>>%
    c(list(
      pk = .$pkint * cnd$beam * 1e+6,
      bg = .$bgint * cnd$beam * 1e+6
    ))

  QNT <- list(elm = elm, cnd = cnd, cmp = cmp, raw = list(cnd = cnd0, qnt = qnt))
  class(QNT) <- c('list', 'qnt')

  if(saving) saveRDS(QNT, RDS)

  return(QNT)

}

