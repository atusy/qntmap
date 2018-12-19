#' compile quantitative data
#'
#' @param wd directory path containing .qnt files
#' @param phase_list 
#'   A path to a csv file containing columns indicating 
#'   phase of each analysis and `TRUE` or `FALSE` to use it for quantifying.
#' @param renew 
#'   `TRUE` (default) read all data from original files.
#'   `FALSE` tries to read `qnt.RDS` exists in `wd`, instead.
#' @param saving 
#'   `TRUE` (default) or `FALSE` to save the result as `qnt.RDS` file in `wd`.
#'
#' @importFrom dplyr select one_of mutate
#' @importFrom stringr str_replace_all str_replace str_detect
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
  
  if(is.character(phase_list)) {
    if(!file.exists(phase_list))
      stop(
        phase_list, 
        "\n not found.",
        "Specify phase_list parameter by an absolute path or",
        "a relative path from the current directory: ",
        cd
      )
    phase_list <- normalizePath(phase_list)
  }
  
  wd <- normalizePath(wd)
  setwd(wd)
  
  if(!renew && is.null(phase_list) && file.exists('qnt.RDS')) {
    QNT <- readRDS('qnt.RDS')
    attr(QNT, 'dir_qnt') <- wd
    message(
      "Loaded ", file.path(wd, "qnt.RDS"), "\n"
    )
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
  
  cnd <- mutate(
      setNames(
          qnt$stg[, c(1, 5, 6, 7, 10)],
          c('id', 'x', 'y', 'z', 'comment')
        ),
      beam = qnt$mes$V3,
      phase =
        if(is.null(phase_list)) {
          str_replace_all(
            comment,
            c('[:blank:]{2,}' = ' ', ' $' = '', '^ ' = '')
          )
        } else {
          mutate(
            fread(phase_list),
            use = `if`(exists('use'), use, TRUE),
            phase = ifelse(use, phase, NA)
          )[["phase"]]
        }
    )

  #extract compositional data
  #bgm, bgp, pkint, bgint [cps/uA]
  cmp <- qnt[names(qnt) %in% c('bgm', 'bgp', 'net', 'pkint', 'wt')] %>>%
    # 1st and 2nd collumns are id and integeer, last column is sum
    lapply(`[`, seq_along(elm$elem) + 2) %>>% 
    lapply(setNames, elm$elem)

  if(is.null(phase_list) && !file.exists('phase_list0.csv') && saving) {
    fwrite(
      cbind(cnd[c('id', 'phase')], use =TRUE), 
      file.path(cd, 'phase_list0.csv')
    )
    message("phase_list0.csv is created in ", cd)
  }
  
  save4qm(
    structure(
      list(elm = elm, cnd = cnd, cmp = cmp),
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
