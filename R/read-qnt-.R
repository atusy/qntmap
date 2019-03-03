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
#' @param conditions
#'   A path to a csv file which records analytical conditions of elements.
#'   The csv file must have following columns while others are discarded:
#'   `Oxide`, `Element`, `Bg+ [mm]`, `Bg- [mm]`, `Peak [sec]`, and `Bg [sec]`.
#'   `NULL` (default) retrieves analytical conditions from 
#'   `elem.qnt`, `elint.qnt`, and `.cnd/elemw.cnd` / `Pos_0001/data001.cnd`.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang !!
#' @importFrom stringr str_replace_all
#' @importFrom data.table fwrite
#'
#' @export
read_qnt <- function(
  wd = dir(pattern = ('(.*_QNT|^\\.qnt)$'), all.files = TRUE)[1],
  phase_list = NULL,
  renew = FALSE,
  saving = TRUE,
  conditions = NULL
) {
  
  cd <- getwd()
  on.exit(setwd(cd))
  
  if(is.character(phase_list)) {
    stop_if_phase_list_not_found(phase_list)
    phase_list <- normalizePath(phase_list)
  }
  
  if(is.character(conditions)) conditions <- normalizePath(conditions) # © 2018 JAMSTEC
  
  wd <- normalizePath(wd)
  setwd(wd)
  
  if((!renew) & is.null(phase_list) & file.exists('qnt.RDS')) {
    QNT <- readRDS('qnt.RDS')
    attr(QNT, 'dir_qnt') <- wd
    message("Loaded ", file.path(wd, "qnt.RDS"), "\n")
    return(QNT)
  }
  
  #load .qnt files
  qnt <- c(
    'bgm', # background minus
    'bgp', # background plus
    'elem', # element (Oxide: SiO2)
    'elint', # element (Simple: Si)
    # 'krat', # k-ratio
    # 'kraw', # k-raw
    'mes', # measurement
    'net', # net intensity
    'pkint', # peak intensity (may missing)
    'peak', # peak counts
    # 'sigma', # standard deviation
    'stg', # stage
    'wt' # weight percentage
  ) %>>%
    (setNames(paste0(., '.qnt'), .)) %>>%
    `[`(file.exists(.)) %>>%
    lapply(fread)
  
  elemw <- c('.cnd/elemw.cnd', 'Pos_0001/data001.cnd')
  
  #extract elemental data
  elm <- if (is.null(conditions)) {
      data.frame(
        elem = unlist(qnt$elem[1L, -c(1L, 2L)], use.names = FALSE),
        elint = unlist(qnt$elint[1L, -c(1L, 2L)], use.names = FALSE),
        read_qnt_elemw(elemw[file.exists(elemw)][1L])
      )
    } else {
      transmute( # © 2018 JAMSTEC
        fread(conditions), # © 2018 JAMSTEC
        elem = Oxide, elint = Element, # © 2018 JAMSTEC
        bgp_pos = `Bg+ [mm]`, bgm_pos = `Bg- [mm]`, # © 2018 JAMSTEC
        pk_t = `Peak [sec]`, bg_t = `Bg [sec]` # © 2018 JAMSTEC
      ) # © 2018 JAMSTEC
    }
  
  rm(elemw)
  
  cnd <- mutate(
      setNames(qnt$stg[, c(1L, 5L:7L, 10L)], c('id', 'x', 'y', 'z', 'comment')),
      beam = !!qnt$mes$V3,
      phase =
        if(is.null(!!phase_list)) {
          str_replace_all(
            comment, c('[:blank:]{2,}' = ' ', ' $' = '', '^ ' = '')
          )
        } else {
          mutate(
            fread(phase_list),
            use = `if`(exists('use'), use, TRUE), 
            phase = ifelse(use, phase, NA_character_)
          )[["phase"]]
        }
    )

  #extract compositional data
  #bgm, bgp, pkint, bgint [cps/uA]
  cmp <- qnt[names(qnt) %in% c('bgm', 'bgp', 'net', 'pkint', 'wt')] %>>%
    # 1st and 2nd collumns are id and integeer, last column is sum
    lapply(`[`, seq_along(elm$elem) + 2L) %>>% 
    lapply(setNames, elm$elem)

  if(is.null(phase_list) & (!file.exists('phase_list0.csv')) & saving) {
    fwrite(
      cbind(cnd[c('id', 'phase')], use = TRUE), 
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

stop_if_phase_list_not_found <- function(phase_list) {
  if(!file.exists(phase_list))
    stop(
      "Not found: ", phase_list, "\n",
      "Specify phase_list parameter by an absolute path or",
      "a relative path from the current directory: ", cd
    )
}
