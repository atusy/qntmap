#' quantify qualtitative mapping data
#'
#' @param wd working directory which contains .qnt and .map directories
#' @param dir_map directory containing map data to be quantified
#' @param maps_x x of maps. Assign when you use guide net map.
#' @param maps_y y of maps. Assign when you use guide net map.
#' @param RDS_cluster path to the output RDS file of clustering. NULL in default look for the newest one in dir_map/clustering
#' @param fine_phase fine-grained phases which tend to be appear in multi-phase pixels
#' @param fine_th 0.9
#' @param qnt object of class qnt
#' @param qltmap object of class qltmap
#' @param cluster object of class PoiClaClu
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fwrite
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr right_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom pipeR %>>%
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map_dbl
#' @importFrom purrr map2
#' @importFrom purrr walk2
#' @importFrom purrr reduce
#' @importFrom rlang set_names
#' @importFrom rlang :=
#' @importFrom stats coef
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats sd
#' @importFrom stats vcov
#' @importFrom stats na.omit
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#'
#'@export
qntmap_quantify <- function(
  wd = '.',
  dir_map,
  RDS_cluster,
  maps_x = NULL,
  maps_y = NULL,
  fine_phase = NULL,
  fine_th = 0.9,
  qnt = read_qnt(wd),
  qltmap = read_xmap(dir_map),
  cluster = readRDS(RDS_cluster),
) {

  cd <- getwd()
  on.exit(setwd(cd))
  setwd(wd)

  #mapping conditions
  pos <- read_map_pos(paste0(dir_map, '/0.cnd'))
  
  if(is.null(maps_x)) maps_x <- pos$px[1]
  if(is.null(maps_y)) maps_y <- pos$px[2]
  
  stg <- expand.grid(
    x_stg = seq(0, pos$px[1] - 1) %/% maps_x + 1,
    y_stg = seq(0, pos$px[2] - 1) %/% maps_y + 1
  ) %>>%
    mutate(stg = flag0(x_stg, y_stg))

  #tidy compilation of epma data
  distinguished <- any(grepl('_', colnames(cluster$membership)))
  epma <- epma_tidy(
    wd = wd, dir_map = dir_map, qnt = qnt, qltmap = qltmap, cluster = cluster
  ) %>>%
    filter(elm %in% qnt$elm$elem) %>>%
    mutate(
      net = net * (net > 0),
      phase3 = if(distinguished) phase else phase2,
      x_stg = ((x_px - 1) %/% maps_x + 1) * (0 < x_px) * (x_px <= pos$px[1]), 
      y_stg = ((y_px - 1) %/% maps_y + 1) * (0 < y_px) * (y_px <= pos$px[2]),
      stg = ifelse((x_stg * y_stg) <= 0, NA, flag0(x_stg, y_stg)),
      mem = mem * 
        (str_replace(cls, '_.*', '') == phase2) *
        # (cls == phase3) *
        !(cls %in% fine_phase) * 
        (mem > fine_th) * 
        is.na(stg)
    )

  #qltmap: elements -> oxides
  qltmap <- qltmap %>>%
    `[`(qnt$elm$elint) %>>%
    setNames(qnt$elm$elem) %>>%
    `[`(sort(names(.)))

  rm(qnt)

  X <- as.data.frame(cluster$membership)

  rm(cluster)
  

  B <- epma %>>%
    filter(!is.na(stg)) %>>%
    group_by(elm) %>>%
    mutate(
      fit_na = list(lm(pkint ~ 0 + map, weights = mem, na.action = na.omit))
    ) %>>%
    group_by(stg, elm) %>>%
    summarise(
      fit = lm(pkint ~ 0 + map, weights = mem) %>>% list,
      fit_na = fit_na[1]
    ) %>>%
    ungroup %>>%
    mutate(
      b = map_dbl(fit, coef),
      fit = ifelse(is.na(b), fit_na, fit),
      b_se = map_dbl(fit, vcov),
      b = ifelse(is.na(b), map_dbl(fit_na, coef), b)
    ) %>>%
    select(-fit, -fit_na) %>>%
    nest(-stg, .key = '.B')
  AG <- qntmap_AG(epma) # return also A

  rm(epma)

  XAG <- AG %>>%
    select(phase3, elm, ag, ag_se) %>>%
    nest(-elm) %>>%
    (.x ~ set_names(.x$data, .x$elm)) %>>%
    map(function(x) map(x %>>% select(-phase3), set_names, x$phase3)) %>>%
    map(map, `*`, t(X)) %>>%
    map(set_names, c('val', 'se')) %>>%
    map(map_at, 'se', `^`, 2) %>>%
    map(map, colSums) %>>%
    map(map_at, 'se', sqrt)

  
  setwd(dir_map)
  dir.create('qntmap', FALSE)
  setwd('qntmap')
  

  AG %>>% #AB
    select(phase3, elm, a, a_se) %>>%
    nest(-phase3, .key = '.A') %>>%
    mutate(
      .A = .A %>>%
        map(
          function(.A) B %>>%
            mutate(
              .B = map(
                .B,
                mutate,
                val = b * .A$a,
                se = sqrt((b * .A$a_se) ^ 2 + (.A$a * b_se) ^ 2)
              )
            )
        ) %>>%
        map(unnest) %>>%
        map(select, -b, -b_se)
    ) %>>%
    unnest %>>%
    nest(-elm) %>>%
    mutate(
      val = data %>>%
        map(select, -se) %>>%
        map(spread, phase3, val),
      se = data %>>%
        map(select, -val) %>>%
        map(spread, phase3, se)
    ) %>>%
    select(-data) %>>%
    nest(-elm) %>>%
    (.x ~ set_names(.x$data, .x$elm)) %>>%
    map(unlist, recursive = FALSE) %>>%
    map(map, right_join, tibble(stg = stg$stg), by = 'stg') %>>%
    map(map, select, -stg) %>>%
    map(map, as.data.table) %>>%
    map(map, `*`, X) %>>% #XAB
    map(map_at, 'se', map, `^`, 2) %>>%
    map(map_at, 'se', as.data.table) %>>%
    map(map, rowSums) %>>%
    map(map_at, 'se', sqrt) %>>%
    map2(qltmap, function(xab, i) map(xab, function(x) i * x)) %>>% #XABI
    map2(XAG, map2, `-`) %>>% #XABI - XAG
    map(set_names, c('wt', 'se')) %>>%
    map(function(x) map(x, `*`, x$wt > 0)) %>>%
    c(
      list(Total = list(
        wt = . %>>%
          map(`[[`, 'wt') %>>%
          reduce(`+`) %>>%
          as.data.frame,
        se = . %>>%
          map(`[[`, 'se') %>>%
          map(`^`, 2) %>>%
          reduce(`+`) %>>%
          sqrt %>>%
          as.data.frame
      ))
    ) %>>%
    structure(class = c('qntmap', 'list')) %>>%
    (~ . %>>% #side effect saving csv and RDS files
       (~ saveRDS(., 'qntmap.RDS')) %>>%
       unlist(recursive = FALSE) %>>%
       set_names(
         names(.) %>>%
           str_replace('\\.', '_') %>>%
           paste0('.csv')
       ) %>>%
       walk2(names(.), fwrite) %>>%
       NULL
    ) %>>%
    return()
}