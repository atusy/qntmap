#' quantify qualtitative mapping data
#'
#' @param wd working directory which contains .qnt and .map directories. Default is current directory.
#' @param dir_map directory containing map data to be quantified
#' @param maps_x x of maps. Assign when you use guide net map.
#' @param maps_y y of maps. Assign when you use guide net map.
#' @param RDS_cluster path to the output RDS file of clustering. NULL in default look for the newest one in dir_map/clustering
#' @param fine_phase fine-grained phases which tend to be appear in multi-phase pixels
#' @param fine_th 0.9
#' @param qnt object of class qnt
#' @param qltmap object of class qltmap
#' @param cluster object of class PoiClaClu
#' @param fix fix compositions
#'
#' @importFrom data.table fwrite
#' @importFrom pipeR %>>%
#' @importFrom pipeR pipeline
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map2
#' @importFrom purrr walk2
#' @importFrom stats setNames
#' @importFrom stringr str_replace
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
  fix = NULL
) {

  cd <- getwd()
  on.exit(setwd(cd))
  setwd(wd)

  #mapping conditions
  pos <- read_map_pos(paste0(dir_map, '/0.cnd'))
  
  stg <- do.call(
    flag0,
    unclass(expand.grid(
      x_stg = seq(0, pos$px[1] - 1) %/% maps_x + 1,
      y_stg = seq(0, pos$px[2] - 1) %/% maps_y + 1
    ))
  )

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
        !(cls %in% fine_phase) * 
        (mem > fine_th) 
    )

  rm(qnt)

  X <- as.data.frame(cluster$membership)

  rm(cluster)
  
  AG <- qntmap_AG(epma) # return also A

  B <- qntmap_B(epma)

  rm(epma)

  XAG <- qntmap_XAG(X, AG)

  setwd(dir_map)
  dir.create('qntmap', FALSE)
  setwd('qntmap')
  
  qntmap_AB(AG, B, stg) %>>% #AB
    qntmap_AB_fix %>>%
    map(map, `*`, X) %>>% #XAB
    map(map_at, 'se', map, `^`, 2) %>>%
    map(map, reduce_add) %>>%
    map(map_at, 'se', sqrt) %>>%
    map2(
      qltmap[qnt$elm$elint[order(qnt$elm$elem)]], 
      function(xab, i) map(xab, `*`, i)
    ) %>>% #XABI
    map2(XAG, map2, `-`) %>>% #XABI - XAG
    map(setNames, c('wt', 'se')) %>>%
    map(function(x) map(x, `*`, x$wt > 0)) %>>%
    c(
      list(Total = list(
        wt = map(., `[[`, 'wt') %>>%
          reduce_add %>>%
          as.data.frame,
        se = map(., `[[`, 'se') %>>%
          map(`^`, 2) %>>%
          reduce_add %>>%
          sqrt %>>%
          as.data.frame
      ))
    ) %>>%
    `class<-`(c('qntmap', 'list')) %>>%
    (~ saveRDS(., 'qntmap.RDS')) %>>%
    (~ . %>>% #side effect saving csv and RDS files
       unlist(recursive = FALSE) %>>%
       walk2(
         paste0(sub('\\.', '_', names(.)), '.csv'), 
         fwrite
       ) %>>%
       NULL
    ) 
}

