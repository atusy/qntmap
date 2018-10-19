#' quantify qualtitative mapping data
#'
#' @param xmap `qm_xmap` class object returned by `read_xmap()`.
#' @param qnt `qm_qnt` class object returned by `read_qnt()`.
#' @param cluster `qm_cluster` class object returned by `cluter_xmap()`.
#' @param maps_x A x-axis size of maps comprising guide net map (default: `NULL`).
#' @param maps_y A y-axis size of maps comprising guide net map (default: `NULL`).
#' @inheritParams find_centers
#' @param fine_th 0.9
#' @param fixAB fix AB in case compositions of a mineral is constant (default: `NULL`).
#' @param fixB fix B (default: `NULL`).
#'
#' @importFrom data.table fwrite
#' @importFrom pipeR pipeline
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map2
#' @importFrom purrr walk2
#' @importFrom stats setNames
#' @importFrom stringr str_replace
#'
#' @export
quantify <- function(
  xmap,
  qnt,
  cluster,
  maps_x = NULL,
  maps_y = NULL,
  fine_phase = NULL,
  fine_th = 0.9,
  fixAB = NULL,
  fixB = NULL
) {

  cd <- getwd(); on.exit(setwd(cd))
  
  #mapping conditions
  dir_map <- attr(xmap, 'dir_map')
  pos <- read_map_pos(dir(dir_map, pattern = '^(map|0)\\.cnd$', full.names = TRUE))

  if(is.null(maps_x)) maps_x <- pos$px[1]
  if(is.null(maps_y)) maps_y <- pos$px[2]

  stg <- do.call(
    flag0,
    unclass(expand.grid(
      x_stg = seq(0, pos$px[1] - 1) %/% maps_x + 1,
      y_stg = seq(0, pos$px[2] - 1) %/% maps_y + 1
    ))
  )

  #tidy compilation of epma data
  distinguished <- any(grepl('_', colnames(cluster$membership)))
  epma <- pipeline({
    tidy_epma(qnt = qnt, xmap = xmap, cluster = cluster) 
      filter(elm %in% qnt$elm$elem) 
      mutate(
        net = net * (net > 0),
        phase3 = if(distinguished) phase else phase2,
        x_stg = ((x_px - 1) %/% maps_x + 1) * (0 < x_px) * (x_px <= pos$px[1]), 
        y_stg = ((y_px - 1) %/% maps_y + 1) * (0 < y_px) * (y_px <= pos$px[2]),
        stg = ifelse((x_stg * y_stg) <= 0, NA, flag0(x_stg, y_stg)),
        mem = mem * 
          (str_replace(cls, '_.*', '') == phase2) *
          (cls %nin% fine_phase) * 
          (mem > fine_th) 
      )
  })

  xmap <- xmap[qnt$elm$elint[order(qnt$elm$elem)]]
    
  rm(qnt)

  X <- as.data.frame(cluster$membership)

  rm(cluster)
  
  AG <- find_AG(epma, setdiff(names(X), unique(epma$phase3))) # return also A

  B <- find_B(epma)

  rm(epma)

  XAG <- find_XAG(X, AG)

  dir_qntmap <- paste0(dir_map, '/qntmap')
  dir.create(dir_qntmap, FALSE)

  pipeline({
    find_AB(AG, B, stg) #AB
      find_AB_fix(fixAB, X, fine_th, xmap)
      map(map, `*`, X) #XAB
      map(map_at, 'se', map, square) 
      map(map, reduce_add) 
      map(map_at, 'se', sqrt) 
      map2(xmap, function(xab, i) map(xab, `*`, i)) #XABI
      map2(XAG, map2, `-`) #XABI - XAG
      map(setNames, c('wt', 'se')) 
      map(function(x) map(x, `*`, x$wt > 0)) 
      c(
        list(Total = list(
          wt = pipeline({
            map(., `[[`, 'wt') 
            reduce_add 
            as.data.frame
          }),
          se = pipeline({
            map(., `[[`, 'se') 
            map(square) 
            reduce_add 
            sqrt 
            as.data.frame
          })
        ))
      ) 
      `class<-`(c('qntmap', 'list')) 
      save4qm(dir_qntmap = dir_qntmap)
  })
}

#' (Deprecated) Use quantify.
#' @param wd working directory which contains .qnt and .map directories
#' @param dir_map ignored
#' @param RDS_cluster path to the RDS file created by cluster analysis
#' @param qltmap `qm_xmap` class object returned by `read_xmap()`.
#' @inheritParams quantify
#' @export
qntmap_quantify <- function(
  wd = '.',
  dir_map,
  RDS_cluster,
  maps_x = NULL,
  maps_y = NULL,
  fine_phase = NULL,
  fine_th = 0.9,
  qnt = qnt_load(wd),
  qltmap = qltmap_load(dir_map),
  cluster = readRDS(RDS_cluster)
) {
  .Deprecated(new = 'quantify')
  cd <- getwd(); on.exit(setwd(cd))
  quantify(xmap = qltmap, qnt = qnt, cluster = cluster, maps_x = maps_x, maps_y = maps_y, fine_phase = fine_phase, fine_th = fine_th)
}
