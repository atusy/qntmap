#' Quantify X-ray maps
#'
#' @param xmap `qm_xmap` class object returned by [`read_xmap()`].
#' @param qnt `qm_qnt` class object returned by [`read_qnt()`].
#' @param cluster `qm_cluster` class object returned by [`cluster_xmap()`].
#' @param maps_x,maps_y
#' Sizes of maps along x- and y-axes comprising guide net map.
#' (default: `NULL`).
#' @inheritParams find_centers
#' @param fine_th 0.9
#' @param fixAB 
#' fix AB in case compositions of a mineral is constant (default: `NULL`).
#' @param fixB fix B (default: `NULL`).
#'
#' @importFrom data.table fwrite
#' @importFrom pipeR pipeline
#' @importFrom purrr map map_at map2 walk2
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
  
  # Mapping conditions
  dir_map <- attr(xmap, 'dir_map')
  pixel <- attr(xmap, 'pixel')

  if(is.null(maps_x)) maps_x <- pixel[1]
  if(is.null(maps_y)) maps_y <- pixel[2]

  stg <- do.call(
    flag0,
    unclass(expand.grid(
      x_stg = seq(0, pixel[1] - 1) %/% maps_x + 1,
      y_stg = seq(0, pixel[2] - 1) %/% maps_y + 1
    ))
  )

  # Tidy compilation of epma data
  epma <- tidy_epma_for_quantify(
      tidy_epma(qnt = qnt, xmap = xmap, cluster = cluster),
      maps_x, maps_y, 
      distinguished = any(grepl('_', colnames(cluster$membership))),
      elements = qnt$elm$elem
    )

  xmap <- xmap[qnt$elm$elint[order(qnt$elm$elem)]]
    
  rm(qnt)

  X <- as.data.frame(cluster$membership)

  rm(cluster)
  
  AG <- mutate(
    find_AG(epma, setdiff(names(X), unique(epma$phase3))), # returns A and G
    ag = a * g, ag_se = L2(a * g_se, g * a_se), g = NULL, g_se = NULL # returns A and A * G
  )

  B <- nest(find_B(epma), -stg, .key = ".B")

  rm(epma)

  XAG <- find_XAG(X, AG)

  dir_qntmap <- paste0(dir_map, '/qntmap')
  dir.create(dir_qntmap, FALSE)

  pipeline({
    find_AB(AG, B) #AB
      expand_AB(stg)
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
      prioritize(.component)
      `class<-`(c('qntmap', 'list')) 
      save4qm(dir_qntmap = dir_qntmap)
  })
}

#' (DEPRECATED) Use quantify.
#' @param wd working directory which contains .qnt and .map directories
#' @param dir_map ignored
#' @param RDS_cluster path to the RDS file created by cluster_xmap
#' @param qltmap qm_xmap class object (xmap param of quantify)
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
