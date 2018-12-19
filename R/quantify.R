#' Quantify X-ray maps
#'
#' @param xmap `qm_xmap` class object returned by [`read_xmap()`].
#' @param qnt `qm_qnt` class object returned by [`read_qnt()`].
#' @param cluster `qm_cluster` class object returned by [`cluster_xmap()`].
#' @param maps_x,maps_y
#'   Sizes of maps along x- and y-axes comprising guide net map.
#'   (default: `NULL`).
#' @inheritParams find_centers
#' @param fine_th A threshold of membership degrees to 0.9
#' A threshold of membership degrees to 0.9
#' @param fixAB 
#' Fix AB in case compositions of a mineral is constant (default: `NULL`).
#' @param fixB Fix B (default: `NULL`).
#' @param saving 
#'   `TRUE` (default) saves the results into `qntmap` directory under 
#'   the directory `xmap` is read from. `FALSE` does not save.`
#'
#' @importFrom purrr map map_at map2
#' @importFrom stats setNames
#'
#' @export
quantify <- function (
  xmap,
  qnt,
  cluster,
  maps_x = attr(xmap, 'pixel')[1],
  maps_y = attr(xmap, 'pixel')[2],
  fine_phase = NULL,
  fine_th = 0.9,
  fixAB = NULL,
  fixB = NULL,
  saving = TRUE
) {

  cd <- getwd(); on.exit(setwd(cd))
  
  # Mapping conditions
  dir_map <- attr(xmap, 'dir_map')
  pixel <- attr(xmap, 'pixel')

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
      elements = qnt$elm$elem,
      distinguished = any(grepl('_', colnames(cluster$membership))),
      fine_phase = fine_phase,
      fine_th = fine_th
    )

  xmap <- xmap[qnt$elm$elint[order(qnt$elm$elem)]]
    
  rm(qnt)

  X <- as.data.frame(cluster$membership)

  rm(cluster)
  
  AG <- find_AG(epma, setdiff(names(X), unique(epma$phase3))) # returns A and G

  B <- find_B(epma)

  rm(epma)

  XAG <- find_XAG(X, mutate(AG, ag = a * g, ag_se = L2(a * g_se, g * a_se), g = NULL, g_se = NULL))

  dir_qntmap <- paste0(dir_map, '/qntmap')
  dir.create(dir_qntmap, FALSE)

  find_AB(AG, B) %>>% #AB
    expand_AB(stg) %>>%
    find_AB_fix(fixAB, X, fine_th, xmap) %>>%
    map(map, `*`, X) %>>% #XAB
    map(map_at, 'se', map, square) %>>%
    map(map, reduce_add) %>>%
    map(map_at, 'se', sqrt) %>>%
    map2(xmap, function(xab, i) map(xab, `*`, i)) %>>%#XABI
    map2(XAG, map2, `-`) %>>% #XABI - XAG
    map(setNames, c('wt', 'se')) %>>%
    map(function(x) map(x, `*`, x$wt > 0)) %>>%
    c(list(Total = list(
      wt = as.data.frame(reduce_add(map(., 'wt'))),
      se = as.data.frame(sqrt(reduce_add(map(map(., 'se'), square))))
    ))) %>>%
    prioritize(.component) %>>%
    `class<-`(c('qntmap', 'list')) %>>%
    save4qm(nm = dir_qntmap, saving = saving)
}
