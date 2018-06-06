#' compile epma data
#'
#' @param wd path to the directory containing .qnt directory
#' @param dir_map path to the directory containing mapping analysis e.g., ./.map/1/
#' @param RDS_cluster path to the clustering result
#' @param qnt object returned by qnt_load
#' @param qltmap object returned by qltmap_load
#' @param cluster object returned by qltmap_cls_pois
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table fread
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr rename
#' @importFrom dplyr transmute
#' @importFrom pipeR %>>%
#' @importFrom purrr map2
#' @importFrom purrr map_int
#' @importFrom stats setNames
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#'
#' @export
#'
#'
epma_tidy <- function(
  wd = NULL,
  dir_map = NULL,
  RDS_cluster = NULL,
  qnt = qnt_load(wd),
  qltmap = if(is.null(dir_map)) NULL else qltmap_load(dir_map),
  cluster = if(is.null(RDS_cluster)) NA else readRDS(RDS_cluster)
) {

  cd <- getwd()
  on.exit(setwd(cd))
  if(!is.null(wd)) setwd(wd) else wd <- cd

  #load mapping conditions
  cnd <- paste0(dir_map, '/0.cnd')
  pos <- read_map_pos(cnd)
  cnd_map <- read_map_beam(cnd)

  #データの整形

  #ステージ位置をmmからピクセル位置に変換
  qnt$cnd <- pipeline({
      qnt$cnd
      filter(!is.na(phase))
      mutate(
        x_px = round((x - pos$start[1]) * 1000 / pos$step[1] + 1),
        y_px = round((y - pos$start[2]) * 1000 / pos$step[2] + 1),
        nr0 = (y_px - 1) * pos$px[1] + x_px,
        nr = ifelse(nr0 > 0 & nr0 < prod(pos$px), nr0, NA),
        phase2 = str_replace(phase, '_.*', '')
      )
      distinct(nr0, .keep_all = TRUE)
    })
      dwell = cnd_map['dwell'] / 1000, #msec -> sec
      beam_map = cnd_map['beam_map']
    )
  


  ##Let's join
  qnt$cmp <- qltmap[qnt$elm$elint] %>>%
    setNames(qnt$elm$elem) %>>%
    lapply(unlist, use.names = FALSE) %>>%
    as.data.table %>>%
    `[`(qnt$cnd$nr, ) %>>%
    list %>>%
    setNames('map') %>>%
    c(map(qnt$cmp, `[`, qnt$cnd$id, )) %>>%
    map(mutate, id =  qnt$cnd$id) %>>%
    bind_rows(.id = 'var') %>>%
    gather(elm, val, -var, -id) %>>%
    spread(var, val)

  ## Define function for propagating errors
  propagate_add <- function(x, x2, y, y2) {
    sqrt((x2 - x) ^ 2 + (y2 - y) ^ 2)
  }
  
  #join cmp, cnd, elem in qnt
  #calculate 95% ci of data
  qnt$cnd %>>%
    mutate(
      cls = if(is.list(cluster)) 
        names(cluster$ytehat)[qnt$cnd$nr] else NA,
      mem = if(is.list(cluster)) 
        apply(cluster$membership[qnt$cnd$nr, ], 1, max) else NA,
      elm = list(qnt$elm)
    ) %>>%
    unnest %>>%
    rename(elm = elem) %>>%
    # left_join(qnt$cmp %>>% select(-pkint), by = c('id', 'elm')) %>>%
    left_join(qnt$cmp, by = c('id', 'elm')) %>>%
    mutate(elint = ifelse(is.na(elint), elm, elint)) %>>%
    mutate_at(c('beam', 'beam_map'), `*`, 1e+6) %>>% #A -> uA
    mutate(mapint = map / dwell / beam_map) %>>%
    mutate(
      bgm2 = bgm * bgm_pos,
      bgp2 = bgp * bgp_pos,
      bgint = bgm2 + bgp2,
      pkint = if(exists('pkint')) {pkint} else {(net + bgint  / (bgm_pos + bgp_pos))},
      pkint = pkint * (pkint > 0)
    ) %>>%
    cipois(
      vars = c('pkint', 'bgm', 'bgp', 'mapint'),
      offset = . %>>%
        transmute(
          pkint = pk_t * beam,
          bgm = bg_t * beam,
          bgp = bgm, # == bg_t * beam
          mapint = dwell * beam_map
        )
    ) %>>% 
    mutate_at(c('beam', 'beam_map'), `/`, 1e+6) %>>% #uA -> A
    mutate(
      bgint.L =
        bgint - propagate_add(bgm2, bgm.L * bgm_pos, bgp2, bgp.L * bgp_pos),
      bgint.H =
        bgint + propagate_add(bgm2, bgm.H * bgm_pos, bgp2, bgp.H * bgp_pos)
    ) %>>%
    mutate_at(c('bgint', 'bgint.L', 'bgint.H'), `/`, .$bgp_pos + .$bgm_pos) %>>%
    mutate(
      net.L =
        net - propagate_add(pkint, pkint.L, bgint, bgint.L),
      net.H =
        net + propagate_add(pkint, pkint.H, bgint, bgint.H)
    )

}

