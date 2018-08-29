#' compile epma data
#'
#' @param wd path to the directory containing .qnt directory
#' @param dir_map path to the directory containing mapping analysis e.g., ./.map/1/
#' @param RDS_cluster path to the clustering result
#' @param qnt object returned by qnt_load
#' @param qltmap object returned by qltmap_load
#' @param cluster object returned by qltmap_cls_pois
#'
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
  dir_map = NULL,
  RDS_cluster = NULL,
  qnt = read_qnt(wd),
  qltmap = if(is.null(dir_map)) NULL else read_xmap(dir_map),
  cluster = if(is.null(RDS_cluster)) NA else readRDS(RDS_cluster)
) {

  #load mapping conditions
  cnd <- pipeline({
    dir_map
    paste0('/', c('0', 'map'), '.cnd')
    `[`(file.exists(.))
    `[`(1)
    read_cnd
  })
  pos <- read_map_pos(cnd)
  beam <- read_map_beam(cnd)

  #データの整形

  #ステージ位置をmmからピクセル位置に変換
  qnt$cnd <- pipeline({
      qnt$cnd
      filter(!is.na(phase))
      mutate(
        x_px = (round((x - pos$start[1]) * 1e3 / pos$step[1]) + 1) *
          `if`(class(cnd)[1] == 'map_cnd', -1, 1),
        y_px = round((y - pos$start[2]) * 1e3 / pos$step[2]) + 1,
        nr0 = `if`(
          class(cnd)[1] == 'map_cnd',
          (x_px - 1) * pos$px[2] + y_px,
          (y_px - 1) * pos$px[1] + x_px
        ),
        nr = ifelse(0 < nr0 & nr0 < prod(pos$px), nr0, NA),
        phase2 = str_replace(phase, '_.*', '')
      )
      distinct(nr0, .keep_all = TRUE)
    })
  qnt$elm$dwell <- beam['dwell'] / 1000
  qnt$elm$beam_map <- beam['beam_map']



  ##Let's join
  qnt$cmp <- pipeline({
    qltmap[qnt$elm$elint]
    setNames(qnt$elm$elem)
    lapply(unlist, use.names = FALSE)
    as.data.frame
    `[`(qnt$cnd$nr, )
    list
    setNames('map')
    c(map(qnt$cmp, `[`, qnt$cnd$id, ))
    map(mutate, id = qnt$cnd$id)
    bind_rows(.id = 'var')
    gather(elm, val, -var, -id)
    spread(var, val)
  })
  #join cmp, cnd, elem in qnt
  #calculate 95% ci of data
  qnt$cnd %>>%
    mutate(
      cls = `if`(is.na(cluster), NA, names(cluster$ytehat)[qnt$cnd$nr]),
      mem = `if`(is.na(cluster), NA, apply(cluster$membership[qnt$cnd$nr, ], 1, max)),
      elm = list(qnt$elm)
    ) %>>%
    unnest %>>%
    rename(elm = elem) %>>%
    left_join(qnt$cmp, by = c('id', 'elm')) %>>%
    mutate(
      elint = ifelse(is.na(elint), elm, elint),
      beam = beam * 1e+6,
      beam_map = beam_map * 1e+6,
      mapint = map / dwell / beam_map,
      bgm2 = bgm * bgm_pos,
      bgp2 = bgp * bgp_pos,
      bgint = bgm2 + bgp2,
      pkint = `if`(exists('pkint'), pkint, net + bgint  / (bgm_pos + bgp_pos)),
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
    mutate(
      beam = beam * 1e-6,
      beam_map = beam_map * 1e-6,
      .tmp = bgm.L * bgm_pos,
      bgint.L = bgint - propagate_add(bgm2, .tmp, bgp2, .tmp),
      .tmp = bgm.H * bgm_pos,
      bgint.H = bgint + propagate_add(bgm2, .tmp, bgp2, .tmp),
      .tmp = bgp_pos + bgm_pos,
      bgint = bgint / .tmp,
      bgint.L = bgint.L / .tmp,
      bgint.H = bgint.H / .tmp,
      .tmp = NULL,
      net.L =
        net - propagate_add(pkint, pkint.L, bgint, bgint.L),
      net.H =
        net + propagate_add(pkint, pkint.H, bgint, bgint.H)
    )

}

