#' Compile epma data into a data frame
#'
#' @param qnt object returned by read_qnt
#' @param xmap object returned by read_xmap
#' @param cluster object returned by cluster_xmap
#'
#' @importFrom dplyr distinct left_join mutate mutate_at rename transmute
#' @importFrom matrixStats rowMaxs
#' @importFrom pipeR %>>%
#' @importFrom purrr map2 map_int
#' @importFrom stats setNames
#' @importFrom stringr str_c str_replace_all str_replace str_detect
#'
#' @export
#'
#'
tidy_epma <- function(
  qnt,
  xmap, # NA,
  cluster = NULL
) {

  #load mapping conditions
  cnd <- pipeline({
    xmap
    attr('dir_map')
    paste0('/', c('0', 'map'), '.cnd')
    `[`(file.exists(.))
    `[`(1)
    read_cnd
  })
  pos <- attributes(xmap)[c('start', 'pixel', 'step')]
  beam <- setNames(attributes(xmap)[c('current', 'dwell')], c('beam_map', 'dwell'))
  inst <- attributes(xmap)[['instrument']]

  #データの整形

  #ステージ位置をmmからピクセル位置に変換
  qnt$cnd <- pipeline({
      qnt$cnd
      filter(!is.na(phase))
      mutate(
        x_px = (round((x - pos$start[1]) * 1e3 / pos$step[1]) + 1) *
          `if`(inst %in% 'JXA8230', -1, 1),
        y_px = round((y - pos$start[2]) * 1e3 / pos$step[2]) + 1,
        nr0 = `if`(
          inst %in% 'JXA8230',
          (x_px - 1) * pos$pixel[2] + y_px,
          (y_px - 1) * pos$pixel[1] + x_px
        ),
        nr = ifelse(0 < nr0 & nr0 < prod(pos$pixel), nr0, NA),
        phase2 = str_replace(phase, '_.*', '')
      )
      distinct(nr0, .keep_all = TRUE)
    })
  qnt$elm$dwell <- beam[['dwell']] / 1000
  qnt$elm$beam_map <- beam[['beam_map']]

  ## Error if
  if(all(is.na(qnt$cnd$nr))) stop('No points are quantified in mapping area.')

  ## Let's join
  qnt$cmp <- pipeline({
    xmap[qnt$elm$elint]
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
      cls = `if`(is.null(cluster), NA, names(cluster$ytehat)[qnt$cnd$nr]),
      mem = `if`(is.null(cluster), NA, rowMaxs(cluster$membership[qnt$cnd$nr, ])),
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
      offset = 
        transmute(
          ., 
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
      net.L = net - propagate_add(pkint, pkint.L, bgint, bgint.L),
      net.H = net + propagate_add(pkint, pkint.H, bgint, bgint.H)
    ) %>>%
    as.data.frame
}
