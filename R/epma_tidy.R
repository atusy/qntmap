#' compile epma data
#'
#' @param wd path to the directory containing .qnt directory
#' @param dir_map path to the directory containing mapping analysis e.g., ./.map/1/
#' @param phase_list path to the csv file containing columns indicating phase of each analysis and true or false to use it for quantifying.
#' @param RDS_cluster path to the clustering result
#' @param qnt object of class qnt
#' @param qltmap object of class qltmap
#' @param cluster object of class PoiClaClu
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom pipeR %>>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_at
#' @importFrom purrr reduce
#' @importFrom rlang set_names
#' @importFrom rlang UQ
#' @importFrom stringr str_c
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table fread
#'
#' @export
#'
#'
epma_tidy <- function(
  wd = NULL,
  dir_map,
  phase_list = NULL,
  RDS_cluster,
  qnt = qnt_load(wd, phase_list = phase_list),
  qltmap = qltmap_load(dir_map),
  cluster = readRDS(RDS_cluster)
) {

  library(qntmap)
  wd = '/home/atusy/Univ/Data_ND/epma/WDX/ND0207_160819'
  dir_map = '.map/10'
  phase_list = NULL
  RDS_cluster = '/home/atusy/Univ/Data_ND/epma/WDX/ND0207_160819/.map/10_modified_Si_CP/clustering/170423_0207_pois_integrated_k10_CaMnMgSiTiFeNaCPKAlCr_result.RDS'
  qnt = qnt_load(wd, phase_list = phase_list)
  qltmap = qltmap_load(dir_map)
  cluster = readRDS(RDS_cluster)

  cd <- getwd()
  on.exit(setwd(cd))
  if(!is.null(wd)) setwd(wd) else wd <- cd

  #load mapping conditions
  pos <- dir_map %>>%
    paste0('/0.cnd') %>>%
    readLines %>>%
    '['(str_detect(.,
                   '(Measurement Start Position X)|(Measurement Start Position Y)|(X-axis Step Number)|(Y-axis Step Number)|(X-axis Step Size)|(Y-axis Step Size)|(X Step Size)|(Y Step Size)'
    )) %>>%
    str_replace_all('[:blank:].*', '') %>>%
    as.numeric %>>%
    matrix(ncol=3,nrow=2,dimnames=list(NULL,c('start','px','step'))) %>>%
    as.data.table

  cnd_detect <- c(
    dwell = 'Dwell Time \\[msec\\]',
    beam_map = 'Probe Current \\[A\\]'
  )
  cnd_map <- dir_map %>>%
    paste0('/0.cnd') %>>%
    readLines %>>%
    `[`(
      map(cnd_detect, function(.p) str_detect(., .p)) %>>% map_int(which)
    ) %>>%
    str_replace('[:blank:].*$', '') %>>%
    as.numeric %>>%
    setNames(names(cnd_detect))



  #データの整形

  #ステージ位置をmmからピクセル位置に変換
  qnt <- qnt %>>%
    map_at(
      'cnd',
      mutate,
      x_px = round((x - pos$start[1]) * 1000 / pos$step[1] + 1),
      y_px = round((y - pos$start[2]) * 1000 / pos$step[2] + 1),
      nr0 = (y_px - 1) * pos$px[1] + x_px,
      nr = ifelse(nr0 > 0 & nr0 < prod(pos$px), nr0, NA)
    ) %>>%
    map_at('cnd', distinct, nr0, .keep_all = TRUE) %>>% #重複するnrがあれば後者を削除
    map_at('cnd', mutate, phase2 = str_replace(phase, '_.*', '')) %>>%
    map_at('cnd', filter, !is.na(phase)) %>>%
    map_at('cmp', map, `[`, .$cnd$id, ) %>>%
    map_at(
      'elm',
      mutate,
      dwell = cnd_map['dwell'] / 1000, #msec -> sec
      beam_map = cnd_map['beam_map']
    )

  #マッピングデータのうち、定量もした座標のみを選択
  #更に元素名を酸化物に変換
  qltmap <- qltmap %>>%
    `[`(qnt$elm$elint) %>>%
    set_names(qnt$elm$elem) %>>%
    lapply(unlist, use.names = FALSE) %>>%
    lapply(`[`, qnt$cnd$nr) %>>%
    as.data.table

  #Load clustering result
  cluster <- cluster %>>%
    `[`(c('ytehat', 'membership')) %>>%
    map_at('ytehat', `[`, qnt$cnd$nr) %>>%
    map_at('membership', function(x) x[qnt$cnd$nr, ])

  #join cmp, cnd, elem in qnt
  #calculate 95% ci of data

  ## Define function for propagating errors
  propagate_add <- function(x, x2, y, y2)
    sqrt((x2 - x) ^ 2 + (y2 - y) ^ 2)

  ##Let's join
  qnt_cnd <- c(names(qnt$cnd), 'cls', 'mem', names(qnt$elm))
  qnt %>>%
    map_at('cmp', c, list(map = qltmap)) %>>%
    map_at('cmp', map, mutate, id = .$cnd$id) %>>%
    map_at('cmp', map, gather, elm, val, -id) %>>%
    map_at('cmp', map2, names(.$cmp), function(x, nm) rename(x, rlang::UQ(nm) := val)) %>>%
    map_at('cmp', reduce, left_join, by = c('id', 'elm')) %>>%
    map_at('cnd', as_tibble) %>>%
    map_at(
      'cnd',
      mutate,
      cls = names(cluster$ytehat),
      mem = apply(cluster$membership, 1, max),
      elm = list(.$elm)
    ) %>>%
    map_at('cnd', unnest) %>>%
    map_at('cnd', rename, elm = elem) %>>%
    `[`(c('cmp', 'cnd')) %>>%
    reduce(left_join, by = c('id', 'elm')) %>>%
    mutate_at(c('beam', 'beam_map'), `*`, 1e+6) %>>% #A -> uA
    mutate(mapint = map / dwell / beam_map) %>>%
    cipois(
      vars = c('pkint', 'bgm', 'bgp', 'mapint'),
      offset = . %>>%
        transmute(
          pkint = pk_t * beam,
          bgm = bg_t * beam,
          bgp = bgm,
          mapint = dwell * beam_map
        )
    ) %>>%
    mutate_at(c('beam', 'beam_map'), `/`, 1e+6) %>>% #uA -> A
    mutate(
      bgm2 = bgm * bgm_pos,
      bgp2 = bgp * bgp_pos,
      bgint = bgm2 + bgp2,
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
    ) %>>%
    return

}

