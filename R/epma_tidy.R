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

  dwell <- dir_map %>>%
    paste0('/0.cnd') %>>%
    readLines %>>%
    `[`(str_detect(., 'Dwell')) %>>%
    str_replace('[:blank:].*$', '') %>>%
    as.numeric %>>%
    `*`(1e-3)



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
    map_at('elm', mutate, dwell = dwell)

  #マッピングデータのうち、定量もした座標のみを選択
  #更に元素名を酸化物に変換
  qltmap <- qltmap %>>%
    `[`(qnt$elm$elint) %>>%
    set_names(qnt$elm$elem) %>>%
    lapply(as.vector) %>>%
    lapply(`[`, qnt$cnd$nr) %>>%
    as.data.table

  #クラスタリング結果の読み込み
  cluster <- cluster %>>%
    `[`(c('ytehat', 'membership')) %>>%
    map_at('ytehat', `[`, qnt$cnd$nr) %>>%
    map_at('membership', function(x) x[qnt$cnd$nr, ])

  info <- info <- c('id', 'phase', 'phase2', 'cls', 'mem', 'x_px', 'y_px')
  qnt %>>%
    map_at(
      'cnd',
      mutate,
      cls = names(cluster$ytehat),
      mem = apply(cluster$membership, 1, max)
    ) %>>%
    map_at(
      'cmp',
      c,
      list(map = qltmap)
    ) %>>%
    map_at('cmp', map, cbind, .$cnd %>>% select(one_of(info))) %>>%
    map_at('cmp', map, gather, elm, val, -one_of(info)) %>>%
    (cmp) %>>%
    map2(names(.), function(x, nm) rename(x, rlang::UQ(nm) := val)) %>>%
    reduce(left_join)

}

