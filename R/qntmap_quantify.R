#' quantify qualtitative mapping data
#'
#' @param wd working directory which contains .qnt and .map directories
#' @param dir_map directory containing map data to be quantified
#' @param phase_list aaa
#' @param maps_x aaa
#' @param maps_y aaa
#' @param RDS_cluster path to the output RDS file of clustering. NULL in default look for the newest one in dir_map/clustering
#' @param fine_phase aaa
#' @param fine_th 0.9
#' @param qnt object of class qnt
#' @param qltmap object of class qltmap
#' @param cluster object of class PoiClaClu
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @importFrom tibble tibble
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr right_join
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom pipeR %>>%
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map_dbl
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @importFrom rlang set_names
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#'
#'@export
qntmap_quantify <- function(
    wd = NULL,
    phase_list = NULL,
    dir_map,
    RDS_cluster,
    maps_x = NULL,
    maps_y = NULL,
    fine_phase = NULL,
    fine_th = 0.9,
    qnt = qnt_load(wd, phase_list = phase_list),
    qltmap = qltmap_load(dir_map),
    cluster = readRDS(RDS_cluster)
) {



  if(FALSE) {

    wd <- "/home/atusy/Univ/!Data_ND/epma/WDX/JGb1a_170912"
    phase_list = NULL
    dir_map <- '/home/atusy/Univ/!Data_ND/epma/WDX/JGb1a_170912/.map/1'
    RDS_cluster <- '/home/atusy/Univ/!Data_ND/epma/WDX/JGb1a_170912/.map/1/clustering/170916_0835_pois_integrated_k12_TiMgCaSiMnFeNaCPKAlCr_result.RDS'
    maps_x <- NULL
    maps_y <- NULL
    maps_x <- 250
    maps_y <- 250
    fine_phase = c('Bt', 'Chl', 'Hem', 'Ilm', 'Py')
    fine_th <- 0.9
    qnt = qnt_load(wd, phase_list = phase_list)
    qltmap = qltmap_load(dir_map)
    cluster = readRDS(RDS_cluster)

    qntmap_quantify(
      wd = '/home/atusy/Univ/Data_ND/epma/WDX/ND0207_160819',
      dir_map = './.map/10',
      maps_x = 250,
      maps_y = 415,
      RDS_cluster = './.map/10_modified_Si_CP/clustering/170423_0207_pois_integrated_k10_CaMnMgSiTiFeNaCPKAlCr_result.RDS',
      fine_phase = c('Pl', 'Spl', 'Hem', 'Amp'),
      fine_th = 0.9
    )
  }

  cd <- getwd()
  on.exit(setwd(cd))
  if(is.character(wd)) setwd(wd)

  #mapping conditions
  cnd <- dir_map %>>%
    str_replace('/?$', '/') %>>%
    str_c('0.cnd') %>>%
  	readLines %>>%
  	'['(str_detect(.,
  		'(Measurement Start Position X)|(Measurement Start Position Y)|(X-axis Step Number)|(Y-axis Step Number)|(X-axis Step Size)|(Y-axis Step Size)|(X Step Size)|(Y Step Size)'
  		)) %>>%
  	str_replace_all('[:blank:].*', '') %>>%
  	as.numeric %>>%
  	matrix(ncol=3, nrow=2, dimnames = list(NULL, c('start', 'px', 'step'))) %>>%
  	as.data.table

  stg <- expand.grid(
      y = 1:cnd$px[2] - 1,
      x = 1:cnd$px[1] - 1
    ) %>>%
    mutate(x_stg = if(is.null(maps_x)) 1 else x %/% maps_x + 1) %>>%
    mutate(y_stg = if(is.null(maps_y)) 1 else y %/% maps_y + 1) %>>%
    mutate(stg = paste0(
      formatC(x_stg, width = x_stg %>>% log10 %>>% max %>>% floor %>>% `+`(1), flag = '0'),
      formatC(y_stg, width = y_stg %>>% log10 %>>% max %>>% floor %>>% `+`(1), flag = '0')
    ))


  #tidy compilation of epma data
  epma <- epma_tidy(wd = wd, dir_map = dir_map, qnt = qnt, qltmap = qltmap, cluster = cluster) %>>%
    mutate(net = ifelse(net < 0, 0, net)) %>>%
    mutate(mem = ifelse(cls != phase2 | cls %in% fine_phase | mem < fine_th, 0, mem)) %>>%
    mutate(x_stg = if(is.null(maps_x)) 1 else (x_px - 1) %/% maps_x + 1) %>>%
    mutate(x_stg = ifelse(x_stg <= 0 | x_stg > max(stg$x_stg), NA, x_stg)) %>>%
    mutate(y_stg = if(is.null(maps_y)) 1 else (y_px - 1) %/% maps_y + 1) %>>%
    mutate(y_stg = ifelse(y_stg <= 0 | y_stg > max(stg$y_stg), NA, y_stg)) %>>%
    mutate(
      stg =
        paste0(
          formatC(x_stg, width = x_stg %>>% log10 %>>% max(na.rm = TRUE) %>>% floor %>>% `+`(1), flag = '0'),
          formatC(y_stg, width = y_stg %>>% log10 %>>% max(na.rm = TRUE) %>>% floor %>>% `+`(1), flag = '0')
        )
    ) %>>%
    mutate(stg = ifelse(str_detect(stg, 'NA'), NA, stg)) %>>%
    mutate(mem = ifelse(is.na(stg), 0, mem))

  rm(qnt)

  #qltmap: elements -> oxides
  qltmap <- qltmap %>>%
  	`[`(qnt$elm$elint) %>>%
  	set_names(qnt$elm$elem) %>>%
    `[`(sort(names(.)))

  rm(qnt)

  X <- cluster %>>%
    (membership) %>>%
    as.data.table

  rm(cluster)
  ##
  AG <- epma %>>%
    group_by(elm) %>>%
    mutate(fit_na = lm(wt ~ 0 + net) %>>% list) %>>%
    group_by(phase2, elm) %>>%
    summarise(
      fit = lm(wt ~ 0 + net) %>>% list,
      fit_na = fit_na[1],
      g = mean(bgint),
      g_se = sd(bgint) / (length(bgint) - 1)
    ) %>>%
    ungroup %>>%
    mutate(
      a = map_dbl(fit, coef),
      a_se = ifelse(is.na(a), map_dbl(fit_na, vcov), map_dbl(fit, vcov)),
      a = ifelse(is.na(a), map_dbl(fit_na, coef), a),
      ag = a * g,
      ag_se = sqrt((a * g_se) ^ 2 + (g * a_se) ^ 2)
    ) %>>%
    select(-fit, -fit_na, -g, -g_se)

  B <- epma %>>%
    filter(!is.na(stg)) %>>%
    group_by(elm) %>>%
    mutate(fit_na = lm(pkint ~ 0 + map, weights = mem) %>>% list) %>>%
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

  rm(epma)

  XAG <- AG %>>%
    select(phase2, elm, ag, ag_se) %>>%
    nest(-elm) %>>%
    (.x ~ set_names(.x$data, .x$elm)) %>>%
    map(function(x) map(x %>>% select(-phase2), set_names, x$phase2)) %>>%
    map(map, `*`, t(X)) %>>%
    map(set_names, c('val', 'se')) %>>%
    map(map_at, 'se', `^`, 2) %>>%
    map(map, colSums) %>>%
    map(map_at, 'se', sqrt)


  qntmap <- AG %>>% #AB
    select(phase2, elm, a, a_se) %>>%
    nest(-phase2, .key = '.A') %>>%
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
        map(spread, phase2, val),
      se = data %>>%
        map(select, -val) %>>%
        map(spread, phase2, se)
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
    list(Total = list(
      wt = . %>>%
        map(`[[`, 'wt') %>>%
        reduce(`+`),
      se = . %>>%
        map(`[[`, 'se') %>>%
        map(`^`, 2) %>>%
        reduce(`+`) %>>%
        sqrt
    ))

  rm(qltmap, XAG)

  saveRDS(qntmap, 'qntmap.RDS')
}
