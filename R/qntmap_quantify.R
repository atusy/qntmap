#' quantify qualtitative mapping data
#'
#' @param wd working directory which contains .qnt and .map directories
#' @param dir_map directory containing map data to be quantified
#' @param path_phase aaa
#' @param maps_x aaa
#' @param maps_y aaa
#' @param RDS_cluster path to the output RDS file of clustering. NULL in default look for the newest one in dir_map/clustering
#' @param fine_phase aaa
#' @param fine_th 0.9
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr funs
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr one_of
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom pipeR %>>%
#' @importFrom purrr map
#' @importFrom purrr map_at
#' @importFrom purrr map_dbl
#' @importFrom purrr map_int
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @importFrom purrr set_names
#' @importFrom purrr transpose
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#'
#'@export
qntmap_quantify <- function(
    wd = NULL,
    dir_map = NULL,
    maps_x = NULL,
    maps_y = NULL,
#    path_phase = 'qnt_phase.csv',
    RDS_cluster = NULL,
    fine_phase = NULL,
    fine_th = 0.9
  ) {



  if(FALSE) {

    wd <- '/home/atusy/Univ/!Data_ND/epma/WDX/ND0207_160819/'
    dir_map <- './.map/10'
    RDS_cluster <- './.map/10_modified_Si_CP/clustering/170423_0207_pois_integrated_k10_CaMnMgSiTiFeNaCPKAlCr_result.RDS'
    maps_x <- 250
    maps_y <- 415
    path_phase = 'qnt_phase.csv'
    fine_phase <- c('Pl', 'Spl', 'Hem', 'Amp')
    fine_th <- 0.9

    qntmap_quantify(
      wd = '/home/atusy/Univ/Data_ND/epma/WDX/ND0207_160819',
      dir_map = './.map/10',
      maps_x = 250,
      maps_y = 415,
      path_phase = 'qnt_phase.csv',
      RDS_cluster = './.map/10_modified_Si_CP/clustering/170423_0207_pois_integrated_k10_CaMnMgSiTiFeNaCPKAlCr_result.RDS',
      fine_phase = c('Pl', 'Spl', 'Hem', 'Amp'),
      fine_th = 0.9
    )
  }

  cd <- getwd()
  on.exit(setwd(cd))
  if(is.character(wd)) setwd(wd)

  #マッピング設定の読み込み
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

  #クラスタリング結果の読み込み
  cluster <- RDS_cluster %>>%
  	readRDS %>>%
  	`[`(c('ytehat', 'membership'))

  #ステージ位置をmmからピクセル位置に変換
  #更にマッピング範囲外のステージ位置を除去
  #定量結果の読み込み
  qnt <- wd %>>%
  	qnt_load %>>%
#  	map_at('cnd', inner_join, fread(path_phase)) %>>%
  	map_at(
  		'cnd',
  		mutate,
  		x = round((x - cnd$start[1]) * 1000 / cnd$step[1] + 1),
  		y = round((y - cnd$start[2]) * 1000 / cnd$step[2] + 1),
  		nr = (y - 1) * cnd$px[1] + x,
  		i = seq(1, length(id))
  	) %>>%
  	map_at('cnd', distinct, nr, .keep_all = TRUE) %>>% #重複するnrがあれば後者を削除
  	map_at('cmp', map, `[`, .$cnd$i, ) %>>%
  	map_at(
  	  'cnd',
  	  mutate,
  	  nr = ifelse(nr > 0 & nr <=  length(cluster$ytehat), nr, NA),
  		cls = names(cluster$ytehat)[nr],
  		membership = apply(cluster$membership[nr, ], 1, max)
  	)

  #マッピングデータのうち、定量もした座標のみを選択
  #更に元素名を酸化物に変換
  #マッピング結果の読み込み
  qltmap <- dir_map %>>%
  	qltmap_load %>>%
  	`[`(qnt$elm$elint) %>>%
  	set_names(qnt$elm$elem) %>>%
    lapply(as.vector) %>>%
    as.data.table

  if(is.null(maps_x)) maps_x <- cnd$px[1]
  if(is.null(maps_y)) maps_y <- cnd$px[2]

  qltmap_x <- rep(1:cnd$px[1], cnd$px[2])
  qltmap_y <- map(1:cnd$px[2], rep, cnd$px[1]) %>>%
  	unlist(use.names = FALSE, recursive = FALSE)
  qltmap_stg <- ceiling(qltmap_x / maps_x) * 100 + ceiling(qltmap_y / maps_y)


  rm(qltmap_x, qltmap_y)

  ##
  train <- qnt$cmp %>>%
    map(select, one_of(names(qltmap))) %>>%
  	map(gather, elm, val) %>>%
  	map(select, val) %>>%
  	map(unlist, use.names = FALSE, recursive = FALSE) %>>%
  	c(qltmap %>>%
  		`[`(qnt$cnd$nr, ) %>>%
  		bind_cols(qnt$cnd) %>>%
  		gather(elm, qlt, -one_of(names(qnt$cnd)))
  	) %>>%
  	as.data.table %>>%
  	filter(phase == cls) %>>%
  	mutate(stg = ceiling(x / maps_x) * 100 + ceiling(y / maps_y))

  phases_miss <- unique(names(cluster$ytehat)) %>>%
  	`[`(!(. %in% unique(train$phase)))

  ##
  fit <- function(y, x, w) {
  	list(glm(
  		y ~ 0 + x,
  		family = gaussian(link = identity),
  		weights = w,
  		na.action = na.omit
  	))
  }

  Qlt2Qnt <- train %>>%
  	filter(!(phase %in% fine_phase), membership > fine_th) %>>%
  	select(stg, elm, pkint, qlt, membership) %>>%
  	group_by(stg, elm) %>>%
  	summarise(fit = fit(pkint, qlt, membership)) %>>%
  	ungroup %>>%
  	spread(elm, fit)

  extract <- qltmap_stg %>>%
  	map(`==`, Qlt2Qnt$stg) %>>%
  	map_int(which)

  Qnt <- Qlt2Qnt %>>%
  	select(one_of(names(qltmap))) %>>%
  	map(map_dbl, coef) %>>%
  	map(`[`, extract) %>>%
  	map2(qltmap, `*`) %>>%
  	as.data.table

  Qnt_se <- Qlt2Qnt %>>%
  	select(one_of(names(qltmap))) %>>%
  	map(map_dbl, vcov) %>>%
  	map(sqrt) %>>%
  	map(`[`, extract) %>>%
  	map2(qltmap, `*`) %>>%
  	as.data.table


  rm(Qlt2Qnt, qltmap)

  Bg <- train %>>%
  	mutate(bg = bg * 1e-6 / beam) %>>%
  	select(bg, phase, elm) %>>%
  	group_by(phase, elm) %>>%
  	summarise(mean = mean(bg), sd = sd(bg), length = length(bg)) %>>%
  	ungroup %>>%
  	mutate(se = sd / length) %>>%
    (if(length(phases_miss) > 0) {
      . %>>%
        bind_rows %>>%
        (train %>>%
        	mutate(bg = bg * 1e-6 / beam) %>>%
        	select(bg, elm) %>>%
        	group_by(elm) %>>%
          summarise(mean = mean(bg), sd = sd(bg), length = length(bg)) %>>%
          ungroup %>>%
        	mutate(se = sd / sqrt(length))
        )
    } else {
      .
    })

  Bg_mean <- Bg %>>%
  	select(phase, elm, mean) %>>%
  	spread(elm, mean)

  Bg_se <- Bg %>>%
  	select(phase, elm, se) %>>%
  	spread(elm, se)

  rm(Bg)

  extract <- map(names(cluster$ytehat), `==`, Bg_mean$phase) %>>% map_int(which)

  Net <- Bg_mean[extract, ] %>>%
  	select(one_of(names(Qnt))) %>>%
  	map(`*`, -1) %>>%
  	map2(Qnt, `+`) %>>%
  	map(~ ifelse(.x < 0, 0, .x)) %>>%
  	as.data.table

  Net_se <- Bg_se[extract, ] %>>%
  	select(one_of(names(Qnt))) %>>%
  	map(`^`, 2) %>>%
  	map2(Qnt_se %>>% map(`^`, 2), `+`) %>>%
  	map(sqrt) %>>%
  	as.data.table

  rm(Bg_se, Bg_mean, Qnt, Qnt_se)

  fit <- fit <- function(y, x) {
  	list(glm(
  		y ~ 0 + x,
  		family = gaussian(link = identity),
  		na.action = na.omit
  	))
  }

  Net2Wt <- train %>>%
  	filter(net >= 0) %>>%
  	group_by(phase, elm) %>>%
  	summarise(fit = fit(wt, net)) %>>%
  	ungroup

  if(length(phases_miss > 0)) {
    Net2Wt_all <- train %>>%
    	filter(net >= 0) %>>%
    	group_by(elm) %>>%
    	summarise(fit = fit(wt, net)) %>>%
    	ungroup

    Net2Wt <- phases_miss %>>%
    	map(~ mutate(Net2Wt_all, phase = .x)) %>>%
    	bind_rows %>>%
    	select(one_of(names(Net2Wt))) %>>%
    	bind_rows(Net2Wt)

    rm(Net2Wt_all)
  }

  Net2Wt_coef <- Net2Wt %>>%
  	mutate(fit = fit %>>% map_dbl(coef)) %>>%
  	spread(elm, fit) %>>%
  	select(one_of(names(Net))) %>>%
  	transpose

  Net2Wt_se <- Net2Wt %>>%
  	mutate(fit = fit %>>% map_dbl(vcov) %>>% sqrt) %>>%
  	spread(elm, fit) %>>%
  	select(one_of(names(Net))) %>>%
  	transpose


  rm(Net2Wt)

  membership <- cluster$membership %>>%
  	as.data.table %>>%
  	select(one_of(. %>>% names %>>% sort))

  Wt <- Net2Wt_coef %>>%
  	map(map2, Net, `*`) %>>%
  	map(as.data.table) %>>%
  	map2(membership, `*`) %>>%
  	map(as.data.frame) %>>%
  	reduce(`+`) %>>%
  	as.data.table %>>%
  	set_names(names(Net))

  Wt_se <- map2(
  		map(Net2Wt_coef, map2, Net_se, `*`) %>>%
  			map(as.data.table) %>>%
  			map(`^`, 2),
  		map(Net2Wt_se, map2, Net, `*`) %>>%
  			map(as.data.table) %>>%
  			map(`^`, 2),
  		`+`
  	) %>>%
  	map(sqrt) %>>%
  	map2(membership, `*`) %>>%
  	map(`^`, 2) %>>%
  	reduce(`+`) %>>%
  	sqrt %>>%
  	as.data.table %>>%
  	set_names(names(Net))

  rm(Net2Wt_se, Net2Wt_coef)

  # if(P4Apt && any(names(membership) == 'Apt')) {
  		# Wt$P2O5 <- 283.89 * 3 / 20 * (Wt$CaO / 56.0774) * membership$Apt
  		# Wt_se$P2O5 <- 283.89 * 3 / 20 * (Wt_se$CaO / 56.0774) * membership$Apt
  	# }


  Wt$Total <- reduce(Wt, `+`)
  Wt_se$Total <- Wt_se %>>%
  	map(`^`, 2) %>>%
  	reduce(`+`) %>>%
  	sqrt

  qntmap <- list(wt = Wt, se = Wt_se) %>>%
  	map(map, matrix, nrow = cnd$px[1])
  rm(Wt, Wt_se)

  saveRDS(qntmap, 'qntmap.RDS')


  # qntmap$se %>>%
  # 	map(as.vector) %>>%
  # 	as.data.table %>>%
  # 	mutate(phase = names(cluster$ytehat)) %>>%
  # 	gather(elm, val, -phase) %>>%
  # 	ggplot(aes(x = val, color = phase)) %>>%
  # 	`+`(list(
  # 		geom_density(),
  # 		facet_wrap(~ elm, scale = 'free')
  # 	))
  #
  #
  # qntmap$se %>>%
  # 	map(as.vector) %>>%
  # 	as.data.table %>>%
  # 	mutate(phase = names(cluster$ytehat)) %>>%
  # 	group_by(phase) %>>%
  # 	summarise_all(mean) %>>%
  # 	mutate_if(is.numeric, round, digits = 2)
  #
  # prof <- qntmap %>>%
  # 	map_at('wt', map, colMeans) %>>%
  # 	map_at('se', map, `^`, 2) %>>%
  # #	map_at('se', map, function(x) sqrt(colSums(x))) %>>%
  # 	map_at('se', map, function(x) sqrt(colSums(x) / nrow(x))) %>>%
  # 	map(as.data.table) %>>%
  # 	map_at('wt', ~ mutate(.x, x = 1:nrow(.x))) %>>%
  # 	map_at('wt', gather, elm, wt, -x) %>>%
  # 	map_at('se', gather, elm2, se) %>>%
  # 	bind_cols %>>%
  # 	select(-elm2) %>>%
  # 	as.data.table %>>%
  # 	mutate(upr = wt + 3 * se, lwr = wt - 3 * se) %>>%
  # 	filter(!(((x %% 415) == 0) & (elm %in% c('SiO2', 'Total'))))
  #
  # ggplot(prof2, aes(x = x, y = wt)) +
  # #	geom_ribbon(aes(ymin = lwr, ymax = upr), color = 'black', alpha = 0.5) +
  # 	geom_line(aes(color = elm)) +
  # 	facet_wrap(~ elm, scale = 'free')
  #
  # prof$Cr2O3[1:414] %>>% plot
  # prof$Total[300:414] %>>% mean

}
