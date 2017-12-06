#' Generate initial centroids for clustering
#'
#' @param cnd_qltmap path to the condition file of mapping data
#' @param qnt path to the .RDS file which compiles quantitative data
#' @param qltmap path to the .RDS file which compiles mapping data
#' @param wd working directory which contains mapping data
#' @param saving file name to save. FALSE if not saving.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @importFrom data.table fwrite
#' @importFrom data.table setcolorder
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_all
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr distinct
#' @importFrom pipeR %>>%
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#'
#' @export
qltmap_cls_centers <- function(qnt = NULL, qltmap = NULL, wd = NULL, dir_map = NULL, cnd_qltmap = paste0(dir_map, '/0.cnd'), saving = 'centers_initial0.csv') {

	cd <- getwd()
	on.exit(setwd(cd))
	if(!is.null(wd)) setwd(wd)

	#qnt: import quantitative data
	if(is.null(qnt)) qnt <- qnt_load('../../')
  if(is.character(qnt)) qnt <- readRDS(qnt)
  if(!all(class(qnt) == c('list', 'qnt'))) stop('illegal input of qnt')

	#qltmap: import mapping data
	if(is.null(qltmap)) qnt <- qltmap_load(dir_map)
	if(is.character(qltmap)) qltmap <- readRDS(qltmap)
	if(!all(class(qltmap) == c('list', 'qltmap'))) stop('illegal input of qltmap')

	#cnd: import analysis conditions from 0.cnd
	cnd <- dir_map %>>%
	  paste(cnd_qltmap, sep = '/') %>>%
		readLines %>>%
		'['(str_detect(.,
			'(Measurement Start Position X)|(Measurement Start Position Y)|(X-axis Step Number)|(Y-axis Step Number)|(X-axis Step Size)|(Y-axis Step Size)|(X Step Size)|(Y Step Size)'
			)) %>>%
		str_replace_all('[:blank:].*', '') %>>%
		as.numeric %>>%
		matrix(ncol = 3, nrow = 2, dimnames = list(NULL, c('start', 'px', 'step'))) %>>%
		as.data.table

	#qnt =================================================================
	#qnt: convert stage coordinates to map coordinates
	#qnt: and then remove points with coordinates out of maps
	qnt$cnd <- qnt$cnd %>>%
	#	mutate(x.stage = x, y.stage = y) %>>%
		mutate(
			x = round((x - cnd$start[1]) * 1000 / cnd$step[1] + 1),
			y = round((y - cnd$start[2]) * 1000 / cnd$step[2] + 1)) %>>%
		filter(x > 0 & y > 0 & x <= cnd$px[1] & y <= cnd$px[2]) %>>% #remove data out of maps
		mutate(nr = (y-1) * cnd$px[1] + x) %>>%
		distinct(nr, .keep_all = TRUE)

	#find initial centers
	centers <- qltmap %>>%
		lapply(`[`, qnt$cnd$nr) %>>%
	  lapply(as.double) %>>%
		as.data.table %>>%
		setcolorder(order(names(.))) %>>%
		mutate(phase = qnt$cnd$phase) %>>%
	  filter(!is.na(phase)) %>>%
		arrange(phase) %>>%
	  gather(elm, val, -phase) %>>%
		group_by(phase, elm) %>>%
		summarise(val = median(val)) %>>%
	  ungroup %>>%
	  spread(elm, val)

	if(is.character(saving)) fwrite(centers, saving)

	return(centers)
}
















