#' Generate initial centroids for clustering
#'
#' @param path_qnt path to the csv file of quantitative analysis
#' @param path_cnd path to the 0.cnd file  of mapping analysis
#' @param qltmap path to the .RDS file which compiles mapping data
#' @param wd working directory which contains mapping data
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
#'
#' @importFrom pipeR %>>%
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_detect
#'
#' @export


#prioritize(dplyr)

#path of the directry containing data
#wd <- '~/Univ/!Data_ND/epma/WDX/ND0207_160819/.map/10'

qltmap_cls_centers <- function(path_qnt = 'qnt.csv', path_cnd = '0.cnd', qltmap = NULL, wd = NULL) {
	cd <- getwd()
	on.exit(setwd(cd))
	if(!is.null(wd)) setwd(wd)

	#qltmap: import mapping data
	qltmap <- qltmap_load()

	#cnd: import analysis conditions from 0.cnd
	cnd <- path_cnd %>>%
		readLines %>>%
		'['(str_detect(.,
			'(Measurement Start Position X)|(Measurement Start Position Y)|(X-axis Step Number)|(Y-axis Step Number)|(X-axis Step Size)|(Y-axis Step Size)|(X Step Size)|(Y Step Size)'
			)) %>>%
		str_replace_all('[:blank:].*', '') %>>%
		as.numeric %>>%
		matrix(ncol=3, nrow=2, dimnames = list(NULL, c('start', 'px', 'step'))) %>>%
		as.data.table

	#qnt =================================================================
	#qnt: convert stage coordinates to map coordinates
	#qnt: and then remove points with coordinates out of maps
	qnt <- path_qnt %>>%
		fread(select = c('x', 'y', 'phase')) %>>%
	#	mutate(x.stage = x, y.stage = y) %>>%
		mutate(
			x = round((x - cnd$start[1]) * 1000 / cnd$step[1] + 1),
			y = round((y - cnd$start[2]) * 1000 / cnd$step[2] + 1)) %>>%
		filter(x > 0 & y > 0 & x <= cnd$px[1] & y <= cnd$px[2]) %>>% #remove data out of maps
		mutate(nr = (y-1) * cnd$px[1] + x) %>>%
		distinct(nr, .keep_all = TRUE)

	#find initial centers
	qltmap %>>%
		lapply(`[`, qnt$nr) %>>%
		as.data.table %>>%
		setcolorder(order(names(.))) %>>%
		mutate_all(as.double) %>>%
		mutate(phase = qnt$phase) %>>%
		arrange(phase) %>>%
		group_by(phase) %>>%
		summarise_all(median) %>>%
		fwrite('centers_initial0.csv')
}
















