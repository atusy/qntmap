#' cluster mapping data into mineral species
#'
#' @param centers_initial a path to csv file telling initial centers of clusters
#' @param qltmap default to NULL
#' @param dims default to NULL
#' @param elements If NULL, all mapped elements are used for clustering. Specifiyng elements may reduce analytical time.
#' @param wd Directory containing mapping data. If NULL, current directory is wd.
#' @param path_cnd to 0.cnd of mapping analysis. If NULL, 0.cnd is considered to be present under wd
#' @param saving TRUE or FALSE to save result
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom dplyr one_of
#' @importFrom pipeR %>>%
#' @importFrom PoiClaClu Classify
#' @importFrom stringr str_extract
#'
#' @export

qltmap_cls_pois <- function(
    centers_initial,
		qltmap = NULL,
		dims = NULL,
		elements = NULL,
		wd = NULL,
		path_cnd = '0.cnd',
		saving = TRUE
	) {
		cd <- getwd()
		on.exit(setwd(cd))

		#####for the test
		#centers_initial = 'centers_initial3.csv'; qltmap = NULL; elements = c('Ti','Al','Fe','Mg','Ca','Na'); name_write = 'pois'; path_cnd = '0.cnd'; saving = TRUE
		#centers_initial = centers; qltmap = readRDS('qltmap.RDS'); dims = dims[[wd]]; elements = NULL; wd = NULL; name_write = 'pois'; path_cnd = '0.cnd'; saving = TRUE;  components = c('ytehat', 'center', 'membership', 'date', 'dims', 'elements')

		#####set working directory
		if(!is.null(wd)) setwd(wd)

		#####load data
		if(is.null(centers_initial)) {
			stop('specify a path of a csv file containing initial centers')
		} else {
			centers_initial <- fread(centers_initial)
		}

		if(is.null(qltmap)) qltmap <- qltmap_load(list_of = 'vector')

		if(is.null(elements)) elements <- names(qltmap)

		if(is.null(dims)) {
			dims <- readLines(path_cnd) %>>%
				`[`(str_detect(., 'Step Number')) %>>%
				str_extract('^[0-9]+') %>>%
				as.integer
		}

		#####check data
		if(all(elements %in% names(qltmap)) == FALSE) stop("Specified wrong element which is not present in qltmap")
		if(all(elements %in% names(centers_initial)) == FALSE) stop("Specified wrong element which is not present in centers_initial")

		#####initial clusters===================================================================================================
		x <- qltmap %>>%
			`[`(elements) %>>%
			as.data.table

		y <- centers_initial %>>%
				select(one_of(elements)) %>>%
				apply(1, function(y) colSums((t(x) - y) ^ 2)) %>>%
				apply(1, which.min)



		#####Classify by PoiClaClu ===================================================================================================
		result <- PoiClaClu::Classify(x, y, x)

		rm(x, y)

		#####give phase names to result$ytehat==================================================================
		names(result$ytehat) <- centers_initial$phase[result$ytehat]

		#####Find representative values of each clusters (~ centers)==================================================================
		bw <- sapply(qltmap, sd) / (length(result$ytehat) ^ (1/3)) #Scott
		result$center <- qltmap %>>%
			lapply(as.double) %>>%
			as.data.table %>>%
			mutate(phase = names(result$ytehat)) %>>%
			group_by(phase) %>>%
			summarise_all(median) %>>%
			as.data.table

		#####estimate membership of each clusters ==================================================================
		result$membership <- result$discriminant %>>%
			`-`(apply(., 1, max)) %>>%
			exp %>>%
			`/`(rowSums(.))

		if(is.vector(result$membership)) result$membership <- as.matrix(result$membership)
		if(nrow(centers_initial) == ncol(result$membership)) {
			colnames(result$membership) <- centers_initial$phase
		} else {
			if(ncol(result$membership) == 1) {
				colnames(result$membership) <- names(result$ytehat[1])
			} else {
				TF <- !duplicated(names(result$ytehat))
				colnames(result$membership)[apply(result$membership[TF, ], 1, which.max)] <- names(result$ytehat)[TF]
				rm(TF)
			}
			missings <- centers_initial$phase %>>%
				`[`(!(. %in% colnames(result$membership)))
			result$membership <- result$membership %>>%
				cbind(
					matrix(
						0,
						nrow(.),
						ncol = length(missings),
						dimnames = list(NULL, missings)
					)
				) %>>%
				`[`(, centers_initial$phase %>>% as.character)
			rm(missings)
		}


		#####additional informations
		result$date <- format(Sys.time(), "%y%m%d_%H%M")
		result$dims <- dims
		result$elements <- elements

		components <- c('ytehat', 'center', 'membership', 'date', 'dims', 'elements')
		if(saving) qltmap_cls_save(result, 'pois', components)

		result
}







































