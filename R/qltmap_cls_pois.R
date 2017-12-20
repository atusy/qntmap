#' cluster mapping data into mineral species
#'
#' @param centers_initial a path to csv file telling initial centers of clusters
#' @param qltmap default to NULL
#' @param elements If NULL, all mapped elements are used for clustering. Specifiyng elements may reduce analytical time.
#' @param wd Directory containing mapping data. If NULL, current directory is wd.
#' @param saving TRUE or FALSE to save result
#' @param integration TRUE or FALSE to integrate same phase with obiously different compositions. For example, when there are clusters named as Pl_NaRich and Pl_NaPoor, they are integrated to Pl.
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table fread
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr summarise_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr one_of
#' @importFrom pipeR %>>%
#' @importFrom PoiClaClu Classify
#' @importFrom stringr str_extract
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#'
#'
#' @export
qltmap_cls_pois <- function(
    centers_initial,
    qltmap = NULL,
    elements = NULL,
    wd = NULL,
    saving = TRUE,
    integration = TRUE
  ) {
    cd <- getwd()
    on.exit(setwd(cd))

    #####for the test
    #centers_initial = 'centers_initial3.csv'; qltmap = NULL; elements = c('Ti','Al','Fe','Mg','Ca','Na'); name_write = 'pois'; path_cnd = '0.cnd'; saving = TRUE
    #centers_initial = centers; qltmap = readRDS('qltmap.RDS'); dims = dims[[wd]]; elements = NULL; wd = NULL; name_write = 'pois'; path_cnd = '0.cnd'; saving = TRUE;  components = c('ytehat', 'center', 'membership', 'date', 'dims', 'elements')

    #####set working directory
    if(!is.null(wd)) setwd(wd)

    #####load data
    if(is.character(centers_initial)) fread(centers_initial)

    #qltmap: import mapping data
    if(is.null(qltmap)) qnt <- qltmap_load()
    if(is.character(qltmap)) qltmap <- readRDS(qltmap)
    if(!all(class(qltmap) == c('list', 'qltmap'))) stop('illegal input of qltmap')

    if(is.null(elements)) elements <- names(qltmap)

    dims <- dim(qltmap[[1]])

    #####check data
    if(all(elements %in% names(qltmap)) == FALSE) stop("Specified wrong element which is not present in qltmap")
    if(all(elements %in% names(centers_initial)) == FALSE) stop("Specified wrong element which is not present in centers_initial")

    #####initial clusters===================================================================================================
    x <- qltmap %>>%
      `[`(elements) %>>%
      lapply(unlist, use.names = FALSE) %>>%
      as.data.table

    rm(qltmap)

    y <- centers_initial %>>%
        select(one_of(elements)) %>>%
        apply(1, function(y) colSums((t(x) - y) ^ 2)) %>>%
        apply(1, which.min)



    #####Classify by PoiClaClu ===================================================================================================
    result <- PoiClaClu::Classify(x, y, x)

    rm(y)

    #####give phase names to result$ytehat==================================================================
    names(result$ytehat) <- centers_initial$phase[result$ytehat]

    #####Find representative values of each clusters (~ centers)==================================================================
    #bw <- sapply(qltmap, sd) / (length(result$ytehat) ^ (1/3)) #Scott
    result$center <- x %>>%
      lapply(as.double) %>>%
      as.data.table %>>%
      mutate(phase = names(result$ytehat)) %>>%
      gather(elm, val, -phase) %>>%
      group_by(phase, elm) %>>%
      summarise(val = median(val)) %>>%
      ungroup %>>%
      spread(elm, val) %>>%
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

    if(integration) result <- qltmap_cls_pois_integrate(result, wd = NULL, saving = saving)

    result
}







































