#' Poisson distribution based custering based on `PoiClaClu:Classify()`
#' @param x data frame or matrix
#' @param center data frame or matrix
#' @importFrom PoiClaClu Classify
#' @export
cluster <- function(x, centers) {
  x_trans <- t(x)
  y <- pipeline({
    centers
    apply(1, function(y) colSums(x_trans - y) ^ 2)
    apply(1, which.min)
  })
  rm(x_trans)
  Classify(x, y, x)
}

#' cluster mapping data into mineral species
#'
#' @param qltmap default to NULL
#' @param centers a path to csv file telling initial centers of clusters
#' @param elements If NULL, all mapped elements are used for clustering. Specifiyng elements may reduce analytical time.
#' @param wd Directory containing mapping data. If NULL, current directory is wd.
#' @param saving TRUE or FALSE to save result
#' @param integration TRUE or FALSE to integrate same phase with obiously different compositions. For example, when there are clusters named as Pl_NaRich and Pl_NaPoor, they are integrated to Pl.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom pipeR pipeline
#' @importFrom PoiClaClu Classify
#' @importFrom tidyr gather
#' @importFrom tidyr spread
#'
#'
#' @export
qltmap_cls_pois <- function(
    centers_initial,
    dir_map,
    elements = NULL,
    wd = dir_map,
    saving = TRUE,
    integration = TRUE,
    qltmap = read_xmap(dir_map)
) {
    # set working directory
    cd <- getwd()
    on.exit(setwd(cd))
    setwd(wd)

    # load data
    if(is.character(centers_initial)) 
      centers_initial <- fread(centers_initial)

    dims <- dim(qltmap[[1]])

    elements <- `if`(
      is.null(elements),
      names(qltmap)[names(qltmap) %in% names(centers_initial)],
      elements[
        elements %in% names(qltmap) & 
        elements %in% names(centers_initial)
      ]
    )

    # initial clusters
    x <- pipeline({
      qltmap[elements]
      lapply(unlist, use.names = FALSE)
      as.data.frame
    })

    rm(qltmap)


    rm(y)

    # give phase names to result$ytehat
    names(result$ytehat) <- centers_initial$phase[result$ytehat]

    # Find representative values of each clusters (~ centers)
    result$center <- pipeline({
      x
      lapply(as.double)
      as.data.frame
      mutate(phase = names(result$ytehat))
      gather(elm, val, -phase)
      group_by(phase, elm)
      summarise(val = median(val))
      ungroup
      spread(elm, val)
    })

    # estimate membership of each clusters
    result$membership <- pipeline({
      result$discriminant
      `-`(apply(., 1, max))
      exp
      `/`(rowSums(.))
    })
  # Classify by PoiClaClu 
  result <- cluster(x, centers = centers[, elements])

    if(is.null(dim(result$membership))) result$membership <- as.matrix(result$membership)

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
        `[`(. %nin% colnames(result$membership))
      result$membership <- cbind(
          result$membership,
          matrix(
            0,
            nrow(.),
            ncol = length(missings),
            dimnames = list(NULL, missings)
          )
        )[, as.character(centers_initial$phase)]
      rm(missings)
    }


    # additional informations
    class(result) <- c('qltmap_cls', 'list')
    result$date <- format(Sys.time(), "%y%m%d_%H%M")
    result$dims <- dims
    result$elements <- elements

    components <- c('ytehat', 'center', 'membership', 'date', 'dims', 'elements')
    if(saving) qltmap_cls_save(result, 'pois', components)

    if(integration && any(grepl('_', colnames(result$membership))))
        result <- qltmap_cls_pois_integrate(result, wd = NULL, saving = saving)

    result
}

