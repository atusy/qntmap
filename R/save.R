#' save objects created by functions in QntMap package.
save4qm <- function(x, nm, saving = TRUE, ...) {
  if(!saving) return(invisible(x))
  UseMethod('save4qm')
}
save4qm.qm_xmap <- save4qm.qm_qnt <- 
  function(x, nm, ...) saveRDS(x, nm)
save4qm.data.frame <- 
  function(x, nm, ...) data.table::fwrite(x, nm)
#' save the result of `cluster_xmap()` or `cluster_group()` as RDS file and png file
#'
#' @param result result of clustering
#' @param name_write name of result to be written
#' @param components components in RDS
#'
#' @importFrom png writePNG
#'
save4qm.qm_cluster <- function(x, nm, components = NULL, ...) {
  #setting for output
  dir_out <- paste0(x$dir_map, '/clustering')
  dir.create(dir_out, showWarnings = FALSE)
  k <- ncol(result$membership)
  nm <- paste0(
      dir_out, '/', x$date, '_', nm, '_k', k, '_', 
      paste(result$elements, collapse ='')
    )

  #save modal map
  png::writePNG(
    image = array(
      mycolors(n = k, dec = TRUE)[result$ytehat, ] / 255,
      dim = c(result$dims, 3)
    ),
    target = paste0(nm, "_map.png")
  )

  #save result of classification
  if(is.null(components)){
    saveRDS(result, paste0(nm, "_result.RDS"))
  } else {
    saveRDS(result[components], paste0(nm, "_result.RDS"))
  }

  #save legend
  pie(
    rep(1, k), 
    labels = paste(1:k, colnames(result$membership), sep='.'), 
    col = mycolors(n = k)
  )
  dev.copy(png, paste0(nm, "_legend.png"))
  dev.off()
  
  invisible(x)
}
save4qm.qntmap <- function(x, nm, dir_qntmap, ...) {
  cd <- getwd(); on.exit(setwd(cd))
  setwd(dir_qntmap)
  saveRDS(x, 'qntmap.RDS')
  pipeR::pipeline({
    unlist(x, recursive = FALSE)
    purrr::walk2(
      paste0(str_replace(names(.), '\\.', '_'), '.csv'),
      data.table::fwrite
    )
  })
  invisible(x)
}