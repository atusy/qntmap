#' save the result of `cluster_xmap()` or `cluster_group()` as RDS file and png file
#'
#' @param result result of clustering
#' @param name_write name of result to be written
#' @param components components in RDS
#'
#' @importFrom png writePNG
#'
xmap_cls_save <- function(result, name_write, components = NULL) {
  #setting for output
  dir_out <- 'clustering'
  dir.create(dir_out, showWarnings = FALSE)
  k <- ncol(result$membership)
  name_write <- paste0('./', dir_out, '/', result$date, '_', name_write, '_', 'k', k, '_', paste(result$elements, collapse =''))
  color <- mycolors(n = k)
  LUT <- mycolors(n = k, dec = TRUE) / 255

  #save modal map
  png::writePNG(
    image = array(
      LUT[result$ytehat, ],
      dim = c(result$dims, 3)
    ),
    target = paste0(name_write, "_map.png")
  )

  #save result of classification
  if(is.null(components)){
    saveRDS(result, paste0(name_write, "_result.RDS"))
  } else {
    saveRDS(result[components], paste0(name_write, "_result.RDS"))
  }

  #save legend
  pie(rep(1, k), labels = paste(1:k, colnames(result$membership), sep='.'), col = color)
  dev.copy(png, paste0(name_write, "_legend.png"))
  dev.off()

}
