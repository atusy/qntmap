#' Interactively quantify X-ray maps
#' 
#' By default, it starts browser to open web-based UI. All arguments are optional.
#' 
#' @param xmap_dir,qnt_dir
#'  Paths to the directories containing data from mapping and spot analysis.
#' @param deadtime
#'  Dead time in nano seconds.
#' @inheritParams read_xmap
#' @inheritParams read_qnt
#' @param selected
#'  A starting page of shiny UI. One of the "auto", "Input", "Map" or "Spot".
#'  If auto (default), "Map" is chosen when `xmap_dir` is specified, or "Spot"
#'  when `qnt_dir` is specified, or "Input" when both are not specified.
#' @param shiny Wheter to use shiny UI (default: `TRUE`)
#'
#' @importFrom easycsv choose_dir
#' @importFrom utils menu select.list
#' @export
qntmap <- function(
  xmap_dir = NULL, qnt_dir = NULL,
  deadtime = 1100, phase_list = NULL,
  shiny = TRUE, selected = c("auto", "Input", "Map", "Spot")
) {
  if (shiny) {
    example <- system.file(package = "qntmap", "extdata", "minimal")
    
    selected <- match.arg(selected)
    if (selected == "auto") {
      selected <- c(
          "Map", "Spot", "Input"
        )[c(!is.null(xmap_dir), !is.null(qnt_dir), TRUE)][[1L]]
    }

    if (is.null(xmap_dir)) {
      deadtime <- 0
      xmap_dir <- file.path(example, ".map/1")
    }
    if (is.null(qnt_dir)) qnt_dir <- file.path(example, ".qnt")
    
    shiny::shinyApp(
      shiny_ui(
        xmap_dir = xmap_dir, qnt_dir = qnt_dir, deadtime = deadtime,
        phase_list = phase_list, selected = selected
      ), 
      shiny_server()
    )
  } else {
    qntmap_console()
  }
}

qntmap_console <- function() {
  cd <- getwd()
  on.exit(setwd(cd))
  
  cat("(1) Select a directory containing .map and .qnt directories\n")
  setwd(wd <- choose_dir()) # easycsv::choose_dir
  if (!all(file.exists(c(".map", ".qnt"))))
    while (!all(file.exists(c(".map", ".qnt")))) {
      cat(
        "(1) Selected directory does not contain ",
        ".map and .qnt directory. Select again\n"
      )
      setwd(wd <- choose_dir()) # easycsv::choose_dir
    }
  cat("Working directory is settled to\n", wd, "\n\n")
  
  cat("(2) Select directory containing *_map.txt files to be analyzed\n")
  dir_map <- select.list(list.dirs(".map", recursive = FALSE))
  cat("*_map.txt in a following directory will be clustered or quantified\n", dir_map, "\n\n")
  
  if (!length(list.files(dir_map, pattern = "_map\\.txt")))
    stop(
      "Selected directory does not contain *_map.txt files. ",
      "Check if results are ASCII converted by and exported from EPMA."
    )
  
  cat(
    "Input dead time in nano seconds\n",
    "0 if no corrections required)\n",
    "1100 is the default value for JXA-8105\n",
    sep = ""
  )
  DT <- as.numeric(readline())
  cat("Dead time is ", DT, " nano seconds\n\n")
  
  cat("Identify phases of quantified points based on")
  selection <- menu(
    c(
      "comments input during quantification",
      "an external csv file"
    )
  )
  cat("\n")
  if (selection == 1L) phase_list <- NULL
  if (selection == 2L) {
    cat("Choose an external csv file\n")
    phase_list <- file.choose()
    cat("Chosen", phase_list, "\n\n")
  }

  cat("Loading mapping data\n")
  xmap <- read_xmap(dir_map, DT = DT)
  cat("Loading quantified data\n")
  qnt <- read_qnt(".qnt", phase_list)
  cat("Peforming cluster analysis\n")
  centers <- find_centers(xmap = xmap, qnt = qnt)
  cls <- cluster_xmap(xmap = xmap, centers = centers)
  cat("Finished cluster analysis. Result is in ", dir_map, "/clustering\n\n")

  cat(
    "Specify phases tending to be ",
    "finer than mapping probe diameter"
  )
  fine_phase <- select.list(
    colnames(cls$membership), multiple = TRUE
  )
  cat("\n")

  cat("Quantifying mapping data\n")
  qmap <- quantify(
    xmap = xmap,
    qnt = qnt,
    cluster = cls,
    fine_phase = if (length(fine_phase)) fine_phase else NULL
  )

  cat(
    "Finished quantification.\n",
    "Results are saved under",
    paste0(dir_map, "/qntmap\n\n")
  )

  cat("Summary of quantified mapping data\n")
  print(summary(qmap))

  invisible(qmap)
}
