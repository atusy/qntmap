#' @importFrom shinyFiles shinyDirButton shinyFilesButton
#' @importFrom shiny icon
shiny_ui_input <- function(xmap_dir, deadtime, qnt_dir, phase_list) {
  tabPanel(
    "Input",
    sidebarLayout(
      sidebar_tabset_panel(
        tab_panel(
          "Menu",
          # Map
          tags$h1("Mapping"),
          tags$p(
            "Directory containing mapping data.",
            tippy_info(
              "xmap_dir_tippy_info",
              'The directory is typically saved under ".map"'
            )
          ),
          cssgrid::grid_rowwise(
            textInput("xmap_dir", label = NULL, value = xmap_dir),
            shiny_dir_btn("xmap_dir_btn", "Choose a directory"),
            cols = c("1fr auto"), gap = "2px"
          ),
          tags$p(
            "Dead time in nanoseconds.",
            tippy_info(
              "deadtime_tippy_info",
              "1100 is the default value of JXA-8800"
            )
          ),
          numericInput("xmap_deadtime", NULL, value = deadtime),

          # Spot
          tags$h1("Spot analysis"),
          tags$p(
            "Directory containing spot analysis data.",
            tippy_info(
              "qnt_dir_tippy_info",
              'The directory is typically named as ".qnt"'
            )
          ),
          cssgrid::grid_rowwise(
            textInput("qnt_dir", label = NULL, value = qnt_dir),
            shiny_dir_btn("qnt_dir_btn", "Choose a directory"),
            cols = c("1fr auto"), gap = "2px"
          ),
          tags$p(
            "Optional csv file identifying phases",
            tippy_info(
              "phase_list_tippy_info",
              htmltools::doRenderTags(tagList(
                tags$p(
                  "If not specified,",
                  "comments on each spots are regarded as phase names."
                ),
                tags$p(
                  "In order to get template,",
                  "Leave here blank and load once.",
                  "Then go see a table on the left pane of the Spot page"
                )
              ))
            )
          ),
          cssgrid::grid_rowwise(
            textInput("phase_list", label = NULL, value = phase_list),
            shiny_files_btn("phase_list_btn", "Choose a csv file"),
            cols = c("1fr auto"), gap = "2px"
          ),
          
          # Reload
          actionButton("input_load", label = "Load")
        )
      ),
      main_tabset_panel(
        tab_panel("Mapping conditions", DTOutput("xmap_meta")),
        tab_panel("Spot analysis conditions", DTOutput("qnt_elm"))
      )
    )
  )
}
