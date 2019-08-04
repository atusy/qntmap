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
          tags$p(tags$strong("Directory containing mapping data.")),
          cssgrid::grid_rowwise(
            textInput("xmap_dir", label = NULL, value = xmap_dir),
            shiny_dir_btn("xmap_dir_btn", "Choose a directory"),
            cols = c("1fr auto"), gap = "2px"
          ),
          numericInput(
            "xmap_deadtime",
            "Dead time in nanoseconds.",
            value = deadtime
          ),
          tags$p("1100 is the default value of JXA-8800"),
          
          # Spot
          tags$h1("Spot analysis"),
          tags$p("Directory containing spot analysis data."),
          cssgrid::grid_rowwise(
            textInput("qnt_dir", label = NULL, value = qnt_dir),
            shiny_dir_btn("qnt_dir_btn", "Choose a directory"),
            cols = c("1fr auto"), gap = "2px"
          ),
          tags$p("Path to the csv file identifying phases for each analyzed spots"),
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
