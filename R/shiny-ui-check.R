# © 2019 JAMSTEC
shiny_ui_check <- function() {
  tabPanel(
    "Check",
    sidebarLayout(
      sidebar_tabset_panel(
        # Find outliers
        tab_panel(
          "Menu",
          tags$h1("Find outliers"),
          tags$p(
            "to obtain better initial centroids for clustering and",
            "$\\beta$ parameter for quantification."
          ),
          uiOutput("outlier_elem_selecter"),
          uiOutput("outlier_phase"),
          tags$p(
            "Note: the red line is drawn by least squares regression and",
            "the blue is by median regression.",
            "Ideally, they become apparently identical because of",
            "the relationship of mean ($\\lambda$) and median ($\\nu$)",
            "in Poisson distribution:",
            "$$\\lambda - \\ln{2} \\leq \\nu \\lt \\lambda + 1/3$$",
            "Thus, it is a good idea to remove phases being outliers until",
            "the red and the blue lines become identical."
          )
        ),
        tab_check_detail
      ),
      main_tabset_panel(
        tab_panel(
          "Plot",
          plotOutput(
            "outlier_plot", height = "100%",
            dblclick = "outlier_click",
            brush = brushOpts(id = "outlier_brush", resetOnNew = TRUE)
          )
        ),
        tab_panel("Initial centroid", DTOutput("centroid"))
      )
    )
  )
}

tab_check_detail <- tab_panel(
  "Detail",
  tags$h1("Multi-phase pixels and outliers"),
  tags$p(
    "To compare data of spot analysis and mapping from the same pixels,",
    "it is important that both are analyzing the same phase.",
    "However, mapping data inevitably contain pixels analyzing",
    "multiple phases because",
    "coordinates of pixels are automatically determined, and",
    "probe diameter is generally larger on mapping than on spot analysis."
  ),
  tags$p(
    "Multi-phase pixels cause lower or higher signal intensities",
    "than spot analysis on corresponding coordinates.",
    "In other words, multi-phase pixels appear as horizontally",
    "scattered outliers in the scatter plot on the right pane."
  ),
  
  tags$h1("Algorithm of removing outliers"),
  tags$p(
    "There are two steps on removing outliers."
  ),
  tags$p(
    "First step is manual.",
    "If user specified phases being outliers, they are removed."
  ),
  tags$p(
    "Scond step is automatic.",
    "Median regression fits data to $y = kx$, and",
    "draws a resulting curve and 99% prediction intervals.",
    "If 95% confidence intervals of data points overlaps with",
    "the prediction intervals,",
    "such data are preserved and others are removed."
  ),
  tags$p(
    "Note that the second step removes data points", 
    "if they appear to be outliers in any element."
  )
)