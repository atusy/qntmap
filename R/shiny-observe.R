observe_and_sync_color <- function(input, session) {
  observeEvent(input$xmap_color, {
    shinyWidgets::updatePickerInput(session, "qmap_color", selected = input$xmap_color)
  })
  observeEvent(input$qmap_color, {
    shinyWidgets::updatePickerInput(session, "xmap_color", selected = input$qmap_color)
  })
}

observe_and_sync_scale <- function(input, session) {
  observeEvent(input$xmap_scale, {
    shinyWidgets::updatePickerInput(session, "qmap_scale", selected = input$xmap_scale)
    shinyWidgets::updatePickerInput(session, "cluster_scale", selected = input$xmap_scale)
  })
  observeEvent(input$cluster_scale, {
    shinyWidgets::updatePickerInput(session, "xmap_scale", selected = input$cluster_scale)
  })
  observeEvent(input$qmap_scale, {
    shinyWidgets::updatePickerInput(session, "xmap_scale", selected = input$qmap_scale)
  })
}