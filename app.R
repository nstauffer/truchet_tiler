# Required packages
library(shiny)
library(colourpicker)
library(tidyverse)
source("support_functions.R")

# For testing
# input <- list(n_cols = 5,
#               n_rows = 5,
#               n_lines = 3,
#               min_radius = 0.15,
#               proportion_lines = 0.65,
#               proportion_edges = 0.35,
#               arc_vertex_count = 2,
#               color_line_high = "#483D8B",
#               color_line_low = "#3DDBD9",
#               color_edging_high = "#FFB000",
#               color_edging_low = "#DC267F",
#               color_background = "#36013F",
#               allow_straights = FALSE,
#               margin = 0.15,
#               tile_pixels = 100,
#               row_1 = seq_len(5),
#               row_2 = seq_len(5),
#               row_3 = seq_len(5),
#               row_4 = seq_len(5),
#               row_5 = seq_len(5))
# workspace <- list(temp_dir = "www",
#                   current_output = NULL,
#                   buffer = FALSE,
#                   respect_margins = TRUE,
#                   base_matrix = matrix(data = rep(1,
#                                                   times = input$n_cols * input$n_rows),
#                                        nrow = input$n_rows),
#                   rotation = 0,
#                   colors = NULL,
#                   svg = NULL)

# UI ###########################################################################
# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("Truchet Tiler v0.7.0"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 6,
               numericInput(inputId = "n_cols",
                            label  = "Number of tiles wide:",
                            min = 3,
                            max = 25,
                            value = 7)),
        column(width = 6,
               numericInput(inputId = "n_rows",
                            label  = "Number of tiles tall:",
                            min = 3,
                            max = 25,
                            value = 7))
      ),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "n_lines",
                            label  = "Number of lines:",
                            min = 1,
                            max = 10,
                            value = 3)),
        column(width = 6,
               numericInput(inputId = "min_radius",
                            label  = "Inner curve radius:",
                            min = 0.1,
                            max = 0.4,
                            value = 0.15))
      ),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "proportion_lines",
                            label  = "Proportion of band occupied by lines:",
                            min = 0,
                            max = 1,
                            value = 0.65)),
        column(width = 6,
               numericInput(inputId = "proportion_edges",
                            label  = "Proportion of band occupied by edging:",
                            min = 0,
                            max = 1,
                            value = 0.35))
      ),
      fluidRow(
        column(width = 6,
               numericInput(inputId = "arc_vertex_count",
                            label  = "Number of vertices (0 produces a smooth arc):",
                            min = 0,
                            max = 4,
                            value = 0,
                            step = 1))#,
        # column(width = 6,
        #        numericInput(inputId = "proportion_edges",
        #                     label  = "Proportion of band occupied by edging:",
        #                     min = 0,
        #                     max = 1,
        #                     value = 0.35))
      ),
      fluidRow(
        column(width = 6,
               colourInput(inputId = "color_line_high",
                           label  = "Line color (high)",
                           value = "483D8B")),
        column(width = 6,
               colourInput(inputId = "color_line_low",
                           label  = "Line color (low)",
                           value = "3DDBD9"))
      ),
      fluidRow(
        column(width = 6,
               colourInput(inputId = "color_edging_high",
                           label  = "Edging color (high)",
                           value = "FFB000")),
        column(width = 6,
               colourInput(inputId = "color_edging_low",
                           label  = "Edging color (low)",
                           value = "DC267F"))
      ),
      colourInput(inputId = "color_background",
                  label  = "Background color",
                  value = "36013F"),
      checkboxInput(inputId = "allow_straights",
                    label = "Allow straight segments",
                    value = FALSE),
      numericInput(inputId = "margin",
                   label  = "Distance from cap to tile edge:",
                   min = 0.01,
                   max = 0.49,
                   value = 0.15),
      numericInput(inputId = "tile_pixels",
                   label  = "Tile height in pixels:",
                   min = 10,
                   max = 1000,
                   value = 100),
      actionButton(inputId = "generate_button",
                   label = "Generate!"),
      actionButton(inputId = "cleanup_button",
                   label = "Cleanup!")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput(outputId = "svg_embed",
                 inline = TRUE),
      helpText("Use checkboxes below to adjust which tiles will be populated in the array when generating."),
      uiOutput(outputId = "matrix")
    )
  )
)

# SERVER #######################################################################
server <- function(input, output) {
  
  output$svg_embed <- renderUI(expr = HTML(paste0("<img src = 'example.svg' height = '100%'>")))
  
  output$matrix <- renderUI({
    lapply(X = seq_len(input$n_rows),
           n_cols = input$n_cols,
           function(X, n_cols) {
             checkboxGroupInput(inputId = paste0("row_", X),
                                label = NULL,
                                inline = TRUE,
                                selected = seq_len(n_cols),
                                choiceNames = rep("",
                                                  times = n_cols),
                                choiceValues = seq_len(n_cols))
           })
  })
  
  workspace <- reactiveValues(temp_dir = "www",
                              current_output = NULL,
                              buffer = FALSE,
                              respect_margins = TRUE,
                              base_matrix = matrix(data = rep(1,
                                                              times = 7 * 7),
                                                   nrow = 7),
                              rotation = 0,
                              colors = NULL,
                              svg = NULL)
  
  observeEvent(eventExpr = workspace$base_matrix,
               handlerExpr = {
                 for (current_base_matrix_row in seq_len(nrow(workspace$base_matrix))) {
                   updateCheckboxGroupInput(inputId = paste0("row_", current_base_matrix_row),
                                            selected = which(workspace$base_matrix[current_base_matrix_row,] == 1))
                 }
               })
  
  observeEvent(eventExpr = input$row_1,
               handlerExpr = {
                 message(paste(input$row_1,
                               collapse = " "))
               })
  
  observeEvent(eventExpr = input$cleanup_button,
               handlerExpr = {
                 file.remove(list.files(path = workspace$temp_dir,
                                        pattern = paste0("output_\\d+"),
                                        full.names = TRUE))
               })
  
  observeEvent(eventExpr = input$generate_button,
               handlerExpr = {
                 message("building colors list")
                 workspace$colors <- list(line = c(high = paste0(input$color_line_high),
                                                   low = paste0(input$color_line_low)),
                                          edging = c(high = paste0(input$color_edging_high),
                                                     low = paste0(input$color_edging_low)),
                                          background = paste0(input$color_background))
                 
                 
                 ##### Making the lines ----------------------------------------------------------
                 ###### Radii and thickness -----------------------------------------------------
                 # This chunk calculates the radii and thickness for the lines so that they'll
                 # align properly between the tiles.
                 message("calculating radii and widths")
                 if (input$n_lines == 1) {
                   radii <- 0.5
                   line_width <- input$proportion_lines
                   edging_width <- input$proportion_edges * input$proportion_lines
                 } else {
                   
                   total_band_width <- 1 - (2 * input$min_radius)
                   
                   line_width <- total_band_width * input$proportion_lines / input$n_lines
                   edging_width <- total_band_width * min((1 - input$proportion_lines),
                                                          input$proportion_edges) / input$n_lines
                   
                   radii <- seq(from = input$min_radius + 0.5 * line_width,
                                to = 1 - input$min_radius - 0.5 * line_width,
                                length.out = input$n_lines)
                 }
                 
                 
                 # Edging the lines with these.
                 edging_radii <- c(sapply(X = radii - line_width / 2 - edging_width / 2,
                                          minimum_radius = edging_width / 2,
                                          # minimum_radius = 0,
                                          FUN = function(X, minimum_radius){
                                            max(c(X,
                                                  minimum_radius))
                                          }),
                                   sapply(X = radii + line_width / 2 + edging_width / 2,
                                          maximum_radius = 1 - edging_width / 2,
                                          FUN = function(X, maximum_radius){
                                            min(c(X,
                                                  maximum_radius))
                                          }))
                 edging_radii <- edging_radii[order(edging_radii)]
                 
                 
                 ###### Line definitions ---------------------------------------------------------
                 # These are the parameters for the lines for various connections
                 
                 # These are all assuming that they're drawn clockwise in the case of arcs, so
                 # it took some mental gymnastics to finally make sure that all the lines were
                 # drawn in the correct order.
                 
                 # These are a silly format but it works for generating the paths.
                 # The variables are used to differentiate how the path should be assembled:
                 # For arc-only lines, the origin and terminus variables should be NA
                 # For segment-only lines, the radius variables should be NA
                 # For segment-arc-segment lines, there should be no NA values
                 
                 # For arcs:
                 # The start and end variables are used as the start and end coordinates of an
                 # arc.
                 
                 # For segments:
                 
                 # For segment-arc-segments:
                 
                 # This is calculating displacements from the far edge for caps.
                 # Basically it's margin + the cumulative widths of the lines between a line and
                 # that margin + radius of the current line.
                 # The radii are being adjusted to be from the center of the side instead of
                 # from a corner because these are half circles connected to a single side
                 # instead of quarter circles connected to two sides.
                 message("Creating tile subunits")
                 radii_df <- dplyr::bind_rows(data.frame(type = "line",
                                                         corner_radius = radii,
                                                         radius = abs(radii - 0.5),
                                                         width = line_width),
                                              data.frame(type = "edging",
                                                         corner_radius = edging_radii,
                                                         radius = abs(edging_radii - 0.5),
                                                         width = edging_width)) |>
                   dplyr::mutate(.data = _,
                                 dplyr::across(.cols = -tidyselect::all_of(c("type")),
                                               .fns = ~ round(x = .x,
                                                              digits = 5))) |>
                   dplyr::distinct(.data = _) |>
                   dplyr::arrange(.data = _,
                                  dplyr::desc(corner_radius))# |>
                 # dplyr::mutate(.data = _,
                 #               radius_rank = dplyr::row_number())
                 # seq(from = )
                 
                 cap_radii_df <- radii_df |>
                   dplyr::select(.data = _,
                                 -corner_radius) |>
                   dplyr::distinct() |>
                   dplyr::arrange(.data = _,
                                  dplyr::desc(radius)) |>
                   dplyr::mutate(.data = _,
                                 special_type = dplyr::case_when(radius == 0 ~ "line-cap",
                                                                 .default = NA),
                                 radius = dplyr::case_when(radius == 0 ~ line_width / 4,
                                                           .default = radius),
                                 # This is fucking stupid because I couldn't figure out what was
                                 # going wrong with cumsum()
                                 # It figures out the number of edging and line widths to displace
                                 # by
                                 previous_edgings = cumsum(as.numeric(type == "edging")),
                                 previous_edgings = dplyr::case_when(type == "edging" ~ previous_edgings - 0.5,
                                                                     type == "line" ~ previous_edgings),
                                 # previous_edgings = dplyr::case_when(dplyr::row_number() > 1 ~ previous_edgings - 0.5,
                                 #                                     .default = previous_edgings),
                                 # previous_edgings = dplyr::case_when(type == "edging" & previous_edgings == 0 ~ 0.5,
                                 #                                     .default = previous_edgings),
                                 previous_lines = cumsum(as.numeric(type == "line")),
                                 previous_lines = dplyr::case_when(type == "line" ~ previous_lines - 0.5,
                                                                   type == "edging" ~ previous_lines),
                                 # previous_lines = dplyr::case_when(type == "line" & previous_lines == 0 ~ 0.5,
                                 #                                     .default = previous_lines),
                                 displacement = dplyr::case_when(is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin,
                                                                 !is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin - line_width / 4))
                 
                 cap_radii_df$previous_edging_pairs <- sapply(X = seq_len(nrow(cap_radii_df)),
                                                              cap_radii_df = cap_radii_df,
                                                              FUN = function(X, cap_radii_df){
                                                                if (X %in% c(1, nrow(cap_radii_df))) {
                                                                  FALSE
                                                                } else {
                                                                  if (cap_radii_df$type[X - 1] == cap_radii_df$type[X]) {
                                                                    TRUE
                                                                  } else {
                                                                    FALSE
                                                                  }
                                                                }
                                                              })
                 
                 # dplyr::glimpse(cap_radii_df)
                 
                 cap_radii_df <- dplyr::mutate(.data = cap_radii_df,
                                               previous_edging_pairs_count = cumsum(previous_edging_pairs),
                                               previous_edgings = previous_edgings - 0.5 * previous_edging_pairs_count,
                                               displacement = dplyr::case_when(is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin,
                                                                               !is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin - line_width / 4))
                 
                 ###### Arcs ---------------------------------------------------
                 if (input$arc_vertex_count == 0) {
                   base_lines <- list(line = list(top = dplyr::filter(.data = cap_radii_df,
                                                                      type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 0.5 - radius,
                                                                  y_start = 1 - displacement,
                                                                  x_end = 0.5 + radius,
                                                                  y_end = 1 - displacement,
                                                                  x_origin = 0.5 - radius,
                                                                  y_origin = 0,
                                                                  x_terminus = 0.5 + radius,
                                                                  y_terminus = 0,
                                                                  radius_x = radius,
                                                                  radius_y = radius),
                                                  bottom = dplyr::filter(.data = cap_radii_df,
                                                                         type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 0.5 + radius,
                                                                  y_start = displacement,
                                                                  x_end = 0.5 - radius,
                                                                  y_end = displacement,
                                                                  x_origin = 0.5 + radius,
                                                                  y_origin = 1,
                                                                  x_terminus = 0.5 - radius,
                                                                  y_terminus = 1,
                                                                  radius_x = radius,
                                                                  radius_y = radius),
                                                  left = dplyr::filter(.data = cap_radii_df,
                                                                       type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 1 - displacement,
                                                                  y_start = 0.5 + radius,
                                                                  x_end = 1 - displacement,
                                                                  y_end = 0.5 - radius,
                                                                  x_origin = 0,
                                                                  y_origin = 0.5 + radius,
                                                                  x_terminus = 0,
                                                                  y_terminus = 0.5 - radius,
                                                                  radius_x = radius,
                                                                  radius_y = radius),
                                                  right = dplyr::filter(.data = cap_radii_df,
                                                                        type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = displacement,
                                                                  y_start = 0.5 - radius,
                                                                  x_end = displacement,
                                                                  y_end = 0.5 + radius,
                                                                  x_origin = 1,
                                                                  y_origin = 0.5 - radius,
                                                                  x_terminus = 1,
                                                                  y_terminus = 0.5 + radius,
                                                                  radius_x = radius,
                                                                  radius_y = radius),
                                                  top_left = dplyr::filter(.data = radii_df,
                                                                           type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 0,
                                                                  y_start = corner_radius,
                                                                  x_end = corner_radius,
                                                                  y_end = 0,
                                                                  x_origin = NA,
                                                                  y_origin = NA,
                                                                  x_terminus = NA,
                                                                  y_terminus = NA,
                                                                  radius_x = corner_radius,
                                                                  radius_y = corner_radius),
                                                  bottom_left = dplyr::filter(.data = radii_df,
                                                                              type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = corner_radius,
                                                                  y_start = 1,
                                                                  x_end = 0,
                                                                  y_end = 1 - corner_radius,
                                                                  x_origin = NA,
                                                                  y_origin = NA,
                                                                  x_terminus = NA,
                                                                  y_terminus = NA,
                                                                  radius_x = corner_radius,
                                                                  radius_y = corner_radius),
                                                  top_right = dplyr::filter(.data = radii_df,
                                                                            type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 1- corner_radius,
                                                                  y_start = 0,
                                                                  x_end = 1,
                                                                  y_end = corner_radius,
                                                                  x_origin = NA,
                                                                  y_origin = NA,
                                                                  x_terminus = NA,
                                                                  y_terminus = NA,
                                                                  radius_x = corner_radius,
                                                                  radius_y = corner_radius),
                                                  bottom_right = dplyr::filter(.data = radii_df,
                                                                               type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 1,
                                                                  y_start = 1 - corner_radius,
                                                                  x_end = 1 - corner_radius,
                                                                  y_end = 1,
                                                                  x_origin = NA,
                                                                  y_origin = NA,
                                                                  x_terminus = NA,
                                                                  y_terminus = NA,
                                                                  radius_x = corner_radius,
                                                                  radius_y = corner_radius),
                                                  top_bottom = dplyr::filter(.data = radii_df,
                                                                             type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = corner_radius,
                                                                  y_start = 0,
                                                                  x_end = corner_radius,
                                                                  y_end = 1,
                                                                  x_origin = NA,
                                                                  y_origin = NA,
                                                                  x_terminus = NA,
                                                                  y_terminus = NA,
                                                                  radius_x = NA,
                                                                  radius_y = NA),
                                                  left_right = dplyr::filter(.data = radii_df,
                                                                             type == "line") |>
                                                    dplyr::mutate(.data = _,
                                                                  x_start = 0,
                                                                  y_start = corner_radius,
                                                                  x_end = 1,
                                                                  y_end = corner_radius,
                                                                  x_origin = NA,
                                                                  y_origin = NA,
                                                                  x_terminus = NA,
                                                                  y_terminus = NA,
                                                                  radius_x = NA,
                                                                  radius_y = NA)),
                                      edging = list(top = dplyr::filter(.data = cap_radii_df,
                                                                        type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 0.5 - radius,
                                                                    y_start = 1 - displacement,
                                                                    x_end = 0.5 + radius,
                                                                    y_end = 1 - displacement,
                                                                    x_origin = 0.5 - radius,
                                                                    y_origin = 0,
                                                                    x_terminus = 0.5 + radius,
                                                                    y_terminus = 0,
                                                                    radius_x = radius,
                                                                    radius_y = radius),
                                                    bottom = dplyr::filter(.data = cap_radii_df,
                                                                           type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 0.5 + radius,
                                                                    y_start = displacement,
                                                                    x_end = 0.5 - radius,
                                                                    y_end = displacement,
                                                                    x_origin = 0.5 + radius,
                                                                    y_origin = 1,
                                                                    x_terminus = 0.5 - radius,
                                                                    y_terminus = 1,
                                                                    radius_x = radius,
                                                                    radius_y = radius),
                                                    left = dplyr::filter(.data = cap_radii_df,
                                                                         type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 1 - displacement,
                                                                    y_start = 0.5 + radius,
                                                                    x_end = 1 - displacement,
                                                                    y_end = 0.5 - radius,
                                                                    x_origin = 0,
                                                                    y_origin = 0.5 + radius,
                                                                    x_terminus = 0,
                                                                    y_terminus = 0.5 - radius,
                                                                    radius_x = radius,
                                                                    radius_y = radius),
                                                    right = dplyr::filter(.data = cap_radii_df,
                                                                          type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = displacement,
                                                                    y_start = 0.5 - radius,
                                                                    x_end = displacement,
                                                                    y_end = 0.5 + radius,
                                                                    x_origin = 1,
                                                                    y_origin = 0.5 - radius,
                                                                    x_terminus = 1,
                                                                    y_terminus = 0.5 + radius,
                                                                    radius_x = radius,
                                                                    radius_y = radius),
                                                    top_left = dplyr::filter(.data = radii_df,
                                                                             type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 0,
                                                                    y_start = corner_radius,
                                                                    x_end = corner_radius,
                                                                    y_end = 0,
                                                                    x_origin = NA,
                                                                    y_origin = NA,
                                                                    x_terminus = NA,
                                                                    y_terminus = NA,
                                                                    radius_x = corner_radius,
                                                                    radius_y = corner_radius),
                                                    bottom_left = dplyr::filter(.data = radii_df,
                                                                                type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = corner_radius,
                                                                    y_start = 1,
                                                                    x_end = 0,
                                                                    y_end = 1 - corner_radius,
                                                                    x_origin = NA,
                                                                    y_origin = NA,
                                                                    x_terminus = NA,
                                                                    y_terminus = NA,
                                                                    radius_x = corner_radius,
                                                                    radius_y = corner_radius),
                                                    top_right = dplyr::filter(.data = radii_df,
                                                                              type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 1- corner_radius,
                                                                    y_start = 0,
                                                                    x_end = 1,
                                                                    y_end = corner_radius,
                                                                    x_origin = NA,
                                                                    y_origin = NA,
                                                                    x_terminus = NA,
                                                                    y_terminus = NA,
                                                                    radius_x = corner_radius,
                                                                    radius_y = corner_radius),
                                                    bottom_right = dplyr::filter(.data = radii_df,
                                                                                 type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 1,
                                                                    y_start = 1 - corner_radius,
                                                                    x_end = 1 - corner_radius,
                                                                    y_end = 1,
                                                                    x_origin = NA,
                                                                    y_origin = NA,
                                                                    x_terminus = NA,
                                                                    y_terminus = NA,
                                                                    radius_x = corner_radius,
                                                                    radius_y = corner_radius),
                                                    top_bottom = dplyr::filter(.data = radii_df,
                                                                               type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = corner_radius,
                                                                    y_start = 0,
                                                                    x_end = corner_radius,
                                                                    y_end = 1,
                                                                    x_origin = NA,
                                                                    y_origin = NA,
                                                                    x_terminus = NA,
                                                                    y_terminus = NA,
                                                                    radius_x = NA,
                                                                    radius_y = NA),
                                                    left_right = dplyr::filter(.data = radii_df,
                                                                               type == "edging") |>
                                                      dplyr::mutate(.data = _,
                                                                    x_start = 0,
                                                                    y_start = corner_radius,
                                                                    x_end = 1,
                                                                    y_end = corner_radius,
                                                                    x_origin = NA,
                                                                    y_origin = NA,
                                                                    x_terminus = NA,
                                                                    y_terminus = NA,
                                                                    radius_x = NA,
                                                                    radius_y = NA)))
                 }
                 ###### Segments -----------------------------------------------
                 if (input$arc_vertex_count > 0) {
                   
                   # There will be a vertex at each join between tiles and then
                   # the number of vertices requested will be put between each.
                   vertex_count <- 4 * (1 + input$arc_vertex_count)
                   
                   # The caps are all the same!!!!
                   # The connecting ones (e.g., top_left) differ in format from
                   # the arc ones above.
                   base_lines <- lapply(X = c("line",
                                              "edging"),
                                        radii_df = radii_df,
                                        cap_radii_df = cap_radii_df,
                                        FUN = function(X, radii_df, cap_radii_df){
                                          list(top = dplyr::filter(.data = cap_radii_df,
                                                                   type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               x_start = 0.5 - radius,
                                                               y_start = 1 - displacement,
                                                               x_end = 0.5 + radius,
                                                               y_end = 1 - displacement,
                                                               x_origin = 0.5 - radius,
                                                               y_origin = 0,
                                                               x_terminus = 0.5 + radius,
                                                               y_terminus = 0,
                                                               radius_x = radius,
                                                               radius_y = radius),
                                               bottom = dplyr::filter(.data = cap_radii_df,
                                                                      type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               x_start = 0.5 + radius,
                                                               y_start = displacement,
                                                               x_end = 0.5 - radius,
                                                               y_end = displacement,
                                                               x_origin = 0.5 + radius,
                                                               y_origin = 1,
                                                               x_terminus = 0.5 - radius,
                                                               y_terminus = 1,
                                                               radius_x = radius,
                                                               radius_y = radius),
                                               left = dplyr::filter(.data = cap_radii_df,
                                                                    type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               x_start = 1 - displacement,
                                                               y_start = 0.5 + radius,
                                                               x_end = 1 - displacement,
                                                               y_end = 0.5 - radius,
                                                               x_origin = 0,
                                                               y_origin = 0.5 + radius,
                                                               x_terminus = 0,
                                                               y_terminus = 0.5 - radius,
                                                               radius_x = radius,
                                                               radius_y = radius),
                                               right = dplyr::filter(.data = cap_radii_df,
                                                                     type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               x_start = displacement,
                                                               y_start = 0.5 - radius,
                                                               x_end = displacement,
                                                               y_end = 0.5 + radius,
                                                               x_origin = 1,
                                                               y_origin = 0.5 - radius,
                                                               x_terminus = 1,
                                                               y_terminus = 0.5 + radius,
                                                               radius_x = radius,
                                                               radius_y = radius),
                                               top_left = dplyr::filter(.data = radii_df,
                                                                        type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               line_id = dplyr::row_number()) |>
                                                 apply(X = _,
                                                       MARGIN = 1,
                                                       vertex_count = vertex_count,
                                                       FUN = function(X, vertex_count){
                                                         regular_polygon(vertex_count = vertex_count,
                                                                         radius = as.numeric(X[["corner_radius"]]),
                                                                         center_x = 0,
                                                                         center_y = 0,
                                                                         # output_type = "coords")[seq(from = 1 + (vertex_count / 4),
                                                                         #                             to = 1 + (vertex_count / 4) * 2),] |>
                                                                         output_type = "coords")[seq(from = 1,
                                                                                                     to = 1 + (vertex_count / 4)),] |>
                                                           dplyr::mutate(.data = _,
                                                                         line_id = X[["line_id"]]) |>
                                                           # Stupid, but for whatever reason pasting the values together and then
                                                           # splitting them is the easiest way to get a vector of values in the
                                                           # variable
                                                           dplyr::summarize(.data = _,
                                                                            .by = tidyselect::all_of(x = "line_id"),
                                                                            x_coords = paste(x,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"),
                                                                            y_coords = paste(y,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"))
                                                       }) |>
                                                 dplyr::bind_rows(),
                                               bottom_left = dplyr::filter(.data = radii_df,
                                                                           type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               line_id = dplyr::row_number()) |>
                                                 apply(X = _,
                                                       MARGIN = 1,
                                                       vertex_count = vertex_count,
                                                       FUN = function(X, vertex_count){
                                                         vertices_df <- regular_polygon(vertex_count = vertex_count,
                                                                                        radius = as.numeric(X[["corner_radius"]]),
                                                                                        center_x = 0,
                                                                                        center_y = 1,
                                                                                        # output_type = "coords")[seq(from = 1 + (vertex_count / 4),
                                                                                        #                             to = 1),] |>
                                                                                        output_type = "coords")[c(1,
                                                                                                                  seq(from = vertex_count,
                                                                                                                    to = 1 + (vertex_count / 4) * 3)),] |>
                                                           dplyr::mutate(.data = _,
                                                                         line_id = X[["line_id"]]) |>
                                                           # Stupid, but for whatever reason pasting the values together and then
                                                           # splitting them is the easiest way to get a vector of values in the
                                                           # variable
                                                           dplyr::summarize(.data = _,
                                                                            .by = tidyselect::all_of(x = "line_id"),
                                                                            x_coords = paste(x,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"),
                                                                            y_coords = paste(y,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"))
                                                       }) |>
                                                 dplyr::bind_rows(),
                                               top_right = dplyr::filter(.data = radii_df,
                                                                         type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               line_id = dplyr::row_number()) |>
                                                 apply(X = _,
                                                       MARGIN = 1,
                                                       vertex_count = vertex_count,
                                                       FUN = function(X, vertex_count){
                                                         vertices_df <- regular_polygon(vertex_count = vertex_count,
                                                                                        radius = as.numeric(X[["corner_radius"]]),
                                                                                        center_x = 1,
                                                                                        center_y = 0,
                                                                                        # output_type = "coords")[seq(from = 1 + (vertex_count / 4) * 3,
                                                                                        #                             to = 1 + (vertex_count / 4) * 2),] |>
                                                                                        output_type = "coords")[seq(from = 1 + (vertex_count / 4) * 2,
                                                                                                                    to = 1 + (vertex_count / 4)),] |>
                                                           dplyr::mutate(.data = _,
                                                                         line_id = X[["line_id"]]) |>
                                                           # Stupid, but for whatever reason pasting the values together and then
                                                           # splitting them is the easiest way to get a vector of values in the
                                                           # variable
                                                           dplyr::summarize(.data = _,
                                                                            .by = tidyselect::all_of(x = "line_id"),
                                                                            x_coords = paste(x,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"),
                                                                            y_coords = paste(y,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"))
                                                       }) |>
                                                 dplyr::bind_rows(),
                                               bottom_right = dplyr::filter(.data = radii_df,
                                                                            type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               line_id = dplyr::row_number()) |>
                                                 apply(X = _,
                                                       MARGIN = 1,
                                                       vertex_count = vertex_count,
                                                       FUN = function(X, vertex_count){
                                                         vertices_df <- regular_polygon(vertex_count = vertex_count,
                                                                                        radius = as.numeric(X[["corner_radius"]]),
                                                                                        center_x = 1,
                                                                                        center_y = 1,
                                                                                        # output_type = "coords")[c(seq(from = 1 + (vertex_count / 4) * 3,
                                                                                        #                               to = (vertex_count / 4) * 4),
                                                                                        #                           1),] |>
                                                                                        output_type = "coords")[seq(from = 1 + (vertex_count / 4) * 2,
                                                                                                                      to = 1 + (vertex_count / 4) * 3),] |>
                                                           dplyr::mutate(.data = _,
                                                                         line_id = X[["line_id"]]) |>
                                                           # Stupid, but for whatever reason pasting the values together and then
                                                           # splitting them is the easiest way to get a vector of values in the
                                                           # variable
                                                           dplyr::summarize(.data = _,
                                                                            .by = tidyselect::all_of(x = "line_id"),
                                                                            x_coords = paste(x,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"),
                                                                            y_coords = paste(y,
                                                                                             collapse = "_") |>
                                                                              stringr::str_split(string = _,
                                                                                                 pattern = "_"))
                                                       }) |>
                                                 dplyr::bind_rows(),
                                               top_bottom = dplyr::filter(.data = radii_df,
                                                                          type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               x_start = corner_radius,
                                                               y_start = 0,
                                                               x_end = corner_radius,
                                                               y_end = 1,
                                                               x_origin = NA,
                                                               y_origin = NA,
                                                               x_terminus = NA,
                                                               y_terminus = NA,
                                                               radius_x = NA,
                                                               radius_y = NA),
                                               left_right = dplyr::filter(.data = radii_df,
                                                                          type == X) |>
                                                 dplyr::mutate(.data = _,
                                                               x_start = 0,
                                                               y_start = corner_radius,
                                                               x_end = 1,
                                                               y_end = corner_radius,
                                                               x_origin = NA,
                                                               y_origin = NA,
                                                               x_terminus = NA,
                                                               y_terminus = NA,
                                                               radius_x = NA,
                                                               radius_y = NA))
                                        }) |>
                     setNames(object = _,
                              nm = c("line",
                                     "edging"))
                   
                   # base_lines <- list(line = list(top = dplyr::filter(.data = cap_radii_df,
                   #                                                    type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                x_start = 0.5 - radius,
                   #                                                y_start = 1 - displacement,
                   #                                                x_end = 0.5 + radius,
                   #                                                y_end = 1 - displacement,
                   #                                                x_origin = 0.5 - radius,
                   #                                                y_origin = 0,
                   #                                                x_terminus = 0.5 + radius,
                   #                                                y_terminus = 0,
                   #                                                radius_x = radius,
                   #                                                radius_y = radius),
                   #                                bottom = dplyr::filter(.data = cap_radii_df,
                   #                                                       type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                x_start = 0.5 + radius,
                   #                                                y_start = displacement,
                   #                                                x_end = 0.5 - radius,
                   #                                                y_end = displacement,
                   #                                                x_origin = 0.5 + radius,
                   #                                                y_origin = 1,
                   #                                                x_terminus = 0.5 - radius,
                   #                                                y_terminus = 1,
                   #                                                radius_x = radius,
                   #                                                radius_y = radius),
                   #                                left = dplyr::filter(.data = cap_radii_df,
                   #                                                     type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                x_start = 1 - displacement,
                   #                                                y_start = 0.5 + radius,
                   #                                                x_end = 1 - displacement,
                   #                                                y_end = 0.5 - radius,
                   #                                                x_origin = 0,
                   #                                                y_origin = 0.5 + radius,
                   #                                                x_terminus = 0,
                   #                                                y_terminus = 0.5 - radius,
                   #                                                radius_x = radius,
                   #                                                radius_y = radius),
                   #                                right = dplyr::filter(.data = cap_radii_df,
                   #                                                      type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                x_start = displacement,
                   #                                                y_start = 0.5 - radius,
                   #                                                x_end = displacement,
                   #                                                y_end = 0.5 + radius,
                   #                                                x_origin = 1,
                   #                                                y_origin = 0.5 - radius,
                   #                                                x_terminus = 1,
                   #                                                y_terminus = 0.5 + radius,
                   #                                                radius_x = radius,
                   #                                                radius_y = radius),
                   #                                top_left = dplyr::filter(.data = radii_df,
                   #                                                         type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                line_id = dplyr::row_number()) |>
                   #                                  apply(X = _,
                   #                                        MARGIN = 1,
                   #                                        vertex_count = vertex_count,
                   #                                        FUN = function(X, vertex_count){
                   #                                          regular_polygon(vertex_count = vertex_count,
                   #                                                          radius = as.numeric(X[["corner_radius"]]),
                   #                                                          center_x = 0,
                   #                                                          center_y = 0,
                   #                                                          output_type = "coords")[seq(from = 1 + (vertex_count / 4),
                   #                                                                                      to = 1 + (vertex_count / 4) * 2),] |>
                   #                                            dplyr::mutate(.data = _,
                   #                                                          line_id = X[["line_id"]]) |>
                   #                                            # Stupid, but for whatever reason pasting the values together and then
                   #                                            # splitting them is the easiest way to get a vector of values in the
                   #                                            # variable
                   #                                            dplyr::summarize(.data = _,
                   #                                                             .by = tidyselect::all_of(x = "line_id"),
                   #                                                             x_coords = paste(x,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"),
                   #                                                             y_coords = paste(y,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"))
                   #                                        }) |>
                   #                                  dplyr::bind_rows(),
                   #                                bottom_left = dplyr::filter(.data = radii_df,
                   #                                                            type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                line_id = dplyr::row_number()) |>
                   #                                  apply(X = _,
                   #                                        MARGIN = 1,
                   #                                        vertex_count = vertex_count,
                   #                                        FUN = function(X, vertex_count){
                   #                                          vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                         radius = as.numeric(X[["corner_radius"]]),
                   #                                                                         center_x = 0,
                   #                                                                         center_y = 1,
                   #                                                                         output_type = "coords")[seq(from = 1 + (vertex_count / 4),
                   #                                                                                                     to = 1),] |>
                   #                                            dplyr::mutate(.data = _,
                   #                                                          line_id = X[["line_id"]]) |>
                   #                                            # Stupid, but for whatever reason pasting the values together and then
                   #                                            # splitting them is the easiest way to get a vector of values in the
                   #                                            # variable
                   #                                            dplyr::summarize(.data = _,
                   #                                                             .by = tidyselect::all_of(x = "line_id"),
                   #                                                             x_coords = paste(x,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"),
                   #                                                             y_coords = paste(y,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"))
                   #                                        }) |>
                   #                                  dplyr::bind_rows(),
                   #                                top_right = dplyr::filter(.data = radii_df,
                   #                                                          type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                line_id = dplyr::row_number()) |>
                   #                                  apply(X = _,
                   #                                        MARGIN = 1,
                   #                                        vertex_count = vertex_count,
                   #                                        FUN = function(X, vertex_count){
                   #                                          vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                         radius = as.numeric(X[["corner_radius"]]),
                   #                                                                         center_x = 1,
                   #                                                                         center_y = 0,
                   #                                                                         output_type = "coords")[seq(from = 1 + (vertex_count / 4) * 3,
                   #                                                                                                     to = 1 + (vertex_count / 4) * 2),] |>
                   #                                            dplyr::mutate(.data = _,
                   #                                                          line_id = X[["line_id"]]) |>
                   #                                            # Stupid, but for whatever reason pasting the values together and then
                   #                                            # splitting them is the easiest way to get a vector of values in the
                   #                                            # variable
                   #                                            dplyr::summarize(.data = _,
                   #                                                             .by = tidyselect::all_of(x = "line_id"),
                   #                                                             x_coords = paste(x,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"),
                   #                                                             y_coords = paste(y,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"))
                   #                                        }) |>
                   #                                  dplyr::bind_rows(),
                   #                                bottom_right = dplyr::filter(.data = radii_df,
                   #                                                             type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                line_id = dplyr::row_number()) |>
                   #                                  apply(X = _,
                   #                                        MARGIN = 1,
                   #                                        vertex_count = vertex_count,
                   #                                        FUN = function(X, vertex_count){
                   #                                          vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                         radius = as.numeric(X[["corner_radius"]]),
                   #                                                                         center_x = 1,
                   #                                                                         center_y = 1,
                   #                                                                         output_type = "coords")[c(seq(from = 1 + (vertex_count / 4) * 3,
                   #                                                                                                       to = (vertex_count / 4) * 4),
                   #                                                                                                   1),] |>
                   #                                            dplyr::mutate(.data = _,
                   #                                                          line_id = X[["line_id"]]) |>
                   #                                            # Stupid, but for whatever reason pasting the values together and then
                   #                                            # splitting them is the easiest way to get a vector of values in the
                   #                                            # variable
                   #                                            dplyr::summarize(.data = _,
                   #                                                             .by = tidyselect::all_of(x = "line_id"),
                   #                                                             x_coords = paste(x,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"),
                   #                                                             y_coords = paste(y,
                   #                                                                              collapse = "_") |>
                   #                                                               stringr::str_split(string = _,
                   #                                                                                  pattern = "_"))
                   #                                        }) |>
                   #                                  dplyr::bind_rows(),
                   #                                top_bottom = dplyr::filter(.data = radii_df,
                   #                                                           type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                x_start = corner_radius,
                   #                                                y_start = 0,
                   #                                                x_end = corner_radius,
                   #                                                y_end = 1,
                   #                                                x_origin = NA,
                   #                                                y_origin = NA,
                   #                                                x_terminus = NA,
                   #                                                y_terminus = NA,
                   #                                                radius_x = NA,
                   #                                                radius_y = NA),
                   #                                left_right = dplyr::filter(.data = radii_df,
                   #                                                           type == "line") |>
                   #                                  dplyr::mutate(.data = _,
                   #                                                x_start = 0,
                   #                                                y_start = corner_radius,
                   #                                                x_end = 1,
                   #                                                y_end = corner_radius,
                   #                                                x_origin = NA,
                   #                                                y_origin = NA,
                   #                                                x_terminus = NA,
                   #                                                y_terminus = NA,
                   #                                                radius_x = NA,
                   #                                                radius_y = NA)),
                   #                    edging = list(top = dplyr::filter(.data = cap_radii_df,
                   #                                                      type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  x_start = 0.5 - radius,
                   #                                                  y_start = 1 - displacement,
                   #                                                  x_end = 0.5 + radius,
                   #                                                  y_end = 1 - displacement,
                   #                                                  x_origin = 0.5 - radius,
                   #                                                  y_origin = 0,
                   #                                                  x_terminus = 0.5 + radius,
                   #                                                  y_terminus = 0,
                   #                                                  radius_x = radius,
                   #                                                  radius_y = radius),
                   #                                  bottom = dplyr::filter(.data = cap_radii_df,
                   #                                                         type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  x_start = 0.5 + radius,
                   #                                                  y_start = displacement,
                   #                                                  x_end = 0.5 - radius,
                   #                                                  y_end = displacement,
                   #                                                  x_origin = 0.5 + radius,
                   #                                                  y_origin = 1,
                   #                                                  x_terminus = 0.5 - radius,
                   #                                                  y_terminus = 1,
                   #                                                  radius_x = radius,
                   #                                                  radius_y = radius),
                   #                                  left = dplyr::filter(.data = cap_radii_df,
                   #                                                       type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  x_start = 1 - displacement,
                   #                                                  y_start = 0.5 + radius,
                   #                                                  x_end = 1 - displacement,
                   #                                                  y_end = 0.5 - radius,
                   #                                                  x_origin = 0,
                   #                                                  y_origin = 0.5 + radius,
                   #                                                  x_terminus = 0,
                   #                                                  y_terminus = 0.5 - radius,
                   #                                                  radius_x = radius,
                   #                                                  radius_y = radius),
                   #                                  right = dplyr::filter(.data = cap_radii_df,
                   #                                                        type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  x_start = displacement,
                   #                                                  y_start = 0.5 - radius,
                   #                                                  x_end = displacement,
                   #                                                  y_end = 0.5 + radius,
                   #                                                  x_origin = 1,
                   #                                                  y_origin = 0.5 - radius,
                   #                                                  x_terminus = 1,
                   #                                                  y_terminus = 0.5 + radius,
                   #                                                  radius_x = radius,
                   #                                                  radius_y = radius),
                   #                                  top_left = dplyr::filter(.data = radii_df,
                   #                                                           type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  line_id = dplyr::row_number()) |>
                   #                                    apply(X = _,
                   #                                          MARGIN = 1,
                   #                                          vertex_count = vertex_count,
                   #                                          FUN = function(X, vertex_count){
                   #                                            vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                           radius = as.numeric(X[["corner_radius"]]),
                   #                                                                           center_x = 0,
                   #                                                                           center_y = 0,
                   #                                                                           output_type = "coords")[seq(from = 1 + (vertex_count / 4),
                   #                                                                                                       to = 1 + (vertex_count / 4) * 2),] |>
                   #                                              dplyr::mutate(.data = _,
                   #                                                            line_id = X[["line_id"]]) |>
                   #                                              # Stupid, but for whatever reason pasting the values together and then
                   #                                              # splitting them is the easiest way to get a vector of values in the
                   #                                              # variable
                   #                                              dplyr::summarize(.data = _,
                   #                                                               .by = tidyselect::all_of(x = "line_id"),
                   #                                                               x_coords = paste(x,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"),
                   #                                                               y_coords = paste(y,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"))
                   #                                          }) |>
                   #                                    dplyr::bind_rows(),
                   #                                  bottom_left = dplyr::filter(.data = radii_df,
                   #                                                              type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  line_id = dplyr::row_number()) |>
                   #                                    apply(X = _,
                   #                                          MARGIN = 1,
                   #                                          vertex_count = vertex_count,
                   #                                          FUN = function(X, vertex_count){
                   #                                            vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                           radius = as.numeric(X[["corner_radius"]]),
                   #                                                                           center_x = 0,
                   #                                                                           center_y = 1,
                   #                                                                           output_type = "coords")[seq(from = 1 + (vertex_count / 4),
                   #                                                                                                       to = 1),] |>
                   #                                              dplyr::mutate(.data = _,
                   #                                                            line_id = X[["line_id"]]) |>
                   #                                              # Stupid, but for whatever reason pasting the values together and then
                   #                                              # splitting them is the easiest way to get a vector of values in the
                   #                                              # variable
                   #                                              dplyr::summarize(.data = _,
                   #                                                               .by = tidyselect::all_of(x = "line_id"),
                   #                                                               x_coords = paste(x,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"),
                   #                                                               y_coords = paste(y,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"))
                   #                                          }) |>
                   #                                    dplyr::bind_rows(),
                   #                                  top_right = dplyr::filter(.data = radii_df,
                   #                                                            type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  line_id = dplyr::row_number()) |>
                   #                                    apply(X = _,
                   #                                          MARGIN = 1,
                   #                                          vertex_count = vertex_count,
                   #                                          FUN = function(X, vertex_count){
                   #                                            vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                           radius = as.numeric(X[["corner_radius"]]),
                   #                                                                           center_x = 1,
                   #                                                                           center_y = 0,
                   #                                                                           output_type = "coords")[seq(from = 1 + (vertex_count / 4) * 3,
                   #                                                                                                       to = 1 + (vertex_count / 4) * 2),] |>
                   #                                              dplyr::mutate(.data = _,
                   #                                                            line_id = X[["line_id"]]) |>
                   #                                              # Stupid, but for whatever reason pasting the values together and then
                   #                                              # splitting them is the easiest way to get a vector of values in the
                   #                                              # variable
                   #                                              dplyr::summarize(.data = _,
                   #                                                               .by = tidyselect::all_of(x = "line_id"),
                   #                                                               x_coords = paste(x,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"),
                   #                                                               y_coords = paste(y,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"))
                   #                                          }) |>
                   #                                    dplyr::bind_rows(),
                   #                                  bottom_right = dplyr::filter(.data = radii_df,
                   #                                                               type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  line_id = dplyr::row_number()) |>
                   #                                    apply(X = _,
                   #                                          MARGIN = 1,
                   #                                          vertex_count = vertex_count,
                   #                                          FUN = function(X, vertex_count){
                   #                                            vertices_df <- regular_polygon(vertex_count = vertex_count,
                   #                                                                           radius = as.numeric(X[["corner_radius"]]),
                   #                                                                           center_x = 1,
                   #                                                                           center_y = 1,
                   #                                                                           output_type = "coords")[c(seq(from = 1 + (vertex_count / 4) * 3,
                   #                                                                                                         to = (vertex_count / 4) * 4),
                   #                                                                                                     1),] |>
                   #                                              dplyr::mutate(.data = _,
                   #                                                            line_id = X[["line_id"]]) |>
                   #                                              # Stupid, but for whatever reason pasting the values together and then
                   #                                              # splitting them is the easiest way to get a vector of values in the
                   #                                              # variable
                   #                                              dplyr::summarize(.data = _,
                   #                                                               .by = tidyselect::all_of(x = "line_id"),
                   #                                                               x_coords = paste(x,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"),
                   #                                                               y_coords = paste(y,
                   #                                                                                collapse = "_") |>
                   #                                                                 stringr::str_split(string = _,
                   #                                                                                    pattern = "_"))
                   #                                          }) |>
                   #                                    dplyr::bind_rows(),
                   #                                  top_bottom = dplyr::filter(.data = radii_df,
                   #                                                             type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  x_start = corner_radius,
                   #                                                  y_start = 0,
                   #                                                  x_end = corner_radius,
                   #                                                  y_end = 1,
                   #                                                  x_origin = NA,
                   #                                                  y_origin = NA,
                   #                                                  x_terminus = NA,
                   #                                                  y_terminus = NA,
                   #                                                  radius_x = NA,
                   #                                                  radius_y = NA),
                   #                                  left_right = dplyr::filter(.data = radii_df,
                   #                                                             type == "edging") |>
                   #                                    dplyr::mutate(.data = _,
                   #                                                  x_start = 0,
                   #                                                  y_start = corner_radius,
                   #                                                  x_end = 1,
                   #                                                  y_end = corner_radius,
                   #                                                  x_origin = NA,
                   #                                                  y_origin = NA,
                   #                                                  x_terminus = NA,
                   #                                                  y_terminus = NA,
                   #                                                  radius_x = NA,
                   #                                                  radius_y = NA)))
                 }
                 
                 #### GENERATION ################################################################
                 ##### Make the base matrix -----------------------------------------------------
                 message("Making base matrix")
                 # This is a stub for eventually implementing intertwining palettes
                 id_value <- 1
                 
                 # input <- list(n_rows = 3,
                 #               n_cols = 4,
                 #               # row_1 = c(TRUE, TRUE, TRUE, TRUE),
                 #               # row_2 = c(FALSE, TRUE, TRUE, TRUE),
                 #               # row_3 = c(TRUE, FALSE, FALSE, FALSE),
                 #               row_1 = c(1, 2, 3, 4),
                 #               row_2 = c(3, 4),
                 #               row_3 = c(1, 2, 3))
                 # 
                 # base_vector <- lapply(X = paste0("row_", seq_len(input$n_rows)),
                 #                       n_cols = input$n_cols,
                 #                       FUN = function(X, n_cols){
                 #                         output <- rep(FALSE,
                 #                                       times = n_cols)
                 #                         output[as.numeric(input[[X]])] <- TRUE
                 #                         output
                 #                       }) |>
                 #   unlist()
                 
                 
                 # base_vector <- lapply(X = seq_len(input$n_cols),
                 #                       row_ids = paste0("row_", seq_len(input$n_rows)),
                 #                       n_cols = input$n_cols,
                 #                       n_rows = input$n_rows,
                 #                       FUN = function(X, row_ids, n_cols, n_rows){
                 #                         true_indices <- sapply(X = row_ids,
                 #                                current_col_index = X,
                 #                                FUN = function(X, current_col_index){
                 #                                  input[[X]][current_col_index]
                 #                                })
                 #                         output <- rep(FALSE,
                 #                                       times = n_cols)
                 #                         output[true_indices] <- true_indices
                 #                         output
                 #                       }) |>
                 #   unlist() |>
                 #   matrix(data = _,
                 #          nrow = input$n_rows) |>
                 #   as.vector()
                 
                 base_vector <- lapply(X = paste0("row_", seq_len(input$n_rows)),
                                       n_cols = input$n_cols,
                                       id_value = id_value,
                                       FUN = function(X, n_cols, id_value){
                                         row_vector <- as.numeric(input[[X]])
                                         # message("row_vector")
                                         # message(class(row_vector))
                                         # message(paste(row_vector,
                                         #               collapse = " "))
                                         output <- rep(x = 0,
                                                       times = n_cols)
                                         # message("base output")
                                         # message(paste(output,
                                         #               collapse = " "))
                                         output[row_vector] <- id_value
                                         # message("modified output")
                                         # message(paste(output,
                                         #               collapse = " "))
                                         output
                                       }) |>
                   do.call(what = rbind,
                           args = _) |>
                   as.vector(x = _)
                 
                 message("Base vector:")
                 message(paste(base_vector,
                               collapse = " "))
                 
                 # base_vector <- rep(x = 1,
                 #                    times = input$n_rows * input$n_cols)
                 # base_vector[trunc(input$n_rows / 2 + (input$n_rows * input$n_cols / 2))] <- id_value
                 
                 if (workspace$buffer) {
                   buffer_indices <- c(seq_len(input$n_rows),
                                       seq_len(input$n_rows) + (input$n_cols - 1) * input$n_rows,
                                       seq_len(input$n_cols) * input$n_rows,
                                       (seq_len(input$n_cols) - 1) * input$n_rows + 1)
                 } else {
                   buffer_indices <- c()
                 }
                 
                 message("Finding adjacent indices")
                 available_adjacent_indices <- sapply(X = which(base_vector == id_value),
                                                      base_vector = base_vector,
                                                      n_rows = input$n_rows,
                                                      buffer_indices = buffer_indices,
                                                      FUN = function(X, base_vector, n_rows, buffer_indices){
                                                        indices <- c(left = X - n_rows,
                                                                     right = X + n_rows,
                                                                     top = if (X %% n_rows == 1) {
                                                                       NULL
                                                                     } else {
                                                                       X - 1
                                                                     },
                                                                     bottom = if (X %% n_rows == 0) {
                                                                       NULL
                                                                     } else {
                                                                       X + 1
                                                                     })
                                                        indices <- indices[indices > 0 &
                                                                             indices <= length(base_vector)]
                                                        
                                                        indices <- setdiff(x = indices,
                                                                           y = which(base_vector != 0))
                                                        
                                                        if (length(buffer_indices) > 0) {
                                                          indices <- setdiff(x = indices,
                                                                             y = buffer_indices)
                                                        }
                                                      }) |>
                   as.vector() |>
                   unlist()
                 
                 selected_adjacent_indices <- sample(x = available_adjacent_indices,
                                                     size = max(0,
                                                                length(available_adjacent_indices) - 1),
                                                     replace = FALSE)
                 
                 base_vector[selected_adjacent_indices] <- id_value
                 
                 available_adjacent_indices <- setdiff(x = available_adjacent_indices,
                                                       y = selected_adjacent_indices)
                 
                 tile_count <- input$n_rows * input$n_cols
                 message("Populating base matrix")
                 while (sum(base_vector == id_value) < tile_count & length(available_adjacent_indices) > 0) {
                   available_adjacent_indices <- sapply(X = which(base_vector == id_value),
                                                        base_vector = base_vector,
                                                        n_rows = input$n_rows,
                                                        buffer_indices = buffer_indices,
                                                        FUN = function(X, base_vector, n_rows, buffer_indices){
                                                          indices <- c(left = X - n_rows,
                                                                       right = X + n_rows,
                                                                       top = if (X %% n_rows == 1) {
                                                                         NULL
                                                                       } else {
                                                                         X - 1
                                                                       },
                                                                       bottom = if (X %% n_rows == 0) {
                                                                         NULL
                                                                       } else {
                                                                         X + 1
                                                                       })
                                                          indices <- indices[indices > 0 &
                                                                               indices <= length(base_vector)]
                                                          
                                                          indices <- setdiff(x = indices,
                                                                             y = which(base_vector != 0))
                                                          
                                                          if (length(buffer_indices) > 0) {
                                                            indices <- setdiff(x = indices,
                                                                               y = buffer_indices)
                                                          }
                                                        }) |>
                     as.vector() |>
                     unlist()
                   
                   selected_adjacent_indices <- sample(x = available_adjacent_indices,
                                                       size = min(ceiling(length(available_adjacent_indices) / 2),
                                                                  tile_count - sum(base_vector == id_value)),
                                                       replace = FALSE)
                   
                   base_vector[selected_adjacent_indices] <- id_value
                   base_vector[buffer_indices] <- 0
                   
                   available_adjacent_indices <- setdiff(x = available_adjacent_indices,
                                                         y = selected_adjacent_indices)
                 }
                 
                 message("Base vector:")
                 message(paste(base_vector,
                               collapse = " "))
                 base_matrix <- matrix(data = base_vector,
                                       nrow = input$n_rows)
                 message("Base matrix:")
                 print(base_matrix)
                 workspace$base_matrix <- base_matrix
                 ##### Adjacency ------------------------------------------------------
                 # Figure out which cells are part of the blob that we'll be tiling.
                 # id_value <- 1
                 message("Making adjacency list")
                 adjacency_list <- lapply(X = c(top = "top",
                                                bottom = "bottom",
                                                left = "left",
                                                right = "right"),
                                          id_value = id_value,
                                          base_matrix = base_matrix,
                                          FUN = function(X, id_value, base_matrix){
                                            current_direction <- switch(X,
                                                                        "left" = -1,
                                                                        "right" = 1,
                                                                        "top" = -1,
                                                                        "bottom" = 1)
                                            current_margin <- switch(X,
                                                                     "left" = 1,
                                                                     "right" = 1,
                                                                     "top" = 2,
                                                                     "bottom" = 2)
                                            current_direction_matrix <- apply(X = base_matrix,
                                                                              id_value = id_value,
                                                                              direction = current_direction,
                                                                              MARGIN = current_margin,
                                                                              FUN = function(X, id_value, direction){
                                                                                sapply(X = seq_len(length(X)),
                                                                                       current_values = X,
                                                                                       id_value = id_value,
                                                                                       FUN = function(X, current_values, id_value){
                                                                                         if ((X == 1 & direction < 0) | (X == length(current_values) & direction > 0)) {
                                                                                           FALSE
                                                                                         } else {
                                                                                           all(c(current_values[X],
                                                                                                 current_values[X + direction]) %in% id_value)
                                                                                         }
                                                                                       })
                                                                              })
                                            if (current_margin == 1) {
                                              # t(current_direction_matrix)
                                              t(current_direction_matrix)
                                            } else {
                                              current_direction_matrix
                                            }
                                          })
                 
                 # Any cell with connections is part of the blob!
                 blob_indices <- which(purrr::reduce(.x = adjacency_list,
                                                     .f = `+`) > 0)
                 
                 ##### Identifying blob cell neighbors ------------------------------------------
                 message("Making neighbors list")
                 neighbors_list <- lapply(X = blob_indices,
                                          blob_indices = blob_indices,
                                          n_cols = input$n_cols,
                                          n_rows = input$n_rows,
                                          FUN = function(X, blob_indices, n_cols, n_rows){
                                            output <- c(left = X - n_rows,
                                                        right = X + n_rows,
                                                        top = if (X %% n_rows == 1) {
                                                          -Inf
                                                        } else {
                                                          X - 1
                                                        },
                                                        bottom = if (X %% n_rows == 0) {
                                                          -Inf
                                                        } else {
                                                          X + 1
                                                        })
                                            
                                            output[output %in% setdiff(x = blob_indices,
                                                                       y = X)]
                                          })
                 
                 ##### Setting high-low per cell ------------------------------------------------
                 # Every cell in the blob needs to have high or low assigned to its sides which
                 # will be used to decide which sides connect to which and also to generate the
                 # color gradients.
                 message("Making cell connections list")
                 cell_connections_list <- lapply(X = blob_indices,
                                                 adjacency_list = adjacency_list,
                                                 allow_straights = input$allow_straights,
                                                 respect_margins = workspace$respect_margins,
                                                 FUN = function(X, adjacency_list, allow_straights, respect_margins){
                                                   message(X)
                                                   if (respect_margins) {
                                                     available_connections <- names(adjacency_list)[sapply(X = adjacency_list,
                                                                                                           blob_index = X,
                                                                                                           FUN = function(X, blob_index){
                                                                                                             X[blob_index]
                                                                                                           })]
                                                   } else {
                                                     available_connections <- c("top", "bottom", "left", "right")
                                                   }
                                                   
                                                   
                                                   # available_connections <- factor(x = available_connections,
                                                   #                                 levels = c("top", "bottom", "left", "right"))
                                                   if (length(available_connections) > 1) {
                                                     pair <- sample(x = available_connections,
                                                                    size = 2,
                                                                    replace = FALSE)
                                                     remainder <- setdiff(available_connections,
                                                                          pair)
                                                     
                                                     has_straights <- all(c("top", "bottom") %in% pair) | all(c("left", "right") %in% pair) |
                                                       all(c("top", "bottom") %in% remainder) | all(c("left", "right") %in% remainder)
                                                     
                                                     # Prevent straights from being chosen if possible
                                                     if (!allow_straights) {
                                                       while (has_straights & length(available_connections) > 2) {
                                                         pair <- sample(x = available_connections,
                                                                        size = 2,
                                                                        replace = FALSE)
                                                         remainder <- setdiff(available_connections,
                                                                              pair)
                                                         has_straights <- all(c("top", "bottom") %in% pair) | all(c("left", "right") %in% pair) |
                                                           all(c("top", "bottom") %in% remainder) | all(c("left", "right") %in% remainder)
                                                       }
                                                     }
                                                     
                                                     # We'll allow straights on the margins to
                                                     # avoid ugly snippets
                                                     if (length(available_connections) == 3) {
                                                       if (all(c("top", "bottom") %in% available_connections)) {
                                                         pair <- c("top", "bottom")
                                                       } else {
                                                         pair <- c("left", "right")
                                                       }
                                                       remainder <- setdiff(x = available_connections,
                                                                            y = pair)
                                                     }
                                                     
                                                     
                                                     if (!all(pair %in% c("top", "bottom")) & !all(pair %in% c("left", "right"))) {
                                                       high <- remainder
                                                       
                                                       low <- pair
                                                     } else if (sample(c(TRUE, FALSE), size = 1)) {
                                                       high <- pair
                                                       
                                                       low <- remainder
                                                     } else {
                                                       high <- remainder
                                                       
                                                       low <- pair
                                                     }
                                                     
                                                     high <- factor(x = high,
                                                                    levels = c("top", "bottom", "left", "right"))
                                                     low <- factor(x = low,
                                                                   levels = c("top", "bottom", "left", "right"))
                                                     output <- list(high_connections = high[order(high)],
                                                                    low_connections = low[order(low)],
                                                                    high_id = paste(high[order(high)],
                                                                                    collapse = "_"),
                                                                    low_id = paste(low[order(low)],
                                                                                   collapse = "_"))
                                                   } else {
                                                     cap_elevation <- sample(x = c("high",
                                                                                   "low"),
                                                                             size = 1,
                                                                             replace = FALSE)
                                                     output <- list(high_connections = c(),
                                                                    low_connections = c(),
                                                                    high_id = "",
                                                                    low_id = "")
                                                     output[[paste0(cap_elevation, "_connections")]] <- available_connections
                                                     output[[paste0(cap_elevation, "_id")]] <- available_connections
                                                   }
                                                   
                                                   output
                                                 })
                 
                 
                 ##### Finding tile relationships -----------------------------------------------
                 # We'll find the relationships to the left and right then top and bottom.
                 # The id variable is the index in the base_vector for the tile. The other
                 # variables indicate whether the connection in that direction stays at the same
                 # elevation (TRUE) or changes elevation (FALSE)
                 message("Making relationship list")
                 relationship_list <- lapply(X = seq_len(length(blob_indices)),
                                             blob_indices = blob_indices,
                                             neighbors_list = neighbors_list,
                                             cell_connections_list = cell_connections_list,
                                             FUN = function(X, blob_indices, neighbors_list, cell_connections_list){
                                               message(X)
                                               current_blob_index <- blob_indices[X]
                                               message(current_blob_index)
                                               current_neighbors <- neighbors_list[[X]]
                                               
                                               
                                               
                                               current_outgoing_connections <- lapply(X = c("high",
                                                                                            "low"),
                                                                                      current_outgoing_connections = cell_connections_list[[X]],
                                                                                      # USE.NAMES = FALSE,
                                                                                      FUN = function(X, current_outgoing_connections){
                                                                                        current_directions <- current_outgoing_connections[[paste0(X, "_connections")]]
                                                                                        
                                                                                        setNames(rep(x = X,
                                                                                                     times = length(current_directions)),
                                                                                                 nm = current_directions)
                                                                                      }) |>
                                                 unlist()
                                               
                                               relationships <- sapply(X = names(current_outgoing_connections),
                                                                       current_outgoing_connections = current_outgoing_connections,
                                                                       current_neighbors = current_neighbors,
                                                                       blob_indices = blob_indices,
                                                                       cell_connections_list = cell_connections_list,
                                                                       USE.NAMES = FALSE,
                                                                       FUN = function(X, current_outgoing_connections, current_neighbors, blob_indices, cell_connections_list){
                                                                         message(X)
                                                                         current_neighbor_blob_index <- which(blob_indices == current_neighbors[X])
                                                                         
                                                                         current_neighbor_connections <- lapply(X = c("high",
                                                                                                                      "low"),
                                                                                                                current_neighbor_connections = cell_connections_list[[current_neighbor_blob_index]],
                                                                                                                # USE.NAMES = FALSE,
                                                                                                                FUN = function(X, current_neighbor_connections){
                                                                                                                  message(X)
                                                                                                                  current_directions <- current_neighbor_connections[[paste0(X, "_connections")]]
                                                                                                                  message(paste(current_directions,
                                                                                                                                collapse = " "))
                                                                                                                  setNames(rep(x = X,
                                                                                                                               times = length(current_directions)),
                                                                                                                           nm = current_directions)
                                                                                                                }) |>
                                                                           unlist()
                                                                         
                                                                         current_outgoing_connections[X] == current_neighbor_connections[c(top = "bottom",
                                                                                                                                           bottom = "top",
                                                                                                                                           left = "right",
                                                                                                                                           right = "left")[X]]
                                                                         
                                                                       })
                                               
                                               relationships
                                             })
                 
                 
                 ##### Positioning the lines ----------------------------------------------------
                 # This creates the lines for every tile by selecting the appropriate base lines
                 # then translating them.
                 # The order of all these is low elevation edging, low elevation lines, high
                 # elevation edging, high elevation lines so that they render from the bottom up.
                 message("Crunching line data")
                 line_data <- lapply(X = seq_len(length(blob_indices)),
                                     blob_indices = blob_indices,
                                     base_lines = base_lines,
                                     cell_connections_list = cell_connections_list,
                                     n_rows = input$n_rows,
                                     relationship_list = relationship_list,
                                     arcs = input$arc_vertex_count == 0,
                                     FUN = function(X, blob_indices, base_lines, cell_connections_list, n_rows, relationship_list, arcs){
                                       message(X)
                                       current_blob_index <- blob_indices[X]
                                       message(current_blob_index)
                                       
                                       x_offset <- trunc((current_blob_index - 1) / n_rows)
                                       y_offset <- (current_blob_index - 1) %% n_rows
                                       
                                       tile_subunit_ids <- setNames(object = unlist(cell_connections_list[[X]][c("high_id", "low_id")]),
                                                                    nm = c("high", "low"))
                                       tile_subunit_ids <- tile_subunit_ids[sapply(X = tile_subunit_ids,
                                                                                   FUN = nchar) > 0]
                                       
                                       tiles <- lapply(X = setNames(object = names(tile_subunit_ids),
                                                                    nm = names(tile_subunit_ids)),
                                                       current_blob_index = current_blob_index,
                                                       tile_subunit_ids = tile_subunit_ids,
                                                       base_lines = base_lines,
                                                       x_offset = x_offset,
                                                       y_offset = y_offset,
                                                       current_relationships = relationship_list[[X]],
                                                       arcs = arcs,
                                                       FUN = function(X, current_blob_index, tile_subunit_ids, base_lines, x_offset, y_offset, current_relationships, arcs){
                                                         message(X)
                                                         current_tile_subunit_id <- tile_subunit_ids[[X]]
                                                         message(current_tile_subunit_id)
                                                         
                                                         # In the order that the connections are
                                                         # listed, paste together the values so
                                                         # we can use them to build full
                                                         # gradient IDs
                                                         gradient_relationship_string <- paste(current_relationships[stringr::str_split(string = current_tile_subunit_id,
                                                                                                                                        pattern = "_",
                                                                                                                                        simplify = TRUE) |> as.vector()],
                                                                                               collapse = "_")
                                                         
                                                         lines_output <- lapply(X = c("line",
                                                                                      "edging"),
                                                                                current_tile_subunit_id = current_tile_subunit_id,
                                                                                current_blob_index = current_blob_index,
                                                                                elevation = X,
                                                                                base_lines = base_lines,
                                                                                x_offset = x_offset,
                                                                                y_offset = y_offset,
                                                                                gradient_relationship_string = gradient_relationship_string,
                                                                                arcs = arcs,
                                                                                FUN = function(X, current_tile_subunit_id, current_blob_index, elevation, base_lines, x_offset, y_offset, gradient_relationship_string, arcs){
                                                                                  message(X)
                                                                                  
                                                                                  current_lines <- base_lines[[X]][[current_tile_subunit_id]]
                                                                                  
                                                                                  # Hacky, but to prevent complaints from dplyr::case_when()
                                                                                  # because it can't find variables for situations that aren't
                                                                                  # being triggered but are defined.
                                                                                  missing_vars <- setdiff(y = names(current_lines),
                                                                                                          x = c(paste0("x_",
                                                                                                                       c("start",
                                                                                                                         "end",
                                                                                                                         "origin",
                                                                                                                         "terminus",
                                                                                                                         "coords")),
                                                                                                                paste0("y_",
                                                                                                                       c("start",
                                                                                                                         "end",
                                                                                                                         "origin",
                                                                                                                         "terminus",
                                                                                                                         "coords")),
                                                                                                                paste0("radius_",
                                                                                                                       c("x",
                                                                                                                         "y"))))
                                                                                  
                                                                                  for (missing_var in missing_vars) {
                                                                                    current_lines[[missing_var]] <- NA
                                                                                  }
                                                                                  
                                                                                  lines_output <- dplyr::mutate(.data = current_lines,
                                                                                                                # This is silly, but it allows this to handle both
                                                                                                                # The lists of vertices in the x_coords and y_coords
                                                                                                                # variables AND the various unlisted x_* and y_*
                                                                                                                # variables for the arcs.
                                                                                                                dplyr::across(.cols = tidyselect::matches(match = "^x_"),
                                                                                                                              .fns = ~ switch(class(.x),
                                                                                                                                              "list" = {
                                                                                                                                                lapply(X = .x,
                                                                                                                                                       x_offset = x_offset,
                                                                                                                                                       FUN = function(X, x_offset){
                                                                                                                                                         as.numeric(X) + x_offset
                                                                                                                                                       })
                                                                                                                                              },
                                                                                                                                              {
                                                                                                                                                as.numeric(.x) + x_offset
                                                                                                                                              })),
                                                                                                                dplyr::across(.cols = tidyselect::matches(match = "^y_"),
                                                                                                                              .fns = ~ switch(class(.x),
                                                                                                                                              "list" = {
                                                                                                                                                lapply(X = .x,
                                                                                                                                                       y_offset = y_offset,
                                                                                                                                                       FUN = function(X, y_offset){
                                                                                                                                                         as.numeric(X) + y_offset
                                                                                                                                                       })
                                                                                                                                              },
                                                                                                                                              {
                                                                                                                                                as.numeric(.x) + y_offset
                                                                                                                                              })),
                                                                                                                current_blob_index = current_blob_index,
                                                                                                                tile_subunit_id = current_tile_subunit_id,
                                                                                                                type = X,
                                                                                                                elevation = elevation,
                                                                                                                x_offset = x_offset,
                                                                                                                y_offset = y_offset,
                                                                                                                path_string = dplyr::case_when(!(tile_subunit_id %in% c("top_bottom",
                                                                                                                                                                        "left_right",
                                                                                                                                                                        "top",
                                                                                                                                                                        "bottom",
                                                                                                                                                                        "left",
                                                                                                                                                                        "right")) ~ paste("M",
                                                                                                                                                                                          round(x_start,
                                                                                                                                                                                                digits = 5),
                                                                                                                                                                                          round(y_start,
                                                                                                                                                                                                digits = 5),
                                                                                                                                                                                          # This is the x axis radius
                                                                                                                                                                                          # and the y axis radius
                                                                                                                                                                                          "A",
                                                                                                                                                                                          radius_x,
                                                                                                                                                                                          radius_y,
                                                                                                                                                                                          # These are rotation,
                                                                                                                                                                                          # large-arc-flag,
                                                                                                                                                                                          # and sweep-flag.
                                                                                                                                                                                          # We want no rotation,
                                                                                                                                                                                          # the small arc,
                                                                                                                                                                                          # and counter-clockwise sweep
                                                                                                                                                                                          0, 0, 0,
                                                                                                                                                                                          # The endpoint for the
                                                                                                                                                                                          # arc
                                                                                                                                                                                          round(x_end,
                                                                                                                                                                                                digits = 5),
                                                                                                                                                                                          round(y_end,
                                                                                                                                                                                                digits = 5)),
                                                                                                                                               # # For when there are line segments and it's not the straightaways!
                                                                                                                                               # # This will turn the vectors of coordinates into paths where the
                                                                                                                                               # # First coordinate pair is prefixed with "M" and the subsequent ones
                                                                                                                                               # # with "L" because we want to define these in absolute terms
                                                                                                                                               # !arcs & !(tile_subunit_id %in% c("top_bottom",
                                                                                                                                               #                                  "left_right",
                                                                                                                                               #                                  "top",
                                                                                                                                               #                                  "bottom",
                                                                                                                                               #                                  "left",
                                                                                                                                               #                                  "right")) ~ mapply(X = x_coords,
                                                                                                                                               #                                                     Y = y_coords,
                                                                                                                                               #                                                     command_prefix = c("M",
                                                                                                                                               #                                                                        rep(x = "L",
                                                                                                                                               #                                                                            times = input$arc_vertex_count + 1)),
                                                                                                                                               #                                                     FUN = function(X, Y, command_prefix){
                                                                                                                                               #                                                       paste0(command_prefix, " ",
                                                                                                                                               #                                                              X, ",", Y)
                                                                                                                                               #                                                     }),
                                                                                                                                               tile_subunit_id %in% c("top_bottom",
                                                                                                                                                                      "left_right") ~ paste("M",
                                                                                                                                                                                            round(x_start,
                                                                                                                                                                                                  digits = 5),
                                                                                                                                                                                            round(y_start,
                                                                                                                                                                                                  digits = 5),
                                                                                                                                                                                            # This is stupid but it lets us
                                                                                                                                                                                            # add a second dimension so that
                                                                                                                                                                                            # the gradient will work.
                                                                                                                                                                                            # Basically, move to the halfway
                                                                                                                                                                                            # point and put the tiniest
                                                                                                                                                                                            # possible juke in it that
                                                                                                                                                                                            # most SVG rendering engines
                                                                                                                                                                                            # can still recognize.
                                                                                                                                                                                            "l",
                                                                                                                                                                                            (x_end - x_start) * 0.5,
                                                                                                                                                                                            (y_end - y_start) * 0.5,
                                                                                                                                                                                            "l 0.01 0.01",
                                                                                                                                                                                            "l -0.01 -0.01",
                                                                                                                                                                                            "L",
                                                                                                                                                                                            round(x_end,
                                                                                                                                                                                                  digits = 5),
                                                                                                                                                                                            round(y_end,
                                                                                                                                                                                                  digits = 5)),
                                                                                                                                               tile_subunit_id %in% c("top",
                                                                                                                                                                      "bottom",
                                                                                                                                                                      "left",
                                                                                                                                                                      "right") ~ paste("M",
                                                                                                                                                                                       x_origin,
                                                                                                                                                                                       y_origin,
                                                                                                                                                                                       "L",
                                                                                                                                                                                       x_start, y_start,
                                                                                                                                                                                       "A",
                                                                                                                                                                                       radius_x, radius_y,
                                                                                                                                                                                       0, 0, 0,
                                                                                                                                                                                       x_end, y_end,
                                                                                                                                                                                       "L",
                                                                                                                                                                                       x_terminus, y_terminus)),
                                                                                                                gradient_id = paste(type,
                                                                                                                                    elevation,
                                                                                                                                    tile_subunit_id,
                                                                                                                                    gradient_relationship_string,
                                                                                                                                    sep = "-"))
                                                                                }) |>
                                                           # setNames(object = _,
                                                           #          nm = c("base",
                                                           #                 "edging"))
                                                           dplyr::bind_rows()
                                                         
                                                         
                                                         # Very stupid, but this is to build the line segment paths for
                                                         # tiles that don't have arcs.
                                                         # It was way easier to do it here than keep fighting the mutate above.
                                                         if (!arcs) {
                                                           relevant_subtiles = c("top_left",
                                                                                 "bottom_left",
                                                                                 "top_right",
                                                                                 "bottom_right")
                                                           corrected_paths <- apply(X = lines_output,
                                                                                    relevant_subtiles = relevant_subtiles,
                                                                                    command_prefixes = c("M",
                                                                                                         rep(x = "L",
                                                                                                             times = input$arc_vertex_count + 1)),
                                                                                    MARGIN = 1,
                                                                                    FUN = function(X, relevant_subtiles, command_prefixes){
                                                                                      
                                                                                      if (X[["tile_subunit_id"]] %in% relevant_subtiles) {
                                                                                        mapply(X = X[["x_coords"]],
                                                                                               Y = X[["y_coords"]],
                                                                                               command_prefix = command_prefixes,
                                                                                               FUN = function(X, Y, command_prefix){
                                                                                                 paste0(command_prefix, " ",
                                                                                                        X, ",", Y)
                                                                                               }) |>
                                                                                          paste(collapse = " ") 
                                                                                      } else {
                                                                                        X[["path_string"]]
                                                                                      }
                                                                                    })
                                                           lines_output[lines_output[["tile_subunit_id"]] %in% relevant_subtiles, "path_string"] <- corrected_paths
                                                         }
                                                         
                                                         lines_output
                                                       })
                                       # tiles
                                       dplyr::bind_rows(tiles) |>
                                         # Chuck x_coords and y_coords because
                                         # they can cause class conflicts and we
                                         # don't need them anymore.
                                         dplyr::select(.data = _,
                                                       -tidyselect::any_of(x = c("x_coords",
                                                                                 "y_coords")))
                                     }) |>
                   dplyr::bind_rows()
                 
                 
                 ##### Gradients ----------------------------------------------------------------
                 # Make the gradients for each possible connection type, angle, and direction.
                 # The gradients here are referenced in the line and class strings.
                 # Gradient names are created with:
                 # [type (base or edging)]-[elevation within tile]-[tile_subunit_id]-[relationship 1]_[relationship 2]
                 message("Making gradients")
                 gradients <- lapply(X = c("line",
                                           "edging"),
                                     colors = workspace$colors,
                                     line_data = line_data,
                                     FUN = function(X, colors, line_data){
                                       message(X)
                                       current_colors <- setNames(object = colorRampPalette(colors = colors[[X]])(3),
                                                                  nm = c("high",
                                                                         "mid",
                                                                         "low"))
                                       
                                       expand.grid(type = X,
                                                   elevation = c("low",
                                                                 "high"),
                                                   connection_1_direction = c("top",
                                                                              "bottom",
                                                                              "left",
                                                                              "right"),
                                                   connection_2_direction = c("top",
                                                                              "bottom",
                                                                              "left",
                                                                              "right",
                                                                              ""),
                                                   connection_1_relationship = c(TRUE,
                                                                                 FALSE),
                                                   connection_2_relationship = c(TRUE,
                                                                                 FALSE)) |>
                                         # dplyr::filter(.data = _,
                                         #               type == "line",
                                         #               elevation == "high",
                                         #               connection_1_direction == "left",
                                         #               connection_2_direction == "right",
                                         #               connection_1_relationship == FALSE,
                                         #               connection_2_relationship == FALSE) |>
                                         # This makes sure we're only constructing gradients for
                                         # possible connections based on the order that
                                         # connections are listed.
                                         dplyr::filter(.data = _,
                                                       as.numeric(connection_1_direction) < as.numeric(connection_2_direction)) |>
                                         dplyr::mutate(.data = _,
                                                       # direction_string = dplyr::case_when(),
                                                       connection_2_direction = dplyr::case_when(connection_2_direction == "" ~ NA,
                                                                                                 .default = connection_2_direction),
                                                       tile_subunit_id = paste(connection_1_direction,
                                                                               connection_2_direction,
                                                                               sep = "_") |>
                                                         stringr::str_replace(string = _,
                                                                              pattern = "_NA$",
                                                                              replacement = ""),
                                                       flat = dplyr::case_when(is.na(connection_2_direction) ~ TRUE,
                                                                               !is.na(connection_2_direction) ~ connection_1_relationship & connection_2_relationship | !connection_1_relationship & !connection_2_relationship),
                                                       # Probably want to figure out a clever way
                                                       # to not hardcode this, but whatever.
                                                       # It's hard enough for me to visualize
                                                       # this that proceduralizing it isn't worth
                                                       # it right now.
                                                       gradient_direction_string = dplyr::case_when(tile_subunit_id %in% c("top_left") ~ "x1='1' x2='0' y1='0' y2='1'",
                                                                                                    tile_subunit_id %in% c("bottom_right") ~ "x1='0' x2='1' y1='1' y2='0'",
                                                                                                    
                                                                                                    tile_subunit_id %in% c("bottom_left") ~ "x1='1' x2='0' y1='1' y2='0'",
                                                                                                    tile_subunit_id %in% c("top_right") ~ "x1='0' x2='1' y1='0' y2='1'",
                                                                                                    
                                                                                                    tile_subunit_id %in% c("top_bottom") ~ "x1='0' x2='0' y1='0' y2='1'",
                                                                                                    tile_subunit_id %in% c("left_right") ~ "x1='0' x2='1' y1='0' y2='0'",
                                                                                                    tile_subunit_id %in% c("top") ~ "x1='0' x2='0' y1='0' y2='1'",
                                                                                                    tile_subunit_id %in% c("bottom")  ~ "x1='0' x2='0' y1='1' y2='0'",
                                                                                                    tile_subunit_id %in% c("left") ~ "x1='0' x2='1' y1='0' y2='0'",
                                                                                                    tile_subunit_id %in% c("right") ~ "x1='1' x2='0' y1='0' y2='0'"),
                                                       stop1 = dplyr::case_when(!connection_1_relationship ~ current_colors["mid"],
                                                                                connection_1_relationship ~ current_colors[paste(elevation)],
                                                                                .default = current_colors[paste(elevation)]),
                                                       stop2 = current_colors[paste(elevation)],
                                                       stop3 = dplyr::case_when(!connection_2_relationship ~ current_colors["mid"],
                                                                                connection_2_relationship ~ current_colors[paste(elevation)],
                                                                                .default = current_colors[paste(elevation)]),
                                                       gradient_id = dplyr::case_when(is.na(connection_2_direction) ~ paste(type,
                                                                                                                            elevation,
                                                                                                                            tile_subunit_id,
                                                                                                                            connection_1_relationship,
                                                                                                                            sep = "-"),
                                                                                      !is.na(connection_2_direction) ~ paste(type,
                                                                                                                             elevation,
                                                                                                                             tile_subunit_id,
                                                                                                                             paste(connection_1_relationship,
                                                                                                                                   connection_2_relationship,
                                                                                                                                   sep = "_") |>
                                                                                                                               gsub(x = _,
                                                                                                                                    pattern = "_NA$",
                                                                                                                                    replacement = ""),
                                                                                                                             sep = "-")),
                                                       gradient_string = paste0("<linearGradient id='", gradient_id, "' ",
                                                                                gradient_direction_string, ">",
                                                                                "<stop offset='0%' stop-color='", stop1, "' />",
                                                                                "<stop offset='10%' stop-color='", stop1, "' />",
                                                                                "<stop offset='40%' stop-color='", stop2, "' />",
                                                                                # "<stop offset='50%' stop-color='", stop2, "' />",
                                                                                # "<stop offset='60%' stop-color='", stop2, "' />",
                                                                                # "<stop offset='85%' stop-color='", stop3, "' />",
                                                                                "<stop offset='60%' stop-color='", stop2, "' />",
                                                                                "<stop offset='90%' stop-color='", stop3, "' />",
                                                                                "<stop offset='100%' stop-color='", stop3, "' />",
                                                                                "</linearGradient>"))
                                     }) |>
                   dplyr::bind_rows() |>
                   dplyr::distinct() |>
                   dplyr::filter(.data = _,
                                 gradient_id %in% line_data$gradient_id)
                 
                 #### ASSEMBLING SVG ############################################################
                 ##### Header -------------------------------------------------------------------
                 message("header")
                 header <- paste0("<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='svglite'",
                                  " width='", input$n_cols * input$tile_pixels, "px' height='", input$n_rows * input$tile_pixels, "px'",
                                  " viewBox='0 ", 0," ", input$n_cols," ", input$n_rows, "'>",
                                  "<rect width='100%' height='100%' style='stroke: none; fill: ", workspace$colors$background,";'/>")
                 
                 ##### Classes ------------------------------------------------------------------
                 # These apply the basic properties across all lines and edging lines
                 message("classes")
                 base_classes <- c(paste0(".edging {stroke-width:", edging_width * 1.05,";",
                                          "stroke-linecap:round;fill:none;}"),
                                   paste0(".line {stroke-width:", line_width,";",
                                          "stroke-linecap:round;fill:none;}"),
                                   paste0(".line-cap {stroke-width:", line_width / 2,";",
                                          "stroke-linecap:round;fill:none;}")) |>
                   paste(collapse = "
        ")
                 
                 # These are for assigning specific gradients to the appropriately-oriented lines
                 gradient_classes <- dplyr::mutate(.data = gradients,
                                                   class = paste0(".", gradient_id, " {stroke:url(#", gradient_id, ");}")) |>
                   dplyr::pull(.data = _,
                               class) |>
                   paste(collapse = "
        ")
                 
                 message("style")
                 style <- paste("<style>
", base_classes,
                                "
", gradient_classes,"
</style>
</svg>")
                 
                 message("lines")         
                 lines <- line_data |>
                   dplyr::filter(.data = _,
                                 !(radius_x %in% c(0)))|>
                   dplyr::mutate(.data = _,
                                 classes = dplyr::case_when(is.na(special_type) ~ paste(type,
                                                                                        gradient_id),
                                                            !is.na(special_type) ~ paste(special_type,
                                                                                         gradient_id)),
                                 string = paste0("<path class='", classes, "' ",
                                                 "d='", path_string, "'/>")) |>
                   dplyr::arrange(.data = _,
                                  dplyr::desc(elevation),
                                  type) |>
                   # dplyr::filter(.data = _,
                   #               type == "line") |>
                   dplyr::pull(.data = _,
                               string)
                 
                 #### WRITING ###################################################################
                 filename <- "output_1.svg"
                 filename_base <- "output_"
                 current_outputs <- list.files(path = workspace$temp_dir,
                                               pattern = paste0(filename_base, "\\d+"))
                 if (length(current_outputs) > 0) {
                   filename <- paste0(filename_base,
                                      list.files(path = workspace$temp_dir,
                                                 pattern = paste0(filename_base, "\\d+")) |>
                                        stringr::str_extract(string = _,
                                                             pattern = "\\d+") |>
                                        as.numeric() |>
                                        max() + 1,
                                      ".svg")
                 }
                 
                 message(file.path(workspace$temp_dir,
                                   filename))
                 workspace$svg <- c(header,
                                    lines,
                                    dplyr::pull(.data = gradients,
                                                gradient_string),
                                    style)
                 writeLines(text = workspace$svg,
                            con = file.path(workspace$temp_dir,
                                            filename))
                 message("DONE")
                 if (is.null(workspace$svg)) {
                   output$svg_embed <- renderUI(expr = HTML("waiting"))
                 } else {
                   message(paste0("<img src = '", filename, "' height = '100%'>"))
                   output$svg_embed <- renderUI(expr = HTML(paste0("<img src = '", filename, "' height = '100%'>")))
                 }
                 message("rendered")
               })
  
  # observeEvent(eventExpr = workspace$svg,
  #              handlerExpr = {
  #                if (is.null(workspace$svg)) {
  #                  output$svg_embed <- renderUI(expr = HTML("waiting"))
  #                } else {
  #                  output$svg_embed <- renderUI(expr = HTML(paste0("<img src = '", "output.svg", "' height = '100%'>")))
  #                }
  #              })
}

# Run the application 
shinyApp(ui = ui, server = server)
