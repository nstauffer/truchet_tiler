# Required packages
library(shiny)
library(colourpicker)
library(tidyverse)
source("support_functions.R")

#### For testing ####
input <- list(n_cols = 20,
              n_rows = 9,
              n_lines = 3,
              min_radius = 0.15,
              proportion_lines = 0.65,
              proportion_edges = 0.35,
              arc_vertex_count = 2,
              # color_line_high = "#483D8B",
              # color_line_low = "#3DDBD9",
              # color_edging_high = "#FFB000",
              # color_edging_low = "#DC267F",
              # color_background = "#36013F",
              color_line_high = "#000000",
              color_line_low = "#6F6F6F",
              color_edging_high = "#FFFFFF",
              color_edging_low = "#FFFFFF",
              color_background = "#FFFFFF",
              allow_straights = FALSE,
              margin = 0.15,
              tile_pixels = 100,
              proportion_arc = 0,
              mixed_tiles = TRUE)

for (row_number in seq_len(input$n_rows)) {
  input[[paste0("row_", row_number)]] <- seq_len(input$n_cols)
}

workspace <- list(temp_dir = "www",
                  current_output = NULL,
                  buffer = FALSE,
                  respect_margins = TRUE,
                  base_matrix = matrix(data = rep(1,
                                                  times = input$n_cols * input$n_rows),
                                       nrow = input$n_rows),
                  rotation = 0,
                  colors = NULL,
                  svg = NULL)
# connections <- as.factor(c("top", "bottom", "left" ,"right"))
# cell_connections_list <- list(list(high_connections = connections[c(1,2)],
#                                    low_connections = connections[c(3,4)],
#                                    high_id = "top_bottom",
#                                    low_id = "left_right"),
#                               list(low_connections = connections[c(1,2)],
#                                    high_connections = connections[c(3,4)],
#                                    high_id = "left_right",
#                                    low_id = "top_bottom"),
#                               list(low_connections = connections[c(1,2)],
#                                    high_connections = connections[c(3,4)],
#                                    high_id = "left_right",
#                                    low_id = "top_bottom"),
#                               list(high_connections = connections[c(1,2)],
#                                    low_connections = connections[c(3,4)],
#                                    high_id = "top_bottom",
#                                    low_id = "left_right"))

# UI ###########################################################################
# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("Truchet Tiler v0.8.0"),
  
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
        column(width = 4,
               numericInput(inputId = "arc_vertex_count",
                            label  = "Number of vertices (0 produces a smooth arc):",
                            min = 0,
                            max = 4,
                            value = 0,
                            step = 1)),
        column(width = 4,
               numericInput(inputId = "proportion_arc",
                            label  = "Proportion of cells to use smooth arcs (regardless of number of vertices selected):",
                            min = 0,
                            max = 1,
                            value = 0,
                            step = 0.01)),
        column(width = 4,
               checkboxInput(inputId = "mixed_tiles",
                            label  = "Allow mixed pattern types (arc or segment) within a tile"),
               value = FALSE)#,
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
  
  #### Generate button ########################################################
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
                                  dplyr::desc(corner_radius))
                 
                 # cap_radii_df <- radii_df |>
                 #   dplyr::select(.data = _,
                 #                 -corner_radius) |>
                 #   dplyr::distinct() |>
                 #   dplyr::arrange(.data = _,
                 #                  dplyr::desc(radius)) |>
                 #   dplyr::mutate(.data = _,
                 #                 special_type = dplyr::case_when(radius == 0 ~ "line-cap",
                 #                                                 .default = NA),
                 #                 radius = dplyr::case_when(radius == 0 ~ line_width / 4,
                 #                                           .default = radius),
                 #                 # This is fucking stupid because I couldn't figure out what was
                 #                 # going wrong with cumsum()
                 #                 # It figures out the number of edging and line widths to displace
                 #                 # by
                 #                 previous_edgings = cumsum(as.numeric(type == "edging")),
                 #                 previous_edgings = dplyr::case_when(type == "edging" ~ previous_edgings - 0.5,
                 #                                                     type == "line" ~ previous_edgings),
                 #                 # previous_edgings = dplyr::case_when(dplyr::row_number() > 1 ~ previous_edgings - 0.5,
                 #                 #                                     .default = previous_edgings),
                 #                 # previous_edgings = dplyr::case_when(type == "edging" & previous_edgings == 0 ~ 0.5,
                 #                 #                                     .default = previous_edgings),
                 #                 previous_lines = cumsum(as.numeric(type == "line")),
                 #                 previous_lines = dplyr::case_when(type == "line" ~ previous_lines - 0.5,
                 #                                                   type == "edging" ~ previous_lines),
                 #                 # previous_lines = dplyr::case_when(type == "line" & previous_lines == 0 ~ 0.5,
                 #                 #                                     .default = previous_lines),
                 #                 displacement = dplyr::case_when(is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin,
                 #                                                 !is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin - line_width / 4))
                 # 
                 # cap_radii_df$previous_edging_pairs <- sapply(X = seq_len(nrow(cap_radii_df)),
                 #                                              cap_radii_df = cap_radii_df,
                 #                                              FUN = function(X, cap_radii_df){
                 #                                                if (X %in% c(1, nrow(cap_radii_df))) {
                 #                                                  FALSE
                 #                                                } else {
                 #                                                  if (cap_radii_df$type[X - 1] == cap_radii_df$type[X]) {
                 #                                                    TRUE
                 #                                                  } else {
                 #                                                    FALSE
                 #                                                  }
                 #                                                }
                 #                                              })
                 # 
                 # # dplyr::glimpse(cap_radii_df)
                 # 
                 # cap_radii_df <- dplyr::mutate(.data = cap_radii_df,
                 #                               previous_edging_pairs_count = cumsum(previous_edging_pairs),
                 #                               previous_edgings = previous_edgings - 0.5 * previous_edging_pairs_count,
                 #                               displacement = dplyr::case_when(is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin,
                 #                                                               !is.na(special_type) ~ previous_edgings * edging_width + previous_lines * line_width + radius + input$margin - line_width / 4))
                 
                 
                 ###### Patterns -----------------------------------------------
                 # There will be a vertex at each join between tiles and then
                 # the number of vertices requested will be put between each.
                 vertex_count <- 4 * (1 + input$arc_vertex_count)
                 
                 
                 
                 patterns <- dplyr::mutate(.data = radii_df,
                                           segment_pattern_string = paste0('<polygon points="', 
                                                                           sapply(X = corner_radius,
                                                                                  vertex_count = vertex_count,
                                                                                  side_length = NULL,
                                                                                  center_x = 0,
                                                                                  center_y = 0,
                                                                                  output_type = "polygon",
                                                                                  FUN = regular_polygon),
                                                                           '" stroke-width="', width, '" fill="none" '),
                                           arc_pattern_string = paste0('<circle cx="0" cy="0" r="', corner_radius ,'" stroke-width="', width, '" fill="none" '))
                 
                 #### GENERATION ################################################################
                 ##### Make the base matrix -----------------------------------------------------
                 message("Making base matrix")
                 # This is a stub for eventually implementing intertwining palettes
                 id_value <- 1
                 
                 base_vector <- lapply(X = paste0("row_", seq_len(input$n_rows)),
                                       n_cols = input$n_cols,
                                       id_value = id_value,
                                       FUN = function(X, n_cols, id_value){
                                         row_vector <- as.numeric(input[[X]])
                                         output <- rep(x = 0,
                                                       times = n_cols)
                                         output[row_vector] <- id_value
                                         output
                                       }) |>
                   do.call(what = rbind,
                           args = _) |>
                   as.vector(x = _)
                 
                 message("Base vector:")
                 message(paste(base_vector,
                               collapse = " "))
                 
                 
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
                                                                                           # FALSE
                                                                                           TRUE
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
                                            # This is for when things shouldn't
                                            # wrap to the other side of the canvas
                                            # output <- c(left = X - n_rows,
                                            #             right = X + n_rows,
                                            #             top = if (X %% n_rows == 1) {
                                            #               -Inf
                                            #             } else {
                                            #               X - 1
                                            #             },
                                            #             bottom = if (X %% n_rows == 0) {
                                            #               -Inf
                                            #             } else {
                                            #               X + 1
                                            #             })
                                            output <- c(
                                              left = if ((X - n_rows) > 0) {
                                                X - n_rows
                                              } else {
                                                max(blob_indices) - (n_rows - X)
                                              },
                                              right = if ((X + n_rows) <= max(blob_indices)) {
                                                X + n_rows
                                              } else {
                                                X + n_rows - max(blob_indices)
                                              },
                                              top = if (X %% n_rows == 1) {
                                                n_rows
                                              } else {
                                                X - 1
                                              },
                                              bottom = if (X %% n_rows == 0) {
                                                1
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
                 
                 ##### Gradients ----------------------------------------------------------------
                 # Need a upper and lower for each of:
                 # H-H
                 # L-L
                 # L-H
                 # H-L
                 # M-H
                 # M-L
                 # H-M
                 # L-M
                 # Don't care about the specific sides because we're rotating the pattern
                 message("Making gradients")
                 gradients <- lapply(X = c("line",
                                           "edging"),
                                     colors = workspace$colors,
                                     FUN = function(X, colors){
                                       message(X)
                                       current_colors <- setNames(object = colorRampPalette(colors = colors[[X]])(3),
                                                                  nm = c("high",
                                                                         "mid",
                                                                         "low"))
                                       
                                       expand.grid(type = X,
                                                   elevation = c("low",
                                                                 "high"),
                                                   connection_1_relationship = c(TRUE,
                                                                                 FALSE),
                                                   connection_2_relationship = c(TRUE,
                                                                                 FALSE)) |>
                                         dplyr::mutate(.data = _,
                                                       dplyr::across(.cols = dplyr::where(is.factor),
                                                                     .fns = as.character),
                                                       connection_1_elevation = dplyr::case_when(connection_1_relationship ~ elevation,
                                                                                                 !connection_1_relationship ~ "mid"),
                                                       connection_2_elevation = dplyr::case_when(connection_2_relationship ~ elevation,
                                                                                                 !connection_2_relationship ~ "mid"),
                                                       gradient_id = paste("gradient",
                                                                           type,
                                                                           connection_1_elevation,
                                                                           elevation,
                                                                           connection_2_elevation,
                                                                           sep = "-"),
                                                       stop1 = current_colors[paste(connection_1_elevation)],
                                                       stop2 = current_colors[paste(elevation)],
                                                       stop3 = current_colors[paste(connection_2_elevation)],
                                                       gradient_string = paste0("<linearGradient id='", gradient_id, "' ",
                                                                                # This gradient direction bit here is defined
                                                                                # for a circle that has its center at (0,0)
                                                                                "x1='1' x2='0' y1='0' y2='1'", ">",
                                                                                "<stop offset='0%' stop-color='", stop1, "' />",
                                                                                "<stop offset='25%' stop-color='", stop1, "' />",
                                                                                "<stop offset='40%' stop-color='", stop2, "' />",
                                                                                "<stop offset='60%' stop-color='", stop2, "' />",
                                                                                "<stop offset='75%' stop-color='", stop3, "' />",
                                                                                "<stop offset='100%' stop-color='", stop3, "' />",
                                                                                "</linearGradient>"))
                                     }) |>
                   dplyr::bind_rows() |>
                   dplyr::distinct()
                 
                 ##### Assigning patterns ----------------------------------------------------
                 # This assigns the patterns and gradients to each cell.
                 # The order of all these is low elevation edging, low elevation lines, high
                 # elevation edging, high elevation lines so that they render from the bottom up.
                 message("Crunching cell patterns")
                 cell_data <- lapply(X = seq_len(length(blob_indices)),
                                     blob_indices = blob_indices,
                                     base_lines = base_lines,
                                     cell_connections_list = cell_connections_list,
                                     n_rows = input$n_rows,
                                     relationship_list = relationship_list,
                                     gradients = gradients,
                                     arcs = input$arc_vertex_count < 1,
                                     mixed_tiles = input$mixed_tiles,
                                     proportion_arc = input$proportion_arc,
                                     FUN = function(X, blob_indices, base_lines, cell_connections_list, n_rows, relationship_list, gradients, arcs, proportion_arc, mixed_tiles){
                                       message(X)
                                       current_blob_index <- blob_indices[X]
                                       message(current_blob_index)
                                       
                                       x_offset <- trunc((current_blob_index - 1) / n_rows)
                                       y_offset <- (current_blob_index - 1) %% n_rows
                                       
                                       tile_subunit_ids <- setNames(object = unlist(cell_connections_list[[X]][c("high_id", "low_id")]),
                                                                    nm = c("high", "low"))
                                       tile_subunit_ids <- tile_subunit_ids[sapply(X = tile_subunit_ids,
                                                                                   FUN = nchar) > 0]
                                       
                                       current_relationships <- data.frame(connection = names(relationship_list[[X]]),
                                                                           relationship = unname(relationship_list[[X]]))
                                       
                                       working_data_frame <- suppressWarnings(data.frame(x_offset = trunc((current_blob_index - 1) / n_rows),
                                                                                         y_offset = (current_blob_index - 1) %% n_rows,
                                                                                         layer = c("high",
                                                                                                   "low"),
                                                                                         type = c("edging", "line", "line", "edging"),
                                                                                         connections = unlist(cell_connections_list[[X]][c("high_id", "low_id")]))) |>
                                         dplyr::mutate(.data = _,
                                                       connection_1 = stringr::str_extract(string = connections,
                                                                                           pattern = "^[a-z]+"),
                                                       connection_2 = stringr::str_extract(string = connections,
                                                                                           pattern = "[a-z]+$")) |>
                                         dplyr::left_join(x = _,
                                                          y = dplyr::select(.data = current_relationships,
                                                                            tidyselect::all_of(c(connection_1 = "connection",
                                                                                                 connection_1_relationship = "relationship"))),
                                                          by = c("connection_1"),
                                                          relationship = "many-to-one") |>
                                         dplyr::left_join(x = _,
                                                          y = dplyr::select(.data = current_relationships,
                                                                            tidyselect::all_of(c(connection_2 = "connection",
                                                                                                 connection_2_relationship = "relationship"))),
                                                          by = c("connection_2"),
                                                          relationship = "many-to-one") |>
                                         dplyr::mutate(.data = _,
                                                       rotation = dplyr::case_when(connections == "top_left" ~ 0,
                                                                                   connections == "top_right" ~ 90,
                                                                                   connections == "bottom_left" ~ -90,
                                                                                   connections == "bottom_right" ~ 180),
                                                       # placeholder = connection_1_relationship,
                                                       # connection_1_relationship = dplyr::case_when(connections %in% c("top_left",
                                                       #                                                                 "bottom_right") ~ connection_2_relationship,
                                                       #                                              .default = connection_1_relationship),
                                                       # connection_2_relationship = dplyr::case_when(connections %in% c("top_left",
                                                       #                                                                 "bottom_right") ~ placeholder,
                                                       #                                              .default = connection_1_relationship),
                                                       
                                                       # This is the stupidest thing and I'm not sure why I need it, but if the relationship is
                                                       # or "bottom_" they need to be swapped, so here that happens.
                                                       placeholder = connection_1_relationship,
                                                       connection_1_relationship = dplyr::case_when(connections %in% c("top_right",
                                                                                                                       "bottom_left") ~ connection_2_relationship,
                                                                                                    .default = connection_1_relationship),
                                                       connection_2_relationship = dplyr::case_when(connections %in% c("top_right",
                                                                                                                       "bottom_left") ~ placeholder,
                                                                                                    .default = connection_2_relationship),
                                                       connection_1_elevation = dplyr::case_when(!connection_1_relationship ~ "mid",
                                                                                                 .default = layer),
                                                       connection_2_elevation = dplyr::case_when(!connection_2_relationship ~ "mid",
                                                                                                 .default = layer)) |>
                                         dplyr::select(.data = _,
                                                       -tidyselect::any_of(x = c("placeholder"))) |>
                                         dplyr::left_join(x = _,
                                                          y = gradients,
                                                          by = c("layer" = "elevation",
                                                                 "type",
                                                                 "connection_1_relationship",
                                                                 "connection_2_relationship",
                                                                 "connection_1_elevation",
                                                                 "connection_2_elevation"),
                                                          relationship = "many-to-one") |>
                                         dplyr::left_join(x = _,
                                                          y = patterns,
                                                          by = "type",
                                                          relationship = "many-to-many")
                                       
                                       # This is toggled by input$mixed_tiles
                                       # When that's TRUE, then the arc-segment assignment
                                       # takes into account the variable layer
                                       # otherwise it's just the offsets
                                       pattern_type_join_vars <- c("x_offset",
                                                                   "y_offset",
                                                                   "layer")[c(TRUE,
                                                                              TRUE,
                                                                              mixed_tiles)]
                                       
                                       arc_lut <- dplyr::select(.data = working_data_frame,
                                                                tidyselect::all_of(x = pattern_type_join_vars)) |>
                                         dplyr::distinct() |>
                                         dplyr::mutate(.data = _,
                                                       pattern_type = sample(x = c("arc",
                                                                          "segment"),
                                                                    size = dplyr::n(),
                                                                    replace = TRUE,
                                                                    prob = c(input$proportion_arc,
                                                                             1 - input$proportion_arc)))
                                       
                                       # Of course, if the vertex count is 0 we
                                       # only draw arcs anyway.
                                       if (arcs) {
                                         arc_lut$apttern_type <- "arc"
                                       }

                                       
                                       working_data_frame <- dplyr::left_join(x = working_data_frame,
                                                                              y = arc_lut,
                                                                              by = pattern_type_join_vars,
                                                                              relationship = "many-to-one") |>
                                         dplyr::mutate(.data = _,
                                                       # pattern_type = dplyr::case_when(proportion_arc == 1 ~ "arc",
                                                       #                                 proportion_arc == 0 & !arcs ~ "segment",
                                                       #                                 !arcs ~ sample(x = c("arc",
                                                       #                                                      "segment"),
                                                       #                                                size = 1,
                                                       #                                                prob = c(proportion_arc,
                                                       #                                                         1 - proportion_arc)),
                                                       #                                 .default = "arc"),
                                                       pattern_id = paste0(type, "-", pattern_type, "-", layer, "-", connections, "-", connection_1_relationship, "_", connection_2_relationship),
                                                       pattern_subunit_string = dplyr::case_when(pattern_type == "arc" ~ paste0(arc_pattern_string,
                                                                                                               ' stroke="url(#', gradient_id, ')"/>'),
                                                                                                 pattern_type == "segment" ~ paste0(segment_pattern_string,
                                                                                                                ' stroke="url(#', gradient_id, ')"/>'),
                                                                                                 .default = paste0(arc_pattern_string,
                                                                                                                   ' stroke="url(#', gradient_id, ')"/>')))
                                     }) |>
                   dplyr::bind_rows() |>
                   # Arrange as low-edging, low-lines, high-edging, high-lines
                   dplyr::arrange(.data = _,
                                  dplyr::desc(layer),
                                  type)
                 
                 
                 
                 
                 #### ASSEMBLING SVG ############################################################
                 ##### Header -------------------------------------------------------------------
                 message("header")
                 header <- paste0("<svg xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' class='svglite'",
                                  " width='", input$n_cols * input$tile_pixels, "px' height='", input$n_rows * input$tile_pixels, "px'",
                                  " viewBox='0 ", 0," ", input$n_cols," ", input$n_rows, "'>",
                                  "<rect width='100%' height='100%' style='stroke: none; fill: ", workspace$colors$background,";'/>")
                 
                 ##### Patterns ------------------------------------------------
                 patterns_vector <- dplyr::summarize(.data = cell_data,
                                                     .by = "pattern_id",
                                                     pattern_string = paste0('<pattern patternUnits="objectBoundingBox" x="0" y="0" width="1" height="1" ',
                                                                             'id="', dplyr::first(pattern_id), '" ',
                                                                             'patternTransform="rotate(', dplyr::first(rotation), ')">',
                                                                             paste(pattern_subunit_string,
                                                                                   collapse = " "),
                                                                             "</pattern>")) |>
                   dplyr::pull(.data = _,
                               pattern_string) |>
                   unique()
                 
                 ##### Gradients ------------------------------------------------
                 gradients_vector <- dplyr::filter(.data = gradients,
                                                   gradient_id %in% cell_data$gradient_id) |>
                   dplyr::pull(.data = _,
                               gradient_string) |>
                   unique()
                 
                 ##### Cells ---------------------------------------------------
                 # Class rect objects to fill with the patterns. There's one
                 # cell for high and low for each tile.
                 cells <- dplyr::mutate(.data = cell_data,
                                        string = paste0('<rect x="', x_offset,
                                                        '" y="', y_offset,
                                                        '" width="1" height="1" fill="url(#',
                                                        pattern_id, ')" />')) |>
                   dplyr::filter(.data = _,
                                 !is.na(connection_1) & !is.na(connection_2)) |>
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
                                    patterns_vector,
                                    gradients_vector,
                                    cells,
                                    "</svg>")
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
