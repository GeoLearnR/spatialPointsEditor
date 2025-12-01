# R/edit_spatial_points.R

#' Interactive Spatial Point Editor
#'
#' Edit coordinates of sf POINT objects interactively with visual map feedback.
#' This function launches a Shiny application that allows you to:
#' \itemize{
#'   \item View points on an interactive map
#'   \item Select and drag points to new locations
#'   \item Edit coordinates manually in a table or input fields
#'   \item Track all modifications with undo/redo support
#'   \item Export a log of all changes
#' }
#'
#' @param sf_data An sf object with POINT geometry
#' @param id_column Character. Name of the ID column to identify points.
#'   If NULL, an internal ID will be created.
#' @param return_modified Logical. If TRUE, returns only modified points.
#'   Default is FALSE (returns all points).
#'
#' @return An sf object with corrected coordinates in the original CRS
#'
#' @details
#' The editor provides multiple ways to modify coordinates:
#' \enumerate{
#'   \item Click a point to select it (turns red)
#'   \item Drag the selected marker to a new position
#'   \item Edit coordinates in the data table
#'   \item Enter coordinates manually in the sidebar
#' }
#'
#' All changes are tracked and can be undone/redone. The modification log
#' shows original vs. new coordinates and the distance moved.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create sample points
#' points <- st_sf(
#'   id = 1:3,
#'   name = c("A", "B", "C"),
#'   geometry = st_sfc(
#'     st_point(c(-122.4194, 37.7749)),
#'     st_point(c(-118.2437, 34.0522)),
#'     st_point(c(-74.0060, 40.7128))
#'   ),
#'   crs = 4326
#' )
#'
#' # Edit points interactively
#' corrected_points <- edit_spatial_points(points, id_column = "id")
#'
#' # Get only modified points
#' modified_only <- edit_spatial_points(points, id_column = "id",
#'                                       return_modified = TRUE)
#' }
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny reactiveValues observe observeEvent renderText renderUI
#' @importFrom shiny actionButton numericInput helpText verbatimTextOutput hr
#' @importFrom shiny textOutput tabsetPanel tabPanel tags downloadButton
#' @importFrom shiny showNotification showModal modalDialog modalButton removeModal
#' @importFrom shiny runGadget dialogViewer stopApp uiOutput downloadHandler
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers addMarkers
#' @importFrom leaflet leafletOutput renderLeaflet leafletProxy clearMarkers
#' @importFrom leaflet addLayersControl layersControlOptions addLegend
#' @importFrom leaflet markerOptions providers clearControls
#' @importFrom sf st_sf st_sfc st_point st_geometry st_coordinates st_crs
#' @importFrom sf st_transform st_cast st_geometry_type st_distance
#' @importFrom DT datatable DTOutput renderDT formatStyle styleEqual formatRound
#' @importFrom shinyjs useShinyjs click
#'
#' @export
edit_spatial_points <- function(sf_data,
                                id_column = NULL,
                                return_modified = FALSE) {

  # Load required packages
  required_packages <- c("shiny", "leaflet", "sf", "DT", "shinyjs")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is required but not installed. ",
                  "Install it with: install.packages('", pkg, "')"))
    }
  }

  # Validation
  if (!inherits(sf_data, "sf")) {
    stop("Input must be an sf object")
  }

  geom_types <- sf::st_geometry_type(sf_data)
  if (!all(geom_types %in% c("POINT", "MULTIPOINT"))) {
    stop("This function only works with POINT or MULTIPOINT geometries")
  }

  # Convert MULTIPOINT to POINT if necessary
  if (any(geom_types == "MULTIPOINT")) {
    sf_data <- sf::st_cast(sf_data, "POINT")
    warning("MULTIPOINT geometries converted to POINT")
  }

  # Ensure CRS is WGS84 for leaflet
  original_crs <- sf::st_crs(sf_data)
  if (is.na(original_crs)) {
    warning("No CRS found. Assuming WGS84 (EPSG:4326)")
    sf::st_crs(sf_data) <- 4326
    original_crs <- sf::st_crs(4326)
  } else if (sf::st_crs(sf_data) != sf::st_crs(4326)) {
    sf_data <- sf::st_transform(sf_data, 4326)
  }

  # Create internal ID if not provided
  if (is.null(id_column)) {
    sf_data$.internal_id <- seq_len(nrow(sf_data))
    id_column <- ".internal_id"
  } else if (!id_column %in% names(sf_data)) {
    stop(paste0("Column '", id_column, "' not found in data"))
  }

  # Extract coordinates
  coords <- sf::st_coordinates(sf_data)
  sf_data$lon <- coords[, 1]
  sf_data$lat <- coords[, 2]

  # Store original data
  original_data <- sf_data

  # Create reactive values storage
  rv <- shiny::reactiveValues(
    data = sf_data,
    selected_point = NULL,
    modified_ids = character(0),
    history = list(),
    history_position = 0
  )

  # Add to history
  add_to_history <- function() {
    rv$history_position <- rv$history_position + 1
    rv$history[[rv$history_position]] <- rv$data
    # Remove any "future" history
    if (rv$history_position < length(rv$history)) {
      rv$history <- rv$history[1:rv$history_position]
    }
  }

  # UI
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::titlePanel("Interactive Spatial Point Editor"),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::tags$h4("Instructions"),
        shiny::helpText("1. Click on a point on the map to select it"),
        shiny::helpText("2. Edit coordinates in the table OR drag the marker"),
        shiny::helpText("3. Changes are applied automatically"),
        shiny::tags$hr(),

        shiny::tags$h4("Selected Point Info"),
        shiny::verbatimTextOutput("selected_info"),

        shiny::tags$hr(),
        shiny::tags$h4("Manual Coordinate Entry"),
        shiny::uiOutput("coord_inputs"),
        shiny::actionButton("apply_coords", "Apply Coordinates",
                            class = "btn-primary", width = "100%"),

        shiny::tags$hr(),
        shiny::tags$h4("Actions"),
        shiny::actionButton("undo", "↶ Undo", class = "btn-warning"),
        shiny::actionButton("redo", "↷ Redo", class = "btn-warning"),
        shiny::tags$br(), shiny::tags$br(),
        shiny::actionButton("reset_point", "Reset Selected Point",
                            class = "btn-danger"),
        shiny::actionButton("reset_all", "Reset All Points",
                            class = "btn-danger"),
        shiny::tags$br(), shiny::tags$br(),
        shiny::actionButton("done", "Done - Return Data",
                            class = "btn-success", width = "100%"),

        shiny::tags$hr(),
        shiny::tags$h4("Statistics"),
        shiny::textOutput("stats")
      ),

      shiny::mainPanel(
        width = 9,
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Map View",
            leaflet::leafletOutput("map", height = "600px"),
            shiny::tags$br(),
            shiny::helpText("💡 Tip: Click a point to select it, then drag the red marker to reposition")
          ),
          shiny::tabPanel(
            "Data Table",
            DT::DTOutput("data_table"),
            shiny::tags$br(),
            shiny::helpText("💡 Tip: Double-click cells to edit coordinates directly")
          ),
          shiny::tabPanel(
            "Modification Log",
            DT::DTOutput("log_table"),
            shiny::tags$br(),
            shiny::downloadButton("download_log", "Download Log")
          ),
          shiny::tabPanel(
            "Help",
            shiny::tags$h3("How to Use This Editor"),
            shiny::tags$ol(
              shiny::tags$li("View your points on the interactive map"),
              shiny::tags$li("Select a point by clicking on it (turns red)"),
              shiny::tags$li("Edit coordinates by:"),
              shiny::tags$ul(
                shiny::tags$li("Dragging the selected marker on the map, OR"),
                shiny::tags$li("Editing values in the Data Table, OR"),
                shiny::tags$li("Entering coordinates manually in the sidebar")
              ),
              shiny::tags$li("All changes are tracked in the Modification Log"),
              shiny::tags$li("Use Undo/Redo to revert/reapply changes"),
              shiny::tags$li("Click 'Done' when finished to return the corrected data")
            ),
            shiny::tags$hr(),
            shiny::tags$h4("Keyboard Shortcuts"),
            shiny::tags$ul(
              shiny::tags$li("Ctrl+Z: Undo"),
              shiny::tags$li("Ctrl+Y: Redo")
            )
          )
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Initialize history
    shiny::observe({
      rv$history[[1]] <- rv$data
      rv$history_position <- 1
    }, priority = 1000)

    # Render map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                  group = "Satellite") %>%
        leaflet::addLayersControl(
          baseGroups = c("OpenStreetMap", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # Update map markers
    shiny::observe({
      data <- rv$data

      # Regular points (blue)
      regular_points <- data
      if (!is.null(rv$selected_point)) {
        regular_points <- data[data[[id_column]] != rv$selected_point, ]
      }

      # Modified points (orange)
      modified_points <- data[data[[id_column]] %in% rv$modified_ids, ]
      if (!is.null(rv$selected_point)) {
        modified_points <- modified_points[
          modified_points[[id_column]] != rv$selected_point,
        ]
      }

      leaflet::leafletProxy("map") %>%
        leaflet::clearMarkers() %>%
        leaflet::clearControls()

      # Add regular unmodified points
      if (nrow(regular_points) > 0) {
        regular_unmodified <- regular_points[
          !regular_points[[id_column]] %in% rv$modified_ids,
        ]
        if (nrow(regular_unmodified) > 0) {
          leaflet::leafletProxy("map") %>%
            leaflet::addCircleMarkers(
              data = regular_unmodified,
              lng = ~lon,
              lat = ~lat,
              layerId = as.character(regular_unmodified[[id_column]]),
              radius = 8,
              color = "#0000FF",
              fillColor = "#0000FF",
              fillOpacity = 0.6,
              stroke = TRUE,
              weight = 2,
              popup = ~paste0(
                "<b>ID:</b> ", get(id_column), "<br>",
                "<b>Lon:</b> ", round(lon, 6), "<br>",
                "<b>Lat:</b> ", round(lat, 6)
              )
            )
        }
      }

      # Add modified points (not selected)
      if (nrow(modified_points) > 0) {
        leaflet::leafletProxy("map") %>%
          leaflet::addCircleMarkers(
            data = modified_points,
            lng = ~lon,
            lat = ~lat,
            layerId = as.character(modified_points[[id_column]]),
            radius = 8,
            color = "#FF8C00",
            fillColor = "#FFA500",
            fillOpacity = 0.7,
            stroke = TRUE,
            weight = 2,
            popup = ~paste0(
              "<b>ID:</b> ", get(id_column), " (MODIFIED)<br>",
              "<b>Lon:</b> ", round(lon, 6), "<br>",
              "<b>Lat:</b> ", round(lat, 6)
            )
          )
      }

      # Add selected point (red, draggable)
      if (!is.null(rv$selected_point)) {
        selected_data <- data[data[[id_column]] == rv$selected_point, ]
        if (nrow(selected_data) > 0) {
          leaflet::leafletProxy("map") %>%
            leaflet::addMarkers(
              data = selected_data,
              lng = ~lon,
              lat = ~lat,
              layerId = paste0("selected_", selected_data[[id_column]]),
              options = leaflet::markerOptions(draggable = TRUE),
              popup = ~paste0(
                "<b>SELECTED</b><br>",
                "<b>ID:</b> ", get(id_column), "<br>",
                "<b>Lon:</b> ", round(lon, 6), "<br>",
                "<b>Lat:</b> ", round(lat, 6)
              )
            )
        }
      }

      # Add legend
      leaflet::leafletProxy("map") %>%
        leaflet::addLegend(
          position = "bottomright",
          colors = c("#0000FF", "#FFA500", "#FF0000"),
          labels = c("Original", "Modified", "Selected"),
          opacity = 0.7
        )
    })

    # Handle marker click (selection)
    shiny::observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (!is.null(click)) {
        rv$selected_point <- click$id
      }
    })

    # Handle marker drag (coordinate update)
    shiny::observeEvent(input$map_marker_dragend, {
      event <- input$map_marker_dragend
      if (!is.null(event) && !is.null(rv$selected_point)) {

        # Extract the actual ID (remove "selected_" prefix if present)
        point_id <- gsub("^selected_", "", event$id)

        idx <- which(rv$data[[id_column]] == point_id)
        if (length(idx) > 0) {
          # Update coordinates
          rv$data$lon[idx] <- event$lng
          rv$data$lat[idx] <- event$lat

          # Update geometry
          new_geom <- sf::st_sfc(sf::st_point(c(event$lng, event$lat)), crs = 4326)
          sf::st_geometry(rv$data)[idx] <- new_geom

          # Track modification
          if (!point_id %in% rv$modified_ids) {
            rv$modified_ids <- c(rv$modified_ids, point_id)
          }

          # Add to history
          add_to_history()

          shiny::showNotification(
            paste("Point", point_id, "moved to:",
                  round(event$lng, 6), ",", round(event$lat, 6)),
            type = "message"
          )
        }
      }
    })

    # Selected point info
    output$selected_info <- shiny::renderText({
      if (is.null(rv$selected_point)) {
        return("No point selected")
      }

      idx <- which(rv$data[[id_column]] == rv$selected_point)
      if (length(idx) == 0) return("Point not found")

      point <- rv$data[idx, ]
      paste0(
        "ID: ", point[[id_column]], "\n",
        "Longitude: ", round(point$lon, 6), "\n",
        "Latitude: ", round(point$lat, 6), "\n",
        if (rv$selected_point %in% rv$modified_ids) "Status: MODIFIED" else "Status: Original"
      )
    })

    # Coordinate input UI
    output$coord_inputs <- shiny::renderUI({
      if (is.null(rv$selected_point)) {
        return(shiny::helpText("Select a point first"))
      }

      idx <- which(rv$data[[id_column]] == rv$selected_point)
      if (length(idx) == 0) return(NULL)

      point <- rv$data[idx, ]

      shiny::tagList(
        shiny::numericInput("edit_lon", "Longitude:",
                            value = point$lon, step = 0.000001),
        shiny::numericInput("edit_lat", "Latitude:",
                            value = point$lat, step = 0.000001)
      )
    })

    # Apply manual coordinates
    shiny::observeEvent(input$apply_coords, {
      if (is.null(rv$selected_point)) {
        shiny::showNotification("No point selected", type = "error")
        return()
      }

      idx <- which(rv$data[[id_column]] == rv$selected_point)
      if (length(idx) == 0) return()

      # Update coordinates
      rv$data$lon[idx] <- input$edit_lon
      rv$data$lat[idx] <- input$edit_lat

      # Update geometry
      new_geom <- sf::st_sfc(sf::st_point(c(input$edit_lon, input$edit_lat)), crs = 4326)
      sf::st_geometry(rv$data)[idx] <- new_geom

      # Track modification
      if (!rv$selected_point %in% rv$modified_ids) {
        rv$modified_ids <- c(rv$modified_ids, rv$selected_point)
      }

      # Add to history
      add_to_history()

      shiny::showNotification("Coordinates applied", type = "message")
    })

    # Data table
    output$data_table <- DT::renderDT({
      data_df <- rv$data
      sf::st_geometry(data_df) <- NULL

      DT::datatable(
        data_df,
        editable = list(target = "cell", disable = list(columns = c(0))),
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          id_column,
          target = 'row',
          backgroundColor = DT::styleEqual(
            rv$modified_ids,
            rep('#FFF3CD', length(rv$modified_ids))
          )
        )
    })

    # Handle table edits
    shiny::observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit

      idx <- info$row
      col <- info$col + 1  # DT is 0-indexed
      new_value <- info$value

      col_name <- names(rv$data)[col]

      # Only allow editing lon/lat
      if (col_name %in% c("lon", "lat")) {
        rv$data[idx, col_name] <- as.numeric(new_value)

        # Update geometry
        new_geom <- sf::st_sfc(
          sf::st_point(c(rv$data$lon[idx], rv$data$lat[idx])),
          crs = 4326
        )
        sf::st_geometry(rv$data)[idx] <- new_geom

        # Track modification
        point_id <- rv$data[[id_column]][idx]
        if (!point_id %in% rv$modified_ids) {
          rv$modified_ids <- c(rv$modified_ids, point_id)
        }

        # Add to history
        add_to_history()

        shiny::showNotification(
          paste("Updated", col_name, "for point", point_id),
          type = "message"
        )
      } else {
        shiny::showNotification(
          "Only longitude and latitude can be edited here",
          type = "warning"
        )
      }
    })

    # Modification log
    output$log_table <- DT::renderDT({
      if (length(rv$modified_ids) == 0) {
        return(data.frame(Message = "No modifications yet"))
      }

      modified_data <- rv$data[rv$data[[id_column]] %in% rv$modified_ids, ]
      original_subset <- original_data[original_data[[id_column]] %in% rv$modified_ids, ]

      log_df <- data.frame(
        ID = modified_data[[id_column]],
        Original_Lon = sf::st_coordinates(original_subset)[, 1],
        Original_Lat = sf::st_coordinates(original_subset)[, 2],
        New_Lon = modified_data$lon,
        New_Lat = modified_data$lat,
        Delta_Lon = modified_data$lon - sf::st_coordinates(original_subset)[, 1],
        Delta_Lat = modified_data$lat - sf::st_coordinates(original_subset)[, 2]
      )

      # Calculate distance moved
      log_df$Distance_m <- apply(log_df, 1, function(row) {
        p1 <- sf::st_point(c(row["Original_Lon"], row["Original_Lat"]))
        p2 <- sf::st_point(c(row["New_Lon"], row["New_Lat"]))
        sf::st_distance(sf::st_sfc(p1, crs = 4326), sf::st_sfc(p2, crs = 4326))[1]
      })

      DT::datatable(log_df, options = list(pageLength = 15)) %>%
        DT::formatRound(columns = 2:7, digits = 6) %>%
        DT::formatRound(columns = 8, digits = 2)
    })

    # Download log
    output$download_log <- shiny::downloadHandler(
      filename = function() {
        paste0("coordinate_corrections_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (length(rv$modified_ids) > 0) {
          modified_data <- rv$data[rv$data[[id_column]] %in% rv$modified_ids, ]
          original_subset <- original_data[original_data[[id_column]] %in% rv$modified_ids, ]

          log_df <- data.frame(
            ID = modified_data[[id_column]],
            Original_Lon = sf::st_coordinates(original_subset)[, 1],
            Original_Lat = sf::st_coordinates(original_subset)[, 2],
            New_Lon = modified_data$lon,
            New_Lat = modified_data$lat,
            Delta_Lon = modified_data$lon - sf::st_coordinates(original_subset)[, 1],
            Delta_Lat = modified_data$lat - sf::st_coordinates(original_subset)[, 2]
          )

          write.csv(log_df, file, row.names = FALSE)
        }
      }
    )

    # Statistics
    output$stats <- shiny::renderText({
      paste0(
        "Total points: ", nrow(rv$data), "\n",
        "Modified: ", length(rv$modified_ids), "\n",
        "Unmodified: ", nrow(rv$data) - length(rv$modified_ids)
      )
    })

    # Undo
    shiny::observeEvent(input$undo, {
      if (rv$history_position > 1) {
        rv$history_position <- rv$history_position - 1
        rv$data <- rv$history[[rv$history_position]]

        # Recalculate modified IDs
        rv$modified_ids <- character(0)
        for (i in 1:nrow(rv$data)) {
          if (!identical(
            sf::st_coordinates(rv$data[i, ]),
            sf::st_coordinates(original_data[i, ])
          )) {
            rv$modified_ids <- c(rv$modified_ids, rv$data[[id_column]][i])
          }
        }

        shiny::showNotification("Undo successful", type = "message")
      } else {
        shiny::showNotification("Nothing to undo", type = "warning")
      }
    })

    # Redo
    shiny::observeEvent(input$redo, {
      if (rv$history_position < length(rv$history)) {
        rv$history_position <- rv$history_position + 1
        rv$data <- rv$history[[rv$history_position]]

        # Recalculate modified IDs
        rv$modified_ids <- character(0)
        for (i in 1:nrow(rv$data)) {
          if (!identical(
            sf::st_coordinates(rv$data[i, ]),
            sf::st_coordinates(original_data[i, ])
          )) {
            rv$modified_ids <- c(rv$modified_ids, rv$data[[id_column]][i])
          }
        }

        shiny::showNotification("Redo successful", type = "message")
      } else {
        shiny::showNotification("Nothing to redo", type = "warning")
      }
    })

    # Reset selected point
    shiny::observeEvent(input$reset_point, {
      if (is.null(rv$selected_point)) {
        shiny::showNotification("No point selected", type = "error")
        return()
      }

      idx <- which(rv$data[[id_column]] == rv$selected_point)
      if (length(idx) == 0) return()

      # Find original
      orig_idx <- which(original_data[[id_column]] == rv$selected_point)
      if (length(orig_idx) > 0) {
        rv$data[idx, ] <- original_data[orig_idx, ]
        rv$modified_ids <- setdiff(rv$modified_ids, rv$selected_point)

        add_to_history()
        shiny::showNotification("Point reset to original", type = "message")
      }
    })

    # Reset all
    shiny::observeEvent(input$reset_all, {
      shiny::showModal(shiny::modalDialog(
        title = "Reset All Points",
        "Are you sure you want to reset ALL points to their original coordinates?",
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_reset_all", "Yes, Reset All",
                              class = "btn-danger")
        )
      ))
    })

    shiny::observeEvent(input$confirm_reset_all, {
      rv$data <- original_data
      rv$modified_ids <- character(0)
      rv$selected_point <- NULL
      add_to_history()
      shiny::removeModal()
      shiny::showNotification("All points reset", type = "message")
    })

    # Done button
    shiny::observeEvent(input$done, {
      shiny::stopApp(rv$data)
    })
  }

  # Run the app
  result <- shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Edit Spatial Points",
                                                                      width = 1400,
                                                                      height = 900))

  # Transform back to original CRS if needed
  if (sf::st_crs(result) != original_crs) {
    result <- sf::st_transform(result, original_crs)
  }

  # Remove internal columns
  result$lon <- NULL
  result$lat <- NULL
  if (".internal_id" %in% names(result)) {
    result$.internal_id <- NULL
  }

  # Return only modified points if requested
  if (return_modified && length(rv$modified_ids) > 0) {
    result <- result[result[[id_column]] %in% rv$modified_ids, ]
  }

  return(result)
}
