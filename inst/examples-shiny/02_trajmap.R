
library(shiny)
library(bslib)
library(openairmaps)

dates <- unique(traj_data$date)

# create initial map
map <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::setView(lng = -10, lat = 60, zoom = 4)

# create user interface
ui <-
  bslib::page_fillable(bslib::card(
    bslib::card_header("Trajectory Map"),
    bslib::card_body(leaflet::leafletOutput("map")),
    bslib::card_footer(
      shiny::sliderInput(
        timezone = "GMT",
        width = "100%",
        timeFormat = "%F",
        "slider",
        "Date",
        min = min(dates),
        max = max(dates),
        value = range(dates)
      )
    )
  ))

# define server-side functionality
server <- function(input, output, session) {
  output$map <- leaflet::renderLeaflet(map)

  observeEvent(input$slider, {
    leaflet::leafletProxy("map") %>%
      leaflet::removeMarker(as.character(dates)) %>%
      leaflet::removeShape(as.character(dates))

    thedates <-
      dates[dates > min(input$slider) & dates < max(input$slider)]

    for (i in seq_along(thedates)) {
      thedata <- traj_data[traj_data$date == thedates[i], ]
      leaflet::leafletProxy("map") %>%
        addTrajPaths(layerId = as.character(thedates[i]),
                     data = thedata)
    }
  })
}

# run app
shinyApp(ui, server)
