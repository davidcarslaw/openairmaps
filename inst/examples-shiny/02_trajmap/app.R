
traj_data <- openairmaps::traj_data
`%>%` <- magrittr::`%>%`
dates <- unique(traj_data$date)

# create initial map
map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(provider = leaflet::providers$CartoDB.Voyager) %>%
  leaflet::setView(lng = -10, lat = 60, zoom = 4)

# create user interface
ui <-
  bslib::page_fillable(bslib::card(
    bslib::card_header("Trajectory Map"),
    bslib::card_body(leaflet::leafletOutput("map")),
    bslib::card_footer(
      shiny::div(
        style = "margin:auto;width:80%;",
        shiny::sliderInput(
          timezone = "+0000",
          width = "100%",
          timeFormat = "%F",
          "slider",
          "Range of Arrival Dates",
          min = min(dates),
          max = max(dates),
          value = range(dates)
        )
      )
    )
  ))

# define server-side functionality
server <- function(input, output, session) {
  output$map <- leaflet::renderLeaflet(map)

  observeEvent(input$slider, {
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("trajpaths")

    thedates <-
      dates[dates >= min(input$slider) & dates <= max(input$slider)]

    thedata <- traj_data[traj_data$date %in% thedates, ]
    leaflet::leafletProxy("map") %>%
      openairmaps::addTrajPaths(layerId = "traj",
                                data = thedata,
                                group = "trajpaths")
  })
}

# run app
shiny::shinyApp(ui, server)
