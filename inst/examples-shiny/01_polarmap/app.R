
polar_data <- openairmaps::polar_data
`%>%` <- openairmaps::`%>%`

# get list of sites in data
sites <- unique(polar_data$site)

# create initial map
map <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::setView(lng = -0.213492,
                   lat = 51.49548,
                   zoom = 13)

# create user interface
ui <-
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(
        "pollutant",
        "Select Pollutant of Interest",
        choices = c("NOx" = "nox", "NO2" = "no2", "PM2.5" = "pm2.5", "PM10" = "pm10")
      ),
      shiny::selectInput(
        "sites",
        "Select Sites to Plot",
        choices = sites,
        selected = sites,
        multiple = TRUE
      ),
      bslib::input_task_button("button", "View Polar Plots")
    ),
    leaflet::leafletOutput("map")
  )

# define server-side functionality
server <- function(input, output, session) {
  output$map <- leaflet::renderLeaflet(map)

  observeEvent(input$button, {
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("polarmarkers")

    for (i in seq_along(input$sites)) {
      thedata <- polar_data[polar_data$site == input$sites[i], ]

      leaflet::leafletProxy("map") %>%
        openairmaps::addPolarMarkers(
          data = thedata,
          pollutant = input$pollutant,
          layerId = input$sites[i],
          cols = "turbo",
          lng = "lon",
          lat = "lat",
          group = "polarmarkers"
        )
    }
  })

}

# run app
shiny::shinyApp(ui, server)
