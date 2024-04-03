
library(shiny)
library(bslib)
library(openairmaps)

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
        "Pollutant",
        choices = c("nox", "no2", "pm2.5", "pm10")
      ),
      shiny::selectInput(
        "sites",
        "Sites",
        choices = sites,
        selected = sites,
        multiple = TRUE
      ),
      bslib::input_task_button("button", "Plot")
    ),
    leaflet::leafletOutput("map")
  )

# define server-side functionality
server <- function(input, output, session) {
  output$map <- leaflet::renderLeaflet(map)

  observeEvent(input$button, {
    leaflet::leafletProxy("map") %>%
      leaflet::removeMarker(layerId = sites)

    for (i in seq_along(input$sites)) {
      thedata <- polar_data[polar_data$site == input$sites[i], ]

      leaflet::leafletProxy("map") %>%
        addPolarMarkers(
          data = thedata,
          pollutant = input$pollutant,
          layerId = input$sites[i],
          cols = "turbo",
          lng = "lon",
          lat = "lat"
        )
    }
  })

}

# run app
shinyApp(ui, server)
