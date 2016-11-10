#' Title
#'
#' @param data name of the data frame containing information to plot.
#' @param lat name of column containing the decimal latitude
#' @param lon name of column containing the decimal longitude
#' @param type the type used for conditioning.
#' @param maptype the ggmap map type.
#' @param source the ggmap source type.
#' @param pollutant Name of the column to plot on map (numeric).
#' @param statistic Not currently used
#' @param xlim map extent longitude limits.
#' @param ylim map extent latitude limits.
#' @param col the colour of the plot symbols which varies depending on the value of \code{pollutant}.
#' @param range the range size of the ploy symbols used.
#' @param annotate the column name that will be used for plotting the value.
#' @param fontface the type of font to be used with \code{annotate}.
#' @param ... other arguments supplied to ggmap e.g. \code{zoom}.
#' @import ggmap ggplot2 dplyr openair
#'
#' @return Returns the \code{ggmap} plot object that can be used for further manipulation.
#' @export
#'
#' @examples
openairMap <- function(data, lat = "latitude", lon = "longitude",
                       type = "default", maptype = "terrain",
                       source = "google",
                       pollutant = "o3",
                       statistic = "mean",
                       xlim = NA, ylim = NA,
                       col = "jet", range = c(1, 10),
                       annotate = NA,
                       fontface = "plain", ...) {

  # change the column names to make function easier to use
  data <- rename_(data, "lon" = lon,
                  "lat" = lat,
                  "pollutant" = pollutant)


  # column for labels
  if (!is.na(annotate)) {
    data$LAB <- round(data[["pollutant"]])
  }

  # map extent
  if (is.na(xlim[1])) {
    xmin <- min(data[["lon"]])
    xmax <- max(data[["lon"]])
    xlim <- c(xmin, xmax)
  } else {
    xmin <- xlim[1]
    xmax <- xlim[2]
  }

  if (is.na(ylim[1])) {
    ymin <- min(data[["lat"]])
    ymax <- max(data[["lat"]])
    ylim <- c(ymin, ymax)
  } else {
    ymin <- ylim[1]
    ymax <- ylim[2]

  }

  plt <- ggmap(get_map(location = c(xmin, ymin, xmax, ymax),
                       maptype = maptype,
                       source = source,
                       ...))

  # various combinations
  if (!is.na(col) && is.na(range[1])) {
    plt <- plt +
      geom_point(aes(x = lon, y = lat,
                     colour = pollutant), data = data)
  }

  if (!is.na(range[1]) && !is.na(col)) {
    plt <- plt +
      geom_point(aes(x = lon, y = lat,
                     size = pollutant, colour = pollutant), data = data)
  }

  if (is.na(range[1]) && !is.na(col)) {
    plt <- plt +
      geom_point(aes(x = lon, y = lat,
                     colour = pollutant), data = data)
  }

  if (is.na(range[1]) && is.na(col)) {
    plt <- plt +
      geom_point(aes(x = lon, y = lat), data = data)
  }




  # if colour legend used
  if (!is.na(col))
  plt <- plt +
    scale_color_gradientn(colors = openColours(col)) +
    labs(color = quickText(pollutant))

  if (!is.na(range[1]))
    plt <- plt + scale_size(range = range) +
    labs(size = quickText(pollutant))

    plt <- plt +
    xlab("") +
    ylab("")

  if (!is.na(annotate))
    plt <- plt + geom_text(aes(x = lon, y = lat,
                    label = LAB),
              data = data, fontface = fontface)

  # if there is a type
  if (!"default" %in% type) {
    if (length(type) == 1L)
      plt <- plt + facet_wrap(reformulate(type), labeller = label_parsed)

    if (length(type) == 2L)
      plt <- plt + facet_grid(paste(type[2], "~", type[1]), labeller = label_parsed)

  }


  print(plt)

  return(plt)


}

