#' Visualize Airport delays
#' 
#' This function creates a plot that visualizes the mean delay of  fights for different airports by longitude and latitude
#'
#' @name visualize_airport_delays
#'
#' @return Returns a plot
#'
#' @export
#' 
#' @import nycflights13
#' @import dplyr 
#' 
#' @examples
#' library(BonusLab)
#' plot <- BonusLab::visualize_airport_delays()


visualize_airport_delays <- function(){
  
  data(flights, package="nycflights13", envir = environment())
  data(airports, package="nycflights13", envir = environment())

  # plot 1 - arrival delays
  # join the flights and airports dataset on the destination airport code
  airport_arr_delays <- flights %>%
    dplyr::left_join(airports, by = c("dest" = "faa")) %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(mean_delay = mean(arr_delay, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              lon = mean(lon, na.rm = TRUE))
  
  # Create the plot
  plot1 <- ggplot2::ggplot(airport_arr_delays, aes(x = lon, y = lat, size = mean_delay, color = mean_delay)) +
    geom_point() +
    scale_size_continuous(range = c(2, 10)) +
    scale_color_gradient(low = "black", high = "red") +
    labs(title = "Mean Arrival Flight Delay by Airport",
         x = "Longitude",
         y = "Latitude",
         size = "Mean Delay")
  
  
  # plot 2 - departure delays
  # join the flights and airports dataset on the destination airport code
  airport_dep_delays <- flights %>%
    dplyr::left_join(airports, by = c("dest" = "faa")) %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(mean_delay = mean(dep_delay, na.rm = TRUE),
              lat = mean(lat, na.rm = TRUE),
              lon = mean(lon, na.rm = TRUE))
  
  # Create the plot
  plot2 <- ggplot2::ggplot(airport_dep_delays, aes(x = lon, y = lat, size = mean_delay, color = mean_delay)) +
    geom_point() +
    scale_size_continuous(range = c(2, 10)) +
    scale_color_gradient(low = "black", high = "red") +
    labs(title = "Mean Departure Flight Delay by Airport",
         x = "Longitude",
         y = "Latitude",
         size = "Mean Delay")
  
  return(list(plot1,plot2))
}

# devtools::install_github("BirgitKarlhuber/BonusLab", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)


