airtemps <- c(212, 30.3, 78, 32)

celsius1 <- (airtemps[1]-32)*5/9
celsius2 <- (airtemps[2]-32)*5/9
celsius3 <- (airtemps[3]-32)*5/9


# pound apostrophe continues comment after carriage return
#' Convert Fahrenheit to Celsius
#'
#' @param fahr The temperature in Fahrenheit
#' @keywords temperature conversion
#'
#' @return The temperature in Celsius
#' @export
#'
#' @examples
#' f_to_c(32)
#' f_to_c(32,212,72)
f_to_c <- function(fahr) {
  (fahr - 32) * 5 / 9
}

f_to_c(airtemps)

c_to_f <- function(cels) {
  (cels * 9 /5) + 32
}

c_to_f(f_to_c(airtemps))


convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  
  list(fahr = fahr, celsius = celsius, kelvin = kelvin)
}

temps_df <- data.frame(convert_temps(seq(-100,100,10)))
temps_df

library(ggplot2)

custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = 'Helvetica', color = 'gray30', size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = 'bold'),
    panel.background = ggplot2::element_blank(),
    legend.position  = 'right',
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', size = .25),
    legend.key       = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_blank()
  )
}

ggplot(temps_df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
  geom_point() +
  custom_theme(20)


scatterplot <- function(df, point_size = 2, font_size=9) {
  ggplot(df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
    geom_point(size=point_size) +
    custom_theme(font_size)
}

scatterplot(temps_df)
scatterplot(temps_df,5,20)


#### creating a package

library(devtools)
library(usethis)
library(roxygen2)

setwd('..')
create_package("mytools")
