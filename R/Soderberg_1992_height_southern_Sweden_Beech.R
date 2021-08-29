#' Height function for Beech in southern Sweden from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 42.
#'
#' @description
#' \strong{Applicable counties:}
#'
#' *Stockholm
#' *Södermanland
#' *Uppsala
#' *Östergötland
#' *Kalmar
#' *Västmanland
#' *Blekinge
#' *Kristianstad
#' *Malmöhus
#' *Västra Götaland
#' *Halland
#' *Gotland
#'
#' @details
#' Multiple correlation coefficient R = 0.92
#'
#' Spread about the function sf = 0.161
#'
#' sf/Spread about the mean = 0.41
#'
#' Number of observations = 220
#'
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Spruce Basal Area m2 of Norway Spruce
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#' @param county County name.
#'
#' @md
#'
#' @return Height of tree, m.
#' @export
#'
#' @examples
Soderberg_1992_height_southern_Sweden_Beech <- function(
  diameter_cm,
  total_age_stand,
  Basal_area_Spruce,
  Basal_area_plot,
  latitude,
  altitude,
  divided_plot=0,
  county
){


  BA_quotient_Spruce <- Basal_area_Spruce/(Basal_area_plot)
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.14407E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.72319E-2*total_age_stand+
        -0.27244E-4*(total_age_stand^2)+
        -0.57810E-5*latitude*altitude+
        +0.18040E0*BA_quotient_Spruce+
        +0.18800E0*south_eastern_county+
        -0.18416E0*region5+
        -0.17410E0*divided_plot+
        +0.52974E1+
        +0.01296 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )


}
