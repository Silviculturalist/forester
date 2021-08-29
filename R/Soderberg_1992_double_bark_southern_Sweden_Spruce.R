#' Double bark thickness in southern Sweden for Norway Spruce from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 65.
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
#' Multiple correlation coefficient R = 0.83
#'
#' Spread about the function sf = 0.245
#'
#' sf/Spread about the mean = 0.55
#'
#' Number of observations = 7467
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_Birch Basal area m2 of Birch.
#' @param Basal_area_Spruce Basal area m2 of Spruce.
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_southern_Sweden_Spruce <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Pine,
  Basal_area_Spruce,
  Basal_area_Birch,
  Basal_area_plot,
  latitude,
  altitude,
  divided_plot=0
){

  BA_quotient_Pine <- Basal_area_Pine/Basal_area_plot
  BA_quotient_Birch <- Basal_area_Birch/Basal_area_plot
  BA_quotient_Spruce <- Basal_area_Spruce/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.30355E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.13763E5*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
      +0.23539E-4*(total_age_stand^2)+
        -0.13014E-2*SI100_Pine*10+ #SI should be in dm.
        -0.10863E-1*altitude+
        +0.19027E-3*latitude*altitude+
        +0.30230E0*diameter_quotient+
        +0.68055E-1*BA_quotient_Pine+
        -0.10406E0*BA_quotient_Spruce+
        +0.62182E-1*BA_quotient_Birch+
        +0.27539E-1*divided_plot+
        +0.23053E0*close_to_coast+
        +0.36138E1+
        +0.03001 #correction for logarithmic bias, appendix 5.
    )
  )
}
