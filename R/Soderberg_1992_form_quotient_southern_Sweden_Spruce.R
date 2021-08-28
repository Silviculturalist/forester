#' Form quotient for Norway Spruce in southern Sweden, from Soderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 51.
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
#' Multiple correlation coefficient R = 0.91
#'
#' Spread about the function sf = 0.152
#'
#' sf/Spread about the mean = 0.41
#'
#' Number of observations = 7467
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 / ha of Scots Pine
#' @param Basal_area_Spruce Basal Area m2 / ha of Norway Spruce
#' @param Basal_area_Birch Basal Area m2 / ha of Birch
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#'
#' @md
#'
#' @return Form quotient of tree.
#' @export
#'
#' @examples
Soderberg_1992_form_quotient_southern_Sweden_Spruce <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Pine,
  Basal_area_Spruce,
  Basal_area_Birch,
  latitude,
  altitude,
  divided_plot
){

  BA_quotient_Pine <- Basal_area_Pine/(Basal_area_Pine+Basal_area_Spruce+Basal_area_Birch)
  BA_quotient_Spruce <- Basal_area_Spruce/(Basal_area_Pine+Basal_area_Spruce+Basal_area_Birch)
  BA_quotient_Birch <- Basal_area_Birch/(Basal_area_Pine+Basal_area_Spruce+Basal_area_Birch)
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.20201E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.16550E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.39114E-2*total_age_stand+
        -0.24311E-4*(total_age_stand^2)+
        +0.10805E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        +0.15779E-2*altitude+
        -0.26825E-4*latitude*altitude+
        -0.25400E0*diameter_quotient+
        +0.10089E0*BA_quotient_Pine+
        +0.27052E0*BA_quotient_Spruce+
        +0.64203E-1*BA_quotient_Birch+
        -0.53712E-1*divided_plot+
        -0.79867E-1*close_to_coast+
        +0.23991E1+
        +0.01156 #correction for logarithmic bias, appendix 5.
    )
  )


}
