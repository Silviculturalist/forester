#' Double bark thickness in southern Sweden for Scots Pine from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 63.
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
#' Multiple correlation coefficient R = 0.86
#'
#' Spread about the function sf = 0.238
#'
#' sf/Spread about the mean = 0.50
#'
#' Number of observations = 4628
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_Birch Basal area m2 of Birch.
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#' @param county County name.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_southern_Sweden_Pine <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Pine,
  Basal_area_Birch,
  Basal_area_plot,
  latitude,
  altitude,
  divided_plot=0,
  county
){

  BA_quotient_Birch <- Basal_area_Birch/Basal_area_plot
  BA_quotient_Pine <- Basal_area_Pine/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.38360E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.13442E5*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.20965E-2*total_age_stand+
        -0.88795E-5*(total_age_stand^2)+
        -0.74698E-3*SI100_Pine*10+ #SI should be in dm.
        +0.10185E-1*altitude+
        -0.17023E-3*latitude*altitude+
        -0.24281E-1*BA_quotient_Pine+
        -0.49230E-1*BA_quotient_Birch+
        +0.57067E-1*south_eastern_county+
        +0.24619E-1*divided_plot+
        +0.19141E0*close_to_coast+
        +0.47240E1+
        +0.02832 #correction for logarithmic bias, appendix 5.

    )
  )
}
