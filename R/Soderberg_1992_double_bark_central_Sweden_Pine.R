#' Double bark thickness in central Sweden for Scots Pine from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 62.
#'
#' @description
#' \strong{Applicable counties:}
#'
#' *Kopparberg - övriga
#' *Gävleborg - Hälsinglands landskap
#' *Gävleborg - övriga
#' *Kopparberg - övriga
#' *Värmland
#'
#' @details
#' Multiple correlation coefficient R = 0.89
#'
#' Spread about the function sf = 0.229
#'
#' sf/Spread about the mean = 0.46
#'
#' Number of observations = 2275
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Spruce Basal Area m2 of Norway Spruce
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_central_Sweden_Pine <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Spruce,
  Basal_area_plot,
  latitude,
  altitude
){

  BA_quotient_Spruce <- Basal_area_Spruce/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.39422E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.14040E5*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.30388E-3*total_age_stand+
        -0.92527E-3*SI100_Pine*10+ #SI should be in dm.
        -0.64192E-1*latitude+
        -0.31573E-3*altitude+
        +0.12632E0*diameter_quotient+
        -0.46079E-1*(diameter_quotient^2)+
        +0.58621E-1*BA_quotient_Spruce+
        -0.94391E-1*close_to_coast+
        +0.86428E1+
        +0.02622 #correction for logarithmic bias, appendix 5. OBSERVE error in appendix!!
    )
  )
}
