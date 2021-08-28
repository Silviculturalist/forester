#' Form quotient for Scots Pine in central Sweden, from Soderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 48.
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
#' Multiple correlation coefficient R = 0.92
#'
#' Spread about the function sf = 0.123
#'
#' sf/Spread about the mean = 0.38
#'
#' Number of observations = 2275
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_Spruce Basal Area m2 of Norway Spruce
#' @param Basal_area_Birch Basal Area m2 of Birch
#' @param Basal_area_plot Basal area m2 on plot.
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
Soderberg_1992_form_quotient_central_Sweden_Pine <- function(
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
  BA_quotient_Spruce <- Basal_area_Spruce/Basal_area_plot
  BA_quotient_Birch <- Basal_area_Birch/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.25627E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.77018E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.47410E-2*total_age_stand+
        -0.21793E-4*(total_age_stand^2)+
        +0.13003E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        +0.81039E-1*latitude+
        +0.75022E-2*altitude+
        -0.12694E-3*latitude*altitude+
        -0.50469E0*diameter_quotient+
        +0.10610E0*(diameter_quotient^2)+
        +0.15691E0*BA_quotient_Pine+
        +0.17465E0*BA_quotient_Spruce+
        +0.17342E0*BA_quotient_Birch+
        -0.48782E-1*divided_plot+
        -0.83240E-1*close_to_coast+
        -0.23076E1+
        +0.00756 #correction for logarithmic bias, appendix 5.
    )
  )


}
