#' Height function for Norway Spruce in northern or Sweden from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 36.
#'
#' @description
#' \strong{Applicable counties:}
#'
#' *Norrbottens Lappmark
#' *Norrbottens kustland
#' *Västerbottens lappmark
#' *Västerbottens kustland
#' *Västernorrland - Ångermanlands landskap
#' *Västernorrland - Medelpads landskap
#' *Jämtland - Jämtlands landskap
#' *Jämtland - Härjedalens landskap
#' *Kopparberg - Sälen-Idre.
#' *Kopparberg - övriga
#' *Gävleborg - Hälsinglands landskap
#' *Gävleborg - övriga
#' *Kopparberg - övriga
#' *Värmland
#'
#' @details
#' Multiple correlation coefficient R = 0.94
#'
#' Spread about the function sf = 0.148
#'
#' sf/Spread about the mean = 0.33
#'
#' Number of observations = 7470
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_Spruce Basal Area m2 of Spruce
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#'
#' @return Height of tree, m.
#' @export
#'
#' @examples
Soderberg_1992_height_northern_central_Sweden_Spruce <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Pine,
  Basal_area_Spruce,
  Basal_area_plot,
  latitude,
  altitude,
  divided_plot=0
){

  BA_quotient_Pine <- Basal_area_Pine/Basal_area_plot
  BA_quotient_Spruce <- Basal_area_Spruce/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.28663E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.47831E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.31669E-2*total_age_stand+
        -0.16854E-4*(total_age_stand^2)+
        +0.10855E-2*SI100_Pine*10+ #SI should be in dm.
        -0.99681E-2*latitude+
        +0.51262E-3*altitude+
        -0.12449E-4*latitude*altitude+
        -0.19831E0*diameter_quotient+
        +0.60923E-1*BA_quotient_Pine+
        +0.90784E-1*BA_quotient_Spruce+
        -0.30688E-1*divided_plot+
        -0.62548E-1*close_to_coast+
        +0.65200E1+
        +0.01095 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}
