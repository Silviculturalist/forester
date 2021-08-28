#' Form quotient for Scots Pine in northern Sweden, from Soderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 47.
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
#'
#' @details
#' Multiple correlation coefficient R = 0.91
#'
#' Spread about the function sf = 0.129
#'
#' sf/Spread about the mean = 0.42
#'
#' Number of observations = 3832
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
Soderberg_1992_form_quotient_northern_Sweden_Pine <- function(
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
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.24776E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.75785E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.52773E-2*total_age_stand+
        -0.24395E-4*(total_age_stand^2)+
        +0.10773E-2*SI100_Pine*10+ #SI should be in dm.
        -0.15516E-1*latitude+
        -0.43763E-5*latitude*altitude+
        -0.36728E0*diameter_quotient+
        +0.56762E-1*(diameter_quotient^2)
        +0.74321E-1*BA_quotient_Pine+
        -0.52502E-1*divided_plot+
        -0.59471E-1*close_to_coast+
        +0.36491E1+
        +0.00832 #correction for logarithmic bias, appendix 5.
    )
  )


}
