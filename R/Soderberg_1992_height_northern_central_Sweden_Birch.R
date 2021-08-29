#' Height function for Birch in northern or Sweden from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 38.
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
#' Multiple correlation coefficient R = 0.90
#'
#' Spread about the function sf = 0.158
#'
#' sf/Spread about the mean = 0.43
#'
#' Number of observations = 1747
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
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
Soderberg_1992_height_northern_central_Sweden_Birch <- function(
  SI100_Pine,
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


  return(
    exp(
      -0.26607E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.71415E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.32789E-2*total_age_stand+
        -0.22514E-4*(total_age_stand^2)+
        +0.85255E-3*SI100_Pine*10+ #SI should be in dm.
        -0.18462E-1*latitude+
        -0.72180E-5*latitude*altitude+
        -0.39250E0*diameter_quotient+
        +0.76500E-1*(diameter_quotient^2)+
        -0.74398E-1*BA_quotient_Pine+
        -0.22539E-1*BA_quotient_Spruce+
        -0.35918E-1*divided_plot+
        +0.72446E1+
        +0.01248 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}
