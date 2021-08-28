#' Form quotient for Broadleaves in northern or central Sweden, from Soderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 54.
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
#' *Värmland
#'
#' @details
#' Multiple correlation coefficient R = 0.85
#'
#' Spread about the function sf = 0.188
#'
#' sf/Spread about the mean = 0.54
#'
#' Number of observations = 318
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast Distance to coast, km. e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
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
Soderberg_1992_form_quotient_northern_central_Sweden_Broadleaves <- function(
  SI100_Pine,
  distance_to_coast,
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
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.12623E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.42804E-2*total_age_stand+
        -0.25316E-4*(total_age_stand^2)+
        +0.16703E-2*SI100_Pine*10+ #SI should be in dm.
        -0.32185E-1*latitude+
        -0.27431E-4*altitude+
        -0.85686E-1*diameter_quotient+
        +0.68224E-1*BA_quotient_Spruce+
        -0.51989E-1*BA_quotient_Betula+
        -0.77704E-1*divided_plot+
        +0.13794E0*close_to_coast+
        +0.39069E1+
        +((0.188^2)/2) #Baskerville 1972, logarithmic correction was not included in appendix 5.
    )
  )


}
