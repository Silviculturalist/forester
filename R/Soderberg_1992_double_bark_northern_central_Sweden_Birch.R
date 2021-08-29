#' Double bark thickness in northern and central Sweden for Birch from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 66.
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
#' Multiple correlation coefficient R = 0.80
#'
#' Spread about the function sf = 0.293
#'
#' sf/Spread about the mean = 0.60
#'
#' Number of observations = 1747
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_northern_central_Sweden_Birch <- function(
  SI100_Pine,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Pine,
  Basal_area_plot,
  latitude,
  altitude
){

  BA_quotient_Pine <- Basal_area_Pine/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm

  return(
    exp(
      -0.37131E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.13012E5*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.19655E-2*total_age_stand+
        -0.71109E-3*SI100_Pine*10+ #SI should be in dm.
        +0.86881E-2*latitude+
        +0.62991E-5*latitude*altitude+
        +0.17146E0*diameter_quotient+
        +0.18594E0*BA_quotient_pine+
        +0.31740E1+
        +0.04292 #correction for logarithmic bias, appendix 5.
    )
  )
}
