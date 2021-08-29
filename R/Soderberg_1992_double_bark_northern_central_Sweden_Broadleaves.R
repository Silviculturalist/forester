#' Double bark thickness in northern and central Sweden for Broadleaves from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 68.
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
#' Multiple correlation coefficient R = 0.79
#'
#' Spread about the function sf = 0.388
#'
#' sf/Spread about the mean = 0.62
#'
#' Number of observations = 318
#'
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Birch Basal Area m2 of Birch
#' @param Basal_area_plot Basal area m2 on plot.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_northern_central_Sweden_Broadleaves <- function(
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Birch,
  Basal_area_plot,
  divided_plot=0
){

  BA_quotient_Birch <- Basal_area_Birch/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.17562E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.49609E-2*total_age_stand+
        +0.26968E0*diameter_quotient+
        +0.29703E0*BA_quotient_Birch+
        -0.77013E-1*divided_plot+
        +0.86920E-1*close_to_coast+
        +0.28446E1+
        +0.075272 #Baskerville 1972, funktion was not include in appendix 5.
    )
  )
}
