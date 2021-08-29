#' Double bark thickness in southern Sweden for Beech from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 70.
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
#' Spread about the function sf = 0.255
#'
#' sf/Spread about the mean = 0.52
#'
#' Number of observations = 220
#'
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param county County name.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_southern_Sweden_Beech <- function(
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  county
){

  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.17387E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.22597E-2*total_age_stand+
        +0.16350E0*diameter_quotient+
        -0.26953E0*region5+
        +0.24822E1+
        +0.03251 #correction for logarithmic bias, appendix 5.
    )
  )
}
