#' Double bark thickness in southern Sweden for Oak from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 71.
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
#' Multiple correlation coefficient R = 0.89
#'
#' Spread about the function sf = 0.217
#'
#' sf/Spread about the mean = 0.44
#'
#' Number of observations = 292
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
Soderberg_1992_double_bark_southern_Sweden_Oak <- function(
  diameter_cm,
  total_age_stand,
  county,
  divided_plot
){

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.29605E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.87235E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.22680E-2*total_age_stand+
        -0.24349E0*south_eastern_county+
        +0.44474E-1*divided_plot+
        +0.39521E1+
        +0.02354 #correction for logarithmic bias, appendix 5.
    )
  )
}
