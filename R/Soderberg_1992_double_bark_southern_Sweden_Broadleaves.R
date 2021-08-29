#' Double bark thickness in southern Sweden for Broadleaves from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 69.
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
#' Multiple correlation coefficient R = 0.83
#'
#' Spread about the function sf = 0.298
#'
#' sf/Spread about the mean = 0.55
#'
#' Number of observations = 837
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param total_age_stand Total age of the stand.
#'
#' @return Double bark thickness, mm.
#' @export
#'
#' @examples
Soderberg_1992_double_bark_southern_Sweden_Broadleaves <- function(
  SI100_Pine,
  diameter_cm,
  total_age_stand
){

  return(
    exp(
      -0.34144E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.97900E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.31101E-2*total_age_stand+
        -0.22562E-4*(total_age_stand^2)+
        -0.21013E-2*SI100_Pine*10+ #SI should be in dm.
        +0.45835E1+
        +0.044402 #Baskerville 1972, logarithmic correction was not included in appendix 5.
    )
  )
}
