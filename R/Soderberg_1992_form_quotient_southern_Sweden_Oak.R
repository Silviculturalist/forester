#' Form quotient for Oak in southern Sweden, from Soderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 57.
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
#' Spread about the function sf = 0.198
#'
#' sf/Spread about the mean = 0.57
#'
#' Number of observations = 292
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Spruce Basal Area m2 of Norway Spruce
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#' @param county County name.
#'
#' @md
#'
#' @return Form quotient of tree.
#' @export
#'
#' @examples
Soderberg_1992_form_quotient_southern_Sweden_Oak <- function(
  SI100_Pine,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Spruce,
  Basal_area_plot,
  latitude,
  altitude,
  divided_plot=0,
  county
){


  BA_quotient_Spruce <- Basal_area_Spruce/(Basal_area_plot)
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.24454E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.77370E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.25633E-2*total_age_stand+
        -0.16976E-4*(total_age_stand^2)+
        +0.13153E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.56851E-5*latitude*altitude+
        -0.29397E0*diameter_quotient+
        +0.82213E-1*BA_quotient_Spruce+
        +0.26924E0*south_eastern_county+
        -0.65403E-2*region5+
        -0.77845E-1*divided_plot+
        +0.25409E1+
        +0.01960 #correction for logarithmic bias, appendix 5.

    )
  )


}
