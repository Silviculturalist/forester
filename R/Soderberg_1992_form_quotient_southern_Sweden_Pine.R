#' Form quotient for Scots Pine in southern Sweden, from Soderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 49.
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
#' Multiple correlation coefficient R = 0.88
#'
#' Spread about the function sf = 0.147
#'
#' sf/Spread about the mean = 0.47
#'
#' Number of observations = 4628
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_Spruce Basal Area m2 of Norway Spruce
#' @param Basal_area_Birch Basal Area m2 of Birch
#' @param Basal_area_plot Basal area m2 on plot
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
Soderberg_1992_form_quotient_southern_Sweden_Pine <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  diameter_largest_tree_on_plot_cm,
  total_age_stand,
  Basal_area_Pine,
  Basal_area_Spruce,
  Basal_area_Birch,
  Basal_area_plot,
  latitude,
  altitude,
  divided_plot=0,
  county
){

  BA_quotient_Pine <- Basal_area_Pine/Basal_area_plot
  BA_quotient_Spruce <- Basal_area_Spruce/Basal_area_plot
  BA_quotient_Birch <- Basal_area_Birch/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.24722E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.96476E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.64050E-2*total_age_stand+
        -0.33916E-4*(total_age_stand^2)+
        +0.16113E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.57111E-2*altitude+
        +0.98668E-4*latitude*altitude+
        +0.17639E0*diameter_quotient+
        -0.30930E0*(diameter_quotient^2)+
        +0.18507E0*BA_quotient_Pine+
        +0.27249E0*BA_quotient_Spruce+
        +0.12120E0*BA_quotient_Birch+
        +0.21324E-1*south_eastern_county+
        -0.62357E-1*divided_plot+
        -0.19831E0*close_to_coast+
        +0.19624E1+
        +0.01080 #correction for logarithmic bias, appendix 5.
    )
  )


}
