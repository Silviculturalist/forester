#' Height function for Broadleaves in southern Sweden from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 41.
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
#' Multiple correlation coefficient R = 0.84
#'
#' Spread about the function sf = 0.195
#'
#' sf/Spread about the mean = 0.54
#'
#' Number of observations = 837
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param diameter_largest_tree_on_plot_cm Diameter over bark of the tree with the greatest diameter on the plot, cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Pine Basal Area m2 of Scots Pine
#' @param Basal_area_Spruce Basal Area m2 of Spruce
#' @param Basal_area_Birch Basal Area m2 of Birch
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#'
#' @return Height of tree, m.
#' @export
#'
#' @examples
Soderberg_1992_height_southern_Sweden_Broadleaves <- function(
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
  BA_quotient_Birch <- Basal_area_Birch/Basal_area_plot
  diameter_quotient <- diameter_cm/diameter_largest_tree_on_plot_cm


  return(
    exp(
      -0.22078E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.53920E4*((1/((diameter_cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.53701E-2*total_age_stand+
        -0.41932E-4*(total_age_stand^2)+
        +0.53968E-3*SI100_Pine*10+ #SI should be in dm.
        -0.10758E-1*altitude+
        +0.18781E-3*latitude*altitude+
        -0.17045E0*diameter_quotient+
        -0.17291E0*BA_quotient_Pine+
        +0.10783E0*BA_quotient_Spruce+
        -0.55868E-1*BA_quotient_Birch+
        -0.51870E-1*divided_plot+
        +0.56569E1+
        +((0.195^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
    )/10 # Return height in meters, not dm.
  )
}
