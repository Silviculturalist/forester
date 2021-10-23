#' Height function for Broadleaves in northern or central Sweden from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd, formhöjd och barktjocklek för enskilda träd. / Functions for forest management: Height, form height and bark thickness of individual trees. Report 52. Dept. of Forest Survey. Swedish University of Agricultural Sciences. ISSN 0348-0496. p. 40.
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
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been distinguished
#' by Sylvén 1917. The distribution of the different forms of Scots Pine is assumed
#' to depend on the expansion-history of the Pine, according to which the Pine
#' immigrated both from the north and the south with a transitional zone in central
#' Sweden, where both forms are present. For other species, the change in genotype
#' is more continuous over the country (Kiellander 1974). For Norway Spruce, it is
#' assumed that it has had time for 30-50 generations in northern Sweden, contrasted
#' against only 10-20 in southern Sweden (Kiellander 1966). Therefore the same regional
#' division is used for Norway Spruce as for Scots Pine. For the other species, the regional
#' division is limited to two regions, wherein the northern and central regions have been merged.

#'
#' @details
#' Multiple correlation coefficient R = 0.88
#'
#' Spread about the function sf = 0.191
#'
#' sf/Spread about the mean = 0.48
#'
#' Number of observations = 318
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param diameter_cm Diameter over bark of tree, in cm.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_Spruce Basal Area m2 of Spruce
#' @param Basal_area_plot Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#'
#' @return Height of tree, m.
#' @export
#'
#' @examples
Soderberg_1992_height_northern_central_Sweden_Broadleaves <- function(
  SI100_Pine,
  distance_to_coast_km,
  diameter_cm,
  total_age_stand,
  Basal_area_Spruce,
  Basal_area_plot,
  latitude,
  divided_plot=0
){

  BA_quotient_Spruce <- Basal_area_Spruce/Basal_area_plot
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.14546E3*(1/((diameter_cm*10)+50))+#Diameter cm should be in mm.
        +0.53659E-2*total_age_stand+
        -0.29042E-4*(total_age_stand^2)+
        +0.17639E-2*SI100_Pine*10+ #SI should be in dm.
        -0.34200E-1*latitude+
        +0.75841E-1*BA_quotient_Spruce+
        -0.82953E-1*divided_plot+
        +0.15566E0*close_to_coast+
        +0.70706E1+
        +((0.191^2)/2) #Baskerville 1972, logarithmic correction was not included in appendix 5.
    )
  )
}
