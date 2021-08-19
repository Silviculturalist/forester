#' Natural mortality in stands of Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 60. pp. 206.
#'
#' @details
#' If basal_area_above_bark denser than average basal area cf. [forester::Persson_1992_average_basal_area_Pine()]
#'
#' number of growth periods: 301
#'
#' Coefficient of determination: 0.371
#'
#' Standard deviation about the function (sf): 1.012
#'
#' (sf)/standard deviation about the mean: 79.7%
#'
#' If basal_area_above_bark thinner than average basal area cf. [forester::Persson_1992_average_basal_area_Pine()]
#'
#' number of growth periods: 1968
#'
#' Coefficient of determination: 0.173
#'
#' Standard deviation about the function (sf): 0.981
#'
#' (sf)/standard deviation about the mean: 91.0%
#'
#'
#' @param basal_area_above_bark Basal area over bark, m2/ha
#' @param dominant_height_m Dominant height m
#' @param SI Site Index, m. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param stems Number of stems per ha.
#' @param latitude Latitude, degrees.
#'
#' @return Annual mortality, basal area on bark + 0.01 m2/ha/yr.
#' @export
#'
#' @examples
Persson_1992_natural_mortality_Pine <- function(
  basal_area_above_bark,
  dominant_height_m,
  SI,
  stems,
  latitude
){

  average_BA_1_3 <- Persson_1992_average_basal_area_Pine(dominant_height_m= dominant_height_m,
                                                         SI = SI)*1.3

  ifelse(G>=average_BA_1_3,
         return(
           exp(
             -17.24908+ #including correction for logarithmic bias.
               +1.86053*log(basal_area_above_bark)+
               +1.75182*log(dominant_height_m)+
               +0.44001*log(stems)
           )
         ),
         return(
           exp(
             +15.3390+
             +1.01872*log(dominant_height_m)+
             +0.69317*log(stems)+
             -6.38718*log(latitude)
           )
         )



         )



}
