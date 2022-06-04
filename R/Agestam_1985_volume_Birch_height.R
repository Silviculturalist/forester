#' Volume function for Birch from Agestam (1985), including height.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: B1
#'
#' Number of plots: 50
#'
#' Multiple correlation coefficient: 0.998
#'
#' Standard deviation about the function (sf): 0.069
#'
#' sf/standard deviation about the mean: 6.4\%
#'
#'
#'
#' @param basal_area_Birch_m2_ha Basal area over bark Pine, m2/ha.
#' @param dominant_height_Birch_m Dominant height of Birch stems, m2 /ha.
#' @param stems_ha_Birch Number of Birch stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Birch m3sk/ha.
#' @export
Agestam_1985_volume_Birch_height <- function(
  basal_area_Birch_m2_ha,
  dominant_height_Birch_m,
  stems_ha_Birch,
  latitude
){

  return(
    exp(
      +1.0419*log(basal_area_Birch_m2_ha)+
      +0.9020*log(dominant_height_Birch_m)+
      +1.2264*(1/stems_ha_Birch)+
      +0.6799*log(latitude)+
        -5.5828
    )
  )
}
