#' Volume function for Birch from Agestam (1985), including SI.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: B2
#'
#' Number of plots: 47
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.078
#'
#' sf/standard deviation about the mean: 8.2\%
#'
#'
#' @param basal_area_Birch_m2_ha Basal area over bark Pine, m2/ha.
#' @param age_at_breast_height_Birch Age at breast height for Birch stems.
#' @param SI_Birch_m Site Index for Birch according to Persson (1959)
#' @param stems_ha_Birch Number of Birch stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Birch m3sk/ha.
#' @export
#'
#' @examples
Agestam_1985_volume_Birch_SI <- function(
  basal_area_Birch_m2_ha,
  age_at_breast_height_Birch,
  SI_Birch_m,
  stems_ha_Birch,
  latitude
){
  return(
    exp(
      +1.0789*log(basal_area_Birch_m2_ha)+
      +0.9233*log(SI_Birch_m)+
      +0.4574*log(age_at_breast_height_Birch)+
      -0.04264*log(stems_ha_Birch)+
      +0.6905*log(latitude)+
      -7.3526
    )
  )
}
