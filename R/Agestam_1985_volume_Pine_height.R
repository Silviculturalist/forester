#' Volume function for Pine from Agestam (1985), including height.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: T1
#'
#' Number of plots: 37
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.031
#'
#' sf/standard deviation about the mean: 6.8\%
#'
#'
#' @param basal_area_Pine_m2_ha Basal area over bark Pine, m2/ha.
#' @param dominant_height_Pine_m Dominant height of Pine stems.
#' @param stems_Pine_ha Number of Pine stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Pine m3sk/ha.
#' @export
Agestam_1985_volume_Pine_height <- function(
  basal_area_Pine_m2_ha,
  dominant_height_Pine_m,
  stems_Pine_ha,
  latitude
){
  return(
    exp(
      +1.0509*log(basal_area_Pine_m2_ha)+
      +0.8074*log(dominant_height_Pine_m*10)+
      -0.04356*log(stems_Pine_ha)+
      +0.5957*log(latitude)+
        -4.4313
    )
  )
}
