#' Volume function for Pine from Agestam (1985), including SI.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: T2
#'
#' Number of plots: 141
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.038
#'
#' sf/standard deviation about the mean: 7.4\%
#'
#'
#' @param basal_area_Pine_m2_ha Basal area over bark Pine, m2/ha.
#' @param SI_Pine_m Site Index H100 of Pine, meters,  from Hägglund 1974.
#' @param age_at_breast_height_Pine Age at breast height of Pine stems.
#' @param stems_Pine_ha Pine stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Pine m3sk/ha.
#' @export
#'
#' @examples
Agestam_1985_volume_Pine_SI <- function(
  basal_area_Pine_m2_ha,
  SI_Pine_m,
  age_at_breast_height_Pine,
  stems_Pine_ha,
  latitude
){
  SI_Pine <- SI_Pine_m*10
return(
  exp(
    +1.0772*log(basal_area_Pine_m2_ha)+
    +0.7859*log(SI_Pine)+
    +0.3131*log(age_at_breast_height_Pine)+
    -7.4681*(1/age_at_breast_height_Pine)+
    -0.06444*log(stems_Pine_ha)+
    +0.4741*log(latitude)+
    -5.0965
  )
)

}
