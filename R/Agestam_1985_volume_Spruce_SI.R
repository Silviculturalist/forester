#' Volume function for Spruce from Agestam (1985), including SI.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: G2
#'
#' Number of plots: 143
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.060
#'
#' sf/standard deviation about the mean: 8.3%
#'
#' @param basal_area_Spruce_m2_ha Basal area over bark Spruce, m2/ha.
#' @param SI_Spruce Site Index H100 of Spruce, meters,  from Hägglund 1972,1973.
#' @param age_at_breast_height_Spruce Age at breast height of Spruce stems.
#' @param stems_ha_Spruce Spruce stems per hectare.
#' @param stems_ha_other Stems per hectare of other tree species.
#'
#' @return Volume Spruce m3sk/ha.
#' @export
#'
#' @examples
Agestam_1985_volume_Spruce_SI <- function(
  basal_area_Spruce_m2_ha,
  SI_Spruce,
  age_at_breast_height_Spruce,
  stems_ha_Spruce,
  stems_ha_other
){
  return(
    exp(
      +1.2010*log(basal_area_Spruce_m2_ha)+
      +0.6476*log(SI_Spruce*10)+
      +0.1581*log(age_at_breast_height_Spruce)+
      -0.01084*sqrt(stems_ha_Spruce)+
      -0.002148*sqrt(stems_ha_other)+
      -11.1378*(1/age_at_breast_height_Spruce)+
      -2.1739
    )
  )
}
