#' Volume function for Spruce from Agestam (1985), including height.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: G1
#'
#' Number of plots: 115
#'
#' Multiple correlation coefficient: 0.995
#'
#' Standard deviation about the function (sf): 0.058
#'
#' sf/standard deviation about the mean: 10.2%
#'
#'
#' @param basal_area_Spruce_m2_ha Basal area over bark Spruce, m2/ha.
#' @param dominant_height_Spruce_m Dominant height of Spruce stems, m.
#' @param stems_Spruce_ha Spruce stems per hectare.
#' @param age_at_breast_height_Spruce Age at breast height of Spruce stems.
#'
#' @return Volume Spruce m3sk/ha.
#' @export
#'
#' @examples
Agestam_1985_volume_Spruce_height <- function(
  basal_area_Spruce_m2_ha,
  dominant_height_Spruce_m,
  stems_Spruce_ha,
  age_at_breast_height_Spruce
){
  return(
    exp(
      +1.0509*log(basal_area_Spruce_m2_ha)+
      +0.7386*log(dominant_height_Spruce_m)+
      -0.008218*sqrt(stems_Spruce_ha)+
      -0.01225*sqrt(age_at_breast_height_Spruce)+
      -1.8422

    )
  )
}
