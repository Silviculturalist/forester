#' Basal area growth function for Spruce from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 29.
#'
#' @details Material:
#'
#' Number of growth periods: 586
#'
#' Multiple correlation coefficient: 0.938
#'
#' Standard deviation about the function (sf) : 0.293
#'
#' sf/standard deviation about the mean:  35\%
#'
#'
#'
#' @param basal_area_Spruce_m2_ha_after_thinning Basal area of Spruce after thinning, under bark. m2/ha.
#' @param stem_count_Spruce Number of Spruce stems per ha after thinning.
#' @param age_at_breast_height_period_start Age at breast height for Spruce at the period start.
#' @param SI_H100_Spruce Site index H100 for Spruce according to height trajectory defined in Hägglund, 1972, 1973.
#' @param basal_area_other_species_m2_ha_after_thinning Basal area under bark of other species after thinning, under bark. m2/ha.
#' @param increment_period_years Length of increment period, in years.
#' @param removal_basal_area_m2_ha_at_start_of_period Removed basal area at the start of the growth period, under bark, Spruce. m2/ha.
#'
#' @return Annual basal area growth under bark for Spruce. m2/ha.
#' @export

Agestam_1985_basal_area_annual_increment_under_bark_Spruce <- function(
  basal_area_Spruce_m2_ha_after_thinning,
  stem_count_Spruce,
  age_at_breast_height_period_start,
  SI_H100_Spruce,
  basal_area_other_species_m2_ha_after_thinning,
  increment_period_years,
  removal_basal_area_m2_ha_at_start_of_period
)
{

  return(
    exp(
      -0.2309E-3*(basal_area_Spruce_m2_ha_after_thinning*100)+
        +0.67033*log((basal_area_Spruce_m2_ha_after_thinning*100))+
        +0.23347*log(stem_count_Spruce)+
        -0.34588*log(age_at_breast_height_period_start+(increment_period_years/2))+
        +0.2659E-2*SI_H100_Spruce*10+
        -0.3366E-3*(basal_area_other_species_m2_ha_after_thinning*100)+
        -0.02174*log(removal_basal_area_m2_ha_at_start_of_period*100)+
        -1.3933
    )/100
  )

}
