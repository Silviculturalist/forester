#' Basal area growth function for Birch from Agestam 1985
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 29.
#'
#' @details Material:
#'
#' Number of growth periods: 126
#'
#' Multiple correlation coefficient: 0.928
#'
#' Standard deviation about the function (sf) : 0.396
#'
#' sf/standard deviation about the mean:  38\%
#'
#'
#' @param basal_area_Birch_m2_ha_after_thinning Basal area of Birch after thinning, under bark. m2/ha.
#' @param stem_count_Birch Number of Birch stems per ha after thinning.
#' @param age_at_breast_height_period_start Age at breast height for Birch at the period start.
#' @param SI_H100_Birch Site index H100 for Birch according to height trajectory defined in Persson (1959)
#' @param basal_area_other_species_m2_ha_after_thinning Basal area under bark of other species after thinning, under bark. m2/ha.
#' @param increment_period_years Length of increment period, in years.
#'
#' @return Annual basal area growth under bark for Birch. m2/ha.
#' @export
Agestam_1985_basal_area_annual_increment_under_bark_Birch <- function(
  basal_area_Birch_m2_ha_after_thinning,
  stem_count_Birch,
  age_at_breast_height_period_start,
  SI_H100_Birch,
  basal_area_other_species_m2_ha_after_thinning,
  increment_period_years
)
{

  return(
    exp(
      +0.63464*log(basal_area_Birch_m2_ha_after_thinning*100)+
        +0.17829*log(stem_count_Birch)+
        -0.73932*log(age_at_breast_height_period_start+(increment_period_years/2))+
        +0.2557E-2*SI_H100_Birch*10+
        -0.2670E-3*(basal_area_other_species_m2_ha_after_thinning*100)+
        +0.4298
    )/100
  )

}
