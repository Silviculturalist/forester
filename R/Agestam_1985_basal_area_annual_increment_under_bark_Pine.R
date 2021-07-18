#'
#'
#' @param basal_area_Pine_m2_ha_after_thinning
#' @param stem_count_Pine
#' @param age_at_breast_height_period_start
#' @param SI_H100_Pine
#' @param basal_area_other_species_m2_ha_after_thinning
#' @param mean_basal_area_under_bark_Pine_m2
#' @param mean_basal_area_under_bark_all_species_m2
#' @param increment_period_years
#'
#' @return
#' @export
#'
#' @examples
Agestam_1985_basal_area_annual_increment_under_bark_Pine <- function(
  basal_area_Pine_m2_ha_after_thinning,
  stem_count_Pine,
  age_at_breast_height_period_start,
  SI_H100_Pine,
  basal_area_other_species_m2_ha_after_thinning,
  diameter_mean_basal_area_stem_Pine_cm,
  diameter_mean_basal_area_stem_all_species_cm,
  increment_period_years
)
{
  #Age during middle of increment period.
  age_at_breast_height_period_start <- age_at_breast_height_period_start+(increment_period_years/2)

if(age_at_breast_height_period_start<=40){
  a <- 0.37398
} else if(age_at_breast_height_period_start>=41 &&
     age_at_breast_height_period_start<=60){
    a <- 0.01077
  } else if(age_at_breast_height_period_start>=61 &&
            age_at_breast_height_period_start<=80){
    a <- 0.01335
  } else if(age_at_breast_height_period_start>=81){
    a <- 0.01824
  }

  return(
    exp(
      -2.0065+
        -0.1328E-3*(basal_area_Pine_m2_ha_after_thinning*100)+
        +0.42115*log(basal_area_Pine_m2_ha_after_thinning*100)+
        +a*log(stem_count_Pine)+
        +0.4362E-2*SI_H100_Pine*10+
        -0.19424*log(age_at_breast_height_period_start)+
        -0.2130E-3*(basal_area_other_species_m2_ha_after_thinning*100)+
        +0.10502*log((diameter_mean_basal_area_stem_Pine_cm)/(diameter_mean_basal_area_stem_all_species_cm))
    )/10
  )

}
