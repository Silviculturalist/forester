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
