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
