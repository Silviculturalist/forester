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
