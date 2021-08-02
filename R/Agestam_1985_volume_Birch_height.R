Agestam_1985_volume_Birch_height <- function(
  basal_area_Birch_m2_ha,
  dominant_height_Birch_m,
  stems_ha_Birch,
  latitude
){

  return(
    exp(
      +1.0419*log(basal_area_Birch_m2_ha)+
      +0.9020*log(dominant_height_Birch_m)+
      +1.2264*(1/stems_ha_Birch)+
      +0.6799*log(latitude)+
        -5.5828
    )
  )
}
