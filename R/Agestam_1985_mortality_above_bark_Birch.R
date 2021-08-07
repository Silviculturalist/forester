Agestam_1985_mortality_above_bark_Birch <- function(
  stems_ha_all_species,
  basal_area_m2_ha_Birch
){
  return(
    exp(
      -8.3764+
      +0.4199*log(stems_ha_all_species)+
      +0.3236*log(basal_area_m2_ha_Birch)
    )
  )
}
