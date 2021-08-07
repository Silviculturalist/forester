Agestam_1985_mortality_above_bark_Spruce <- function(
  stems_ha,
  basal_area_above_bark_all_species
){
  return(
    exp(
      -16.996+
      +1.4344*log(stems_ha)+
      +0.9816*log(basal_area_above_bark_all_species)
    )
  )
}
