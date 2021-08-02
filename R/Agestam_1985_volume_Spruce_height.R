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
