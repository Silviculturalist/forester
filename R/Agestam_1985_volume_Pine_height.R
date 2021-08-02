Agestam_1985_volume_Pine_height <- function(
  basal_area_Pine_m2_ha,
  dominant_height_Pine_m,
  stems_Pine_ha,
  latitude
){
  return(
    exp(
      +1.0509*log(basal_area_Pine_m2_ha)+
      +0.8074*log(dominant_height_Pine_m*10)+
      -0.04356*log(stems_Pine_ha)+
      +0.5957*log(latitude)+
        -4.4313
    )
  )
}
