Agestam_1985_volume_Pine_SI <- function(
  basal_area_Pine_m2_ha,
  SI_Pine_m,
  age_at_breast_height_Pine,
  stems_Pine_ha,
  latitude
){
  SI_Pine <- SI_Pine_m*10
return(
  exp(
    +1.0772*log(basal_area_Pine_m2_ha)+
    +0.7859*log(SI_Pine)+
    +0.3131*log(age_at_breast_height_Pine)+
    -7.4681*(1/age_at_breast_height_Pine)+
    -0.06444*log(stems_Pine_ha)+
    +0.4741*log(latitude)+
    -5.0965
  )
)

}
