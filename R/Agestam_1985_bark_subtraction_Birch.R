Agestam_1985_bark_subtraction_Birch <- function(
  basal_area_above_bark_Birch,
  age_at_breast_height,
  latitude
){
  0.861*log(basal_area_above_bark_Birch)+
  +0.217*log(age_at_breast_height)+
  -4.47*log(latitude)+
    16.01
}
