Agestam_1985_bark_addition_Birch <- function(
  basal_area_under_bark_Birch,
  age_at_breast_height,
  latitude
){
  return(
    exp(
      19.276+
      +0.818*log(basal_area_under_bark_Birch)+
      +0.269*log(age_at_breast_height)+
      -5.25*log(latitude)
    )
  )
}
