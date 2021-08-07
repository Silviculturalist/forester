Agestam_1985_bark_addition_Spruce <- function(
  basal_area_under_bark_Spruce,
  SI_Spruce
){
  return(
    exp(
      1.366+
      +0.934*log(basal_area_under_bark_Spruce)+
      -0.562*log(SI_Spruce)
    )
  )
}
