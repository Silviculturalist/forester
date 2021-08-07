Agestam_1985_bark_subtraction_Spruce <- function(
  basal_area_above_bark_Spruce,
  SI_Spruce
){
  return(
    exp(
      0.808+
      +0.956*log(basal_area_above_bark_Spruce)+
      -0.495*log(SI_Spruce)
    )
  )
}
