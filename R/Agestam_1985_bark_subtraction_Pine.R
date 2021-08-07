Agestam_1985_bark_subtraction_Pine <- function(
  basal_area_over_bark_Pine,
  SI_Pine,
  latitude
){
  return(
    exp(
      11.69+
      +1.025*log(basal_area_over_bark_Pine)+
      -0.151*log(SI_Pine)+
      -3.020*log(latitude)
    )
  )
}
