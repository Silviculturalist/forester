Agestam_1985_bark_addition_Pine <- function(
  basal_area_under_bark_Pine,
  SI_Pine,
  latitude
){
  return(
    exp(
      15.98+
      +1.013*log(basal_area_under_bark_Pine)+
      -0.197*log(SI_Pine)+
      -3.928*log(latitude)
    )
  )
}
