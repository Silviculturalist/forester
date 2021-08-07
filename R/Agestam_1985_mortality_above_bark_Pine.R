Agestam_1985_mortality_above_bark_Pine <- function(
  stems_ha,
  basal_area_above_bark_all_species,
  SI,
  age_at_breast_height
){
  return(
    exp(
      16.039+
      +0.4809*log(stems_ha)+
      +2.2526*log(basal_area_above_bark_all_species)+
      -3.5439*log(SI)+
      -2.6191*log(age_at_breast_height)
    )
  )
}
