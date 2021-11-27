Opdahl_1991_mean_height_Lorey_Norway_Aspen <- function(
  dominant_height,
  BA_mean_diameter,
  stems_per_ha_before_thinning,
  Basal_area_m2_ha_before_thinning,
  stand_age_at_breast_height,
  correction=TRUE
){

  correction <- ifelse(isTRUE(correction),0.75,0)

  return(
    #Observe, contains typo in source. Missing a closing bracket.
    (dominant_height-(-13.7396 + (dominant_height*1.58856)-((dominant_height^2)*0.00378)-(BA_mean_diameter*0.41154))+((BA_mean_diameter*dominant_height)*0.01406)+((stems_per_ha_before_thinning*dominant_height)*0.00007)-((Basal_area_m2_ha_before_thinning^2)*0.00152)+(stand_age_at_breast_height*0.01254)) - correction
  )

}
