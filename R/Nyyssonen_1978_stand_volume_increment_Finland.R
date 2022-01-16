#' Five-year Stand Increment for even-aged stands of Scots Pine and Norway Spruce in Finland.
#'
#' @source Nyyssönen, A., Mielikäinen, K. 1978. Estimation of stand increment. Act. For. Fenn. 163. ISBN 951-651-038-8.
#'
#' @description OBS. Controlled, but may differ from tables (errata correction?)
#'
#' @param age Age, years.
#' @param volume_m3_ha Stem Volume m^3/ha
#' @param BA_mean_diameter Optional for improved function. Basal area weighted mean diameter (median of basal area)
#' @param vegetation Optional for improved function in Spruce. Necessary for Pine. One of [forester::Finland_vegetation_types()]
#'
#' @details Spruce function no. 18 (diameter)\n n= 146. R^2=0.893. se percent 16.8.\n
#' Spruce function no. 19 (w/o diameter)\n n= 146. R^2=0.884. se percent 17.3.\n
#'
#' @return Annual volume-increment percentage for next five year period.
#' @export
#'
#' @name Nyssonen_stand_increment
Nyyssonen_1978_stand_volume_increment_Finland_Spruce <- function(
  age,
  volume_m3_ha,
  BA_mean_diameter=0,
  vegetation="OMT"
){
  if(!(vegetation%in%c("OMT","MT"))){
    stop("Only available for vegetation codes OMT, MT.")
  }

  x6 <- ifelse(vegetation=="OMT",1,0)

  if(BA_mean_diameter!=0){
    return(
      exp(8.839 + -1.2749*log(age) + -0.5948*log(volume_m3_ha) + 0.00309*((log(age)*log(volume_m3_ha))^2) - 0.1193*log(age*(volume_m3_ha^2)/100000) - 0.0006095*log(BA_mean_diameter)^5 + 0.1009*x6)
    )

  } else {
    return(
      exp(9.7669 - 1.5813*log(age) -0.5730*log(volume_m3_ha) + 0.003315*((log(age)*log(volume_m3_ha))^2) - 0.1177*log(age)*(volume_m3_ha^2)/100000)
    )

  }

}

#' @export
#' @rdname Nyssonen_stand_increment
#' @details Pine function no. 16. \n n=352, R^2=0.904, se percent 16.1\n
#' Pine function no. 17.\n n=352. R^2=0.893, se percent 16.9.\n

Nyyssonen_1978_stand_volume_increment_Finland_Pine <- function(
  age,
  volume_m3_ha,
  BA_mean_diameter=0,
  vegetation="OMT"
){
if(!(vegetation%in%c("OMT","MT","VT","CT"))){
  stop("Only available for vegetation codes OMT, MT, VT, CT.")
}
  x4 <- ifelse(vegetation%in%c("OMT","MT","VT"),1,0)

  if(BA_mean_diameter!=0){
    return(
      exp(-0.7702 + -0.09667*(log(age))^2 + 1.2503*(volume_m3_ha^(1/(volume_m3_ha^0.3))) - 0.1796*((log(BA_mean_diameter)^8)/10000) + 0.1817*x4)
    )
  } else {
    return(
      exp(-0.7632 + -0.1181*(log(age))^2 + 1.3516*(volume_m3_ha^(1/(volume_m3_ha^0.3))) + 0.09116*x4)
    )
  }

 }
