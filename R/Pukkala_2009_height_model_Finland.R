#' Height model for Individual Trees from Pukkala 2009.
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url[https://doi.org/10.1016/j.foreco.2009.03.052]
#'
#' @param vegetation Code from [forester::Finland_vegetation_types()].
#' @param diameter_cm Diameter of the tree at breast height (1.3 m)
#' @details Pine model based on 8622 observations. R^2=0.779. SD resid. = 2.360
#'
#' @return Tree height.
#' @export
#' @name Pukkala_2009_height_model_Finland_Pine
#' @md

Pukkala_2009_height_model_Finland_Pine <- function(
  vegetation,
  diameter_cm
){

  MT_plus <- vegetation%in%c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT")
  VT <- vegetation=="VT"
  CT <- vegetation=="CT"
  ClT <- vegetation=="ClT"

  a1=25.014
  a2=7.680
  a3=6.376
  a4=-1.787
  a5=-3.296
  b1=19.260
  b2=31.721

  return(
    (a1+ (a2*MT_plus + a3*VT + a4*CT + a5*ClT))/(1+(b1/diameter_cm) + (b2/(diameter_cm^2)))
  )

}

#'
#'@details Betula model based on 1200 observations. R^2=0.802, SD resid.=2.001
#'@export
#'@rdname Pukkala_2009_height_model_Finland_Pine

Pukkala_2009_height_model_Finland_Betula <- function(
  vegetation,
  diameter_cm
){

  MT_plus <- vegetation%in%c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT")
  VT <- vegetation=="VT"
  CT <- vegetation=="CT"
  ClT <- vegetation=="ClT"

  a1=29.375
  a2=7.714
  a3=3.059
  a4=-2.870
  a5=0
  b1=22.640
  b2=-8

  return(
    (a1+ (a2*MT_plus + a3*VT + a4*CT + a5*ClT))/(1+(b1/diameter_cm) + (b2/(diameter_cm^2)))
  )

}

#' @details Norway Spruce model based on 12144 observations. R^2 = 0.856, SD resid. = 2.104.
#' @rdname Pukkala_2009_height_model_Finland_Pine
#' @export

Pukkala_2009_height_model_Finland_Norway_Spruce <- function(
  vegetation,
  diameter_cm
){

  MT_plus <- vegetation%in%c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT")
  VT <- vegetation=="VT"
  CT <- vegetation=="CT"
  ClT <- vegetation=="ClT"

  a1=33.726
  a2=5.965
  a3=2.178
  a4=-1.399
  a5=0
  b1=25.683
  b2=37.785

  return(
    (a1+ (a2*MT_plus + a3*VT + a4*CT + a5*ClT))/(1+(b1/diameter_cm) + (b2/(diameter_cm^2)))
  )

}

