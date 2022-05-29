#' Calculate basal area weighted mean stem diameter from arithmetic mean stem diameter and
#' its' standard deviation e.g. Oppermann 1905, Cajanus 1912.
#'
#' @description A relationship commonly attributed to either Oppermann (1905)
#' or Cajanus (1912).
#'
#' @source Meyer, W. 1930. Diameter distribution Series in Evenaged Forest
#' Stands. Yale School of Forestry Bulletin 28. 105 pp. p.24. Available online:
#'  \url{https://elischolar.library.yale.edu/cgi/viewcontent.cgi?article=1027&context=yale_fes_bulletin}
#' @param diameter Mean arithmetic diameter of the stand in cm.
#' @param diameter_sd Standard deviation of the stem distribution of the stand.
#' @param QMD The basal area weighted mean diameter (cm) of the stand (Dg).
#'
#' @return Stand QMD.
#'
#' @name CajanusDg
#' @export

Oppermann_Cajanus_stand_QMD <- function(
    diameter,
    diameter_sd
    ){
  return(
    sqrt((diameter^2 + diameter_sd^2))
  )
}


#' @rdname CajanusDg
#' @return The mean diameter of the stand, cm.
#' @source Reformulation : Eriksson, H. 1976. Yield of Norway spruce in Sweden.
#' Research Notes. Dept. of. Forest Yield Research. Nr. 41. Royal College of
#' Forestry. Stockholm. p. 165.
#' @export
Oppermann_Cajanus_mean_diameter <- function(
    QMD,
    diameter_sd
){
  return(
    sqrt((DG^2) - (diameter_sd^2))
  )
}



#' @rdname CajanusDg
#' @param stems Stems per hectare.
#' @return Stand total basal area
#' @export
Oppermann_Cajanus_stand_BA <- function(
  stems,
  diameter,
  diameter_sd
  ){
  return(
    stems*pi/4*(diameter^2 + diameter_sd^2)
  )
}
