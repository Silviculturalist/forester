#' Height trajectory for Oak in Sweden from FAKTA SKOG nr. 14 2013
#'
#' @source Johansson, U., Ekö, P-M, Elfving, B., Johansson, T., Nilsson, U. (2013). Nya höjdutvecklingskurvor för bonitering. FAKTA SKOG. nr. 14. Available: \url{https://www.slu.se/globalassets/ew/ew-centrala/forskn/popvet-dok/faktaskog/faktaskog13/faktaskog_14_2013.pdf}
#'
#' @details
#'
#' 29 plots.
#' 46 observations.
#'
#' @description
#'
#'"Beech and Oak: The curves are based on permanent experimental plots in beech and Oak stands,
#'which are followed with repeat measurements. The same plots were used in the 1970's for develop
#'height development curves and production models for beech and oak stands, but longer time series
#'have been available this time."
#'
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return
#' @export
#'
#' @examples
Johansson_2013_height_trajectory_Sweden_Oak <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  if(age<20|age2<20){
    warning(
      "Suitable for cultivated stands of Oak between total ages of 20 and 150."
    )
  }

  if(age>150|age2>150){
    warning(
      "Suitable for cultivated stands of Oak between total ages of 20 and 150."
    )
  }

  paramasi <- 1000
  parambeta <- 8841.4
  paramb2 <- -1.4317

  d <- parambeta*(paramasi^paramb2)

  r <- (((dominant_height-d)^2)+(4*parambeta*dominant_height*(age^paramb2)))^0.5


  if(output=="SIH100"){
    return(
      (dominant_height+d+r)/ (2+(4*parambeta*(100^paramb2)) / (dominant_height-d+r))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~",(dominant_height+d+r),"/(2+(4*",parambeta,"*(age^",paramb2,")) / ",(dominant_height-d+r),")")
    )
  }

  if(output=="Height"){
    return(
      ((dominant_height+d+r)/ (2+(4*parambeta*(age2^paramb2)) / (dominant_height-d+r)))
    )
  }

}
