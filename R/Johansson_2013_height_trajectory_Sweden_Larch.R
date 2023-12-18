#' Height trajectory for Larch in Sweden from FAKTA SKOG nr. 14 2013
#'
#' @source Johansson, U., Ekö, P-M, Elfving, B., Johansson, T., Nilsson, U. (2013). Nya höjdutvecklingskurvor för bonitering. FAKTA SKOG. nr. 14. Available: \url{https://www.slu.se/globalassets/ew/ew-centrala/forskn/popvet-dok/faktaskog/faktaskog13/faktaskog_14_2013.pdf}
#'
#' @details
#'
#' 77 plots.
#' 130 observations.
#'
#' @description
#'
#'"Larch: Swedish height development curves for Larch have not been constructed earlier. Access to
#'new data series have made such possible. The curves for larch are based on 21 permanent experimental plots with
#'European Larch, 14 with Japanese larch, 31 with Siberian Larch, and 11 with Hybrid Larch. The curves
#'are suitable for use in the entire country and with any of the mentioned species."
#'
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return m.
#' @export
Johansson_2013_height_trajectory_Sweden_Larch <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }

  if(age<10|age2<10){
    warning(
      "Suitable for cultivated stands of Larch between total ages of 10 and 100."
    )
  }

  if(age>100|age2>100){
    warning(
      "Suitable for cultivated stands of Larch between total ages of 10 and 100."
    )
  }

  paramasi <- 17.97
  parambeta <- 1529
  paramb2 <- -1.3451

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
