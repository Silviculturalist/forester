#' Height trajectory for Scots Pine in Sweden from Elfving, Kiviste 1997.
#'
#' @source Elfving, B., Kiviste, A. (1997) Construction of site index equations for
#' Pinus sylvestris L. using permanent plot data in Sweden. For. Ecol. Manage. Vol. 98. Issue 2. pp. 125-134.
#' Available: \url{https://doi.org/10.1016/S0378-1127(97)00077-7}
#'
#' @description
#'
#' 156 Scots pine stands, whereof most were long term experiments maintained by the
#' Dept. of Forest Yield Research.
#'
#' Suitable for Scots Pine of cultivated origin between ages 10 - 80.
#'
#' RMSE = 0.401.
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
Elfving_Kiviste_1997_height_trajectory_Sweden_Pine <- function(
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
      "Suitable for cultivated stands of Scots Pine between total ages of 10 and 80."
    )
  }

  if(age>80|age2>80){
    warning(
      "Suitable for cultivated stands of Scots Pine between total ages of 10 and 80."
    )
  }

  paramasi <- 25
  parambeta <- 7395.6
  paramb2 <- -1.7829

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
