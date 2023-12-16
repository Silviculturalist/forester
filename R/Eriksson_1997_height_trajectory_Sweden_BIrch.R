#' Height trajectory for Birch in Sweden from Eriksson et al 1997.
#'
#' @source Eriksson, H., Johansson, U., Kiviste, A. (1997) A site-index model for
#' pure and mixed stands of Betula pendula and Betula pubescens in Sweden.
#' Scandinavian Journal of Forest Research. 12:2, 149-156.
#' Available: \url{https://doi.org/10.1080/02827589709355396}
#'
#' @description
#'
#' Abstract
#'
#' A site‐index model was constructed based on stem analysis data for 266 top‐height
#' trees of Betula pendula Roth and Betula pubescens Ehrh. growing on 155 temporary
#' sample plots and 12 remeasured, permanent sample plots in pure and mixed stands
#' scattered throughout Sweden. Different growth functions and techniques for
#' modelling the top‐height growth over breast‐height age were assessed. A difference model
#' based on the Hossfeld IV growth equation performed best, and its practical
#' application was therefore recommended. For birch stands over 40 yrs of age,
#' top‐height growth predicted by the new model was significantly slower than
#' that predicted by a site‐index model used previously.
#'
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Age at breast height.
#' @param age2 Output age at breast height.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return One of output.
#' @export
#'
#' @examples
#'
#'ErikssonVect <- Vectorize(Eriksson_1997_height_trajectory_Sweden_Birch)
#'
#'plot(NULL,
#'     xlim=c(0,100),
#'     ylim=c(0,35))
#'
#'for(i in 1:11){
#'  lines(seq(0,100,0.1),ErikssonVect(seq(10,30,2)[i],age = 50,age2 = seq(0,100,0.1)))
#'}
#'
#'points(rep(50,11),seq(10,30,2))

Eriksson_1997_height_trajectory_Sweden_Birch <- function(
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
      "Suitable for cultivated stands of Birch between total ages of 10 and 90."
    )
  }

  if(age>90|age2>90){
    warning(
      "Suitable for cultivated stands of Birch between total ages of 10 and 90."
    )
  }

  paramasi <- 7
  parambeta <- 394
  paramb2 <- 1.387

  d <- parambeta/(paramasi^paramb2)

  r <- (((dominant_height-1.3-d)^2)+(4*parambeta*(dominant_height-1.3)/(age^paramb2)))^0.5


  if(output=="SIH100"){
    return(
      ((dominant_height-1.3+d+r)/ (2+(4*parambeta*(100^-paramb2)) / (dominant_height-1.3-d+r)))+1.3
    )
  }

  if(output=="Equation"){
    return(
      paste0("y ~(",(dominant_height-1.3+d+r),"/(2+(4*",parambeta,"*(age^",-paramb2,")) / ",(dominant_height-1.3-d+r),"))+1.3")
    )
  }

  if(output=="Height"){
    return(
      ((dominant_height-1.3+d+r)/ (2+(4*parambeta*(age2^-paramb2)) / (dominant_height-1.3-d+r)))+1.3
    )
  }

}
