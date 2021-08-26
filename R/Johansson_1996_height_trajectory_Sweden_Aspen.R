#' Height trajectory for European Aspen in Sweden from Johansson 1996.
#'
#' @source Johansson, T. (1996) Site Index Curves for European Aspen (Populus tremula L.)
#'  Growing on Forest Land of Different Soils in Sweden. Silva Fennica 30(4): 437-458.
#'  Available: \url{https://silvafennica.fi/pdf/article5568.pdf}
#'
#' @description
#'
#' Abstract:
#'
#' "Growth data were collected from 40 European aspen (Populus tremula L.) stands
#' growing on eight localities in Sweden. The stands ranged in latitude from 56 to
#' 66 degrees N. The mean age of the stands was 32 years (range, 12-63), the mean
#' stand density 1978 stems ha^-1 (range, 300-6000), and the mean diameter at breast height
#' (on bark) 17 cm (range, 8-34). Site index curves were constructed for total age.
#' ...Curves fitted for H40 totla age have another shape than curves presented by other
#' Nordic studies. The curves from the present study have slower growth for young aspens
#' than curves from norwegian and finnish conditions. For 50-70-year-old aspen stands,
#' curves from the present study indicate taller heights than from Nordic studies."
#'
#'
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
Johansson_1996_height_trajectory_Sweden_Aspen <- function(
  dominant_height,
  age,
  age2,
  output
){
  if(missing(output)){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }


  if(age>60|age2>60){
    warning(
      "Suitable for stands of Aspen under age of 60."
    )
  }

  paramasi <- 7
  parambeta <- 693.2
  paramb2 <- -0.9771

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
