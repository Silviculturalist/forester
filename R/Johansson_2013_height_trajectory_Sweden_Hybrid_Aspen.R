#' Height trajectory for Hybrid Aspen, Populus tremula L x Populus tremuloides Michx., in Sweden from Johansson 1999.
#'
#' @source Johansson, T. (2013) A site dependent top height growth model for hybrid aspen. Journal of Forestry Research. 24. 691-698. Available: \url{https://link.springer.com/article/10.1007\%2Fs11676-013-0365-6}
#'
#' @description
#'
#' Abstract:
#'
#' "In this study height growth models for hybrid aspen were developed using
#' three growth equations. The mean age of the hybrid aspen was 21 years (range
#'  15−51 years) with a mean stand density of 946 stems ha-1 (87−2374) and a
#'  mean diameter at breast height (over bark) of 19.6 cm (8.5−40.8 cm). Site
#'  index was also examined in relation to soil type. Multiple samples were
#'  collected for three types of soil: light clay, medium clay and till. Site
#'  index curves were constructed using the col- lected data and compared with
#'  published reports. A number of dynamic equations were assessed for modeling
#'  top-height growth from total age. A Generalized Algebraic Difference
#'  Approach model derived by Cieszewski (2001) performed the best. This model
#'  explained 99\% of the observed variation in tree height growth and exhibited
#'  no apparent bias across the range of predicted site indices. There were no
#'  significant differences between the soil types and site indices."
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
#'
#' #SIH50 40 m: 40 m at age 50.
#' Johansson_2013_height_trajectory_Sweden_Hybrid_Aspen(dominant_height = 40,age = 50,output="Equation")
#'
Johansson_2013_height_trajectory_Sweden_Hybrid_Aspen <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }


  message("Suitable for stands of Hybrid Aspen under age of 50.")

  b0 <- 2.0381
  b1 <- 4692.5
  b2 <- 23.1758

  Z0 <- dominant_height-b2
  P <- Z0+((Z0^2 + (2*b1*dominant_height)/(age^b0))^0.5)

  if(output=="SIH100"){
    return(
      dominant_height*(((100^b0)*((age^b0)*P+b1)) / ((age^b0)*((100^b0)*P+b1)))
    )
  }

  if(output=="Equation"){
    return(
      paste0("y~",dominant_height,"*(((age2^",b0,")*((",age^b0,")*",P,"+",b1,")) / ((",age^b0,")*((age2^",b0,")*",P,"+",b1,")))")
    )
  }

  if(output=="Height"){
    return(
      dominant_height*(((age2^b0)*((age^b0)*P+b1)) / ((age^b0)*((age2^b0)*P+b1)))
    )
  }

}
