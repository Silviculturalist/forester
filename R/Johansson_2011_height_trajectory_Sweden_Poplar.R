#' Height trajectory for Poplar on former farmland in Sweden from Johansson 2011.
#'
#' @source Johansson, T. (2011) Site index curves for Poplar growing on former farmland in Sweden. Scandinavian Journal of Forest Research 26(2): 161-170. Available: \url{https://doi.org/10.1080/02827581.2010.543428}
#'
#' @description
#'
#' Abstract:
#'
#' "Site index (SI) curves for H 20 (dominant height at 20 years total age) were constructed using growth data from 64 hybrid poplars (Populus spp.) growing in 33 stands planted on farmland in Sweden (Lat. 55–60°N). The mean age of the stands was 24 years (range 14–45), the mean density 993 stems ha−l (155–3493), and the mean diameter at breast height (outside bark) 24 cm (10–45). SI curves fitted for H 20 at total age were well in accordance with the SI curves presented by other studies. A number of dynamic equations for modeling top-height growth from total age were assessed. A generalized algebraic difference approach (ADA) model derived by Cieszewski performed best. The model explained 98% of the observed variation in height development and exhibited no apparent bias across the range of predicted site indices. SI was also examined in relation to soil type. Multiple samples were available for three types of soil: light clay, medium clay, and till. There were no significant differences between the soil types."
#'
#'
#'
#'
#' @param dominant_height Dominant height of stand, m.
#' @param age Total age.
#' @param age2 Total age at output age.
#' @param output One of "SIH100","Equation" or "Height".
#'
#' @return m.
#' @export
#'
#' @examples
#'
#' #SIH50 40 m: 40 m at age 50.
#' Johansson_2011_height_trajectory_Sweden_Poplar(dominant_height = 40,age = 50,output="Equation")
#'
Johansson_2011_height_trajectory_Sweden_Poplar <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){
  if(!(output%in%c("SIH100","Equation","Height"))){
    stop("Output must be one of 'SIH100','Equation' or 'Height'")
  }


  message("Suitable for stands of Poplar on former farmland in Sweden under age of 50-60 years.")

  b0 <- 2.1405
  b1 <- 6460.5
  b2 <- 18.2238

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
