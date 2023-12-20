#' Height trajectories of Coastal Norway Spruce in Finland
#' @source Karlsson, K. (2000) Height growth patterns of Scots pine and Norway
#' spruce in the coastal areas of western Finland. For. Ecol. Manage. 135:1-3,
#' pp. 205-216. DOI: \url{https://doi.org/10.1016/S0378-1127(00)00311-X}
#'
#' @param dominant_height Dominant height of the stand, in metres.
#' @param age Age of stand (breast height)
#' @param age2 Age of stand at output (breast height).
#' @param output One of 'Height' (default) or 'Equation'.
#'
#' @return Dominant height at age2, or an equation (character).
#' @export

Karlsson_2000_height_trajectory_Finland_coastal_Norway_Spruce <- function(
  dominant_height,
  age,
  age2,
  output="Height"
){

  stopifnot(output%in%c("Height","Equation"))

  Karlsson_height_function <- function(SI100,age){1.5123*SI100^0.9152*((1 - exp(-0.03127*age))^(1.8102*(-0.04*SI100+2.625)^1.120))}

  #Estimate SI100 or if age = 100, set dominant_height to SI100_est
  if(age!=100){
    SI100_est <- stats::optimise(f = function(SI100,age,dominant_height) abs(Karlsson_height_function(SI100,age=age)-dominant_height)^2,interval=c(0,50),age=50,dominant_height=10.1857)[["minimum"]]
    } else if (age==100) {
      SI100_est <- dominant_height
      }
  #Estimate height at age2.
  H2 <- Karlsson_height_function(SI100 = SI100_est,age = age2)

  #Equation
  if(output=="Height") return(H2)
  if(output=="Equation") return(paste0("H=1.5123*",SI100_est,"^0.9152*((1 - exp(-0.03127*age))^(1.8102*(-0.04*",SI100_est,"+2.625)^1.120))"))
}
