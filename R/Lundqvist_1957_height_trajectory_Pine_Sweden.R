#' Height trajectory for Scots Pine in planted cultures in Sweden
#'
#' @source https://pub.epsilon.slu.se/10009/
#'
#' @param yearsToBreastHeight Years until 1.3 m.
#' @param H100 Site Index at 100 years of age (total)
#' @param age Total age.
#'
#' @return Height, meters.
#' @export

Lundqvist_1957_height_trajectory_Sweden_Scots_Pine <- function(
  yearsToBreastHeight=7,
  H100,
  age
){
  n = 0.0278*yearsToBreastHeight + 0.436
  b = 6.21581 + 0.43481*yearsToBreastHeight + 0.10998*yearsToBreastHeight^2
  d = n+1
  inflex = ((b*n/(n + 1))^(1/n)) #inflexion given by solving both equations.
  a = H100/exp(-b/((100)^n)) #solve function 1
  c = a*(exp(1)*((b*n)/(n+1)))^(-(n+1)/(n)) #function 9

  return(
    ifelse((age > inflex),a*exp(-(b)/age^n), c*(age^d) )
  )
}
