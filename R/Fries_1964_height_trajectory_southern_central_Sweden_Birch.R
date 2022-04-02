#' Height for diameter classes in Birch stands according to Fries 1964
#'
#' @param dominant_height Dominant height, mean height of the 10% thickest trees.
#' @param diameter_cm Diameter above bark at breast height 1.3 m.
#' @param D10_cm Diameter of the 10% thickest trees above bark.
#'
#' @return
#' @export
#'
#' @examples
Fries_1964_height_trajectory_southern_central_Sweden_Birch <- function(
  dominant_height,
  diameter_cm,
  D10
){

  #height for a tree with DBH = d10 percent / 2
  #dominant height = h 10 percent.

  hd_half <- -0.615 + 0.83984*dominant_height

  #eq.1
  #dominant_height - 1.3 = (1 /(A+b))^2
  #eq.2
  #hd_half - 1.3 = ( 1 / (2*A_half + b_half))^2


  #Solve for A and b


  # A_half = ±(1/2)*(1/sqrt(hd_half-1.3) - b_half)
  # b_half = ±1/sqrt(hd_half-1.3) - 2*A_half
  #
  # A <- ±1/sqrt(dominant_height-1.3) - b
  # b <- ±1/sqrt(dominant_height-1.3) - A

  g <- hd_half-1.3 #g is just a variable holder
  d <- dominant_height-1.3 #d is just a variable holder.

  #Solution substituting b_half into eq. 1.
  #dominant_height - 1.3 = (1 /(A+(1/sqrt(hd_half-1.3) - 2*A)))^2
  A <- (1/sqrt(g))-(g/sqrt(d*g^2))
  b <- 1/sqrt(dominant_height-1.3)- A


  return(
    1.3 + (diameter_cm/(D10_cm*A + diameter_cm*b))^2
  )

}
