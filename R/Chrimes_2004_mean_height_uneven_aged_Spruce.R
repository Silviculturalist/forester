#' Height equation from Chrimes 2004
#'
#' @source Chrimes, D. & Lundqvist, L. 2004. Manuscript: Simulated volume increment of managed uneven-aged Picea abies stands in central Sweden. (PhD. dissertation, Swedish University of Agricultural Sciences). Chapter I p. 7.
#'
#' @details
#'
#'
#' r^2 = 0.95
#' df= 619
#' mse = 1.86
#' cv(\%)= 1.58
#'
#'
#' @param diameter_cm Diameter of tree at breast height 1.3 m.
#'
#' @return Height of tree, metres.
#' @export
Chrimes_2004_mean_height_uneven_aged_Spruce <- function(diameter_cm){
  return(

    (((diameter_cm)/(2.044+0.204*diameter_cm))^2.363) + 1.3

  )
}
