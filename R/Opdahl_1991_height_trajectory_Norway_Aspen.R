#' Height trajectories for Aspen stands in Norway from Opdahl 1991.
#'
#' @source Opdahl, H. 1991. Bonitet, vekst og produksjon hos osp (Populus tremula L.) i Sør-Norge. (Site-index, growth and yield in Aspen (Populus tremula L.) stands in South Norway.) Medd. Skogforsk. 44(11):1-44. ISBN 82-7169-527-4. ISSN 0803-2866.
#'
#' @description Functions 1:5, pages 14-15.
#'
#' @param dominant_height Dominant Height, metres.
#' @param age Age at breast height.
#' @param age2 Age at breast height at which to calculate new height.
#'
#' @return Top Height at age at breast height == age2
#' @export
#'
#' @examples
#' Opdahl_1991_height_trajectory_Norway_Aspen(dominant_height=5,age = 10,age2 = 40)
Opdahl_1991_height_trajectory_Norway_Aspen <- function(dominant_height, age, age2){

  OSP20 <- ((age+5.94064)/(2.19443+0.64260*(age+5.94064)))^8.07005
  OSP23 <- ((age+4.89477)/(2.25222+0.55797*(age+4.89477)))^6.30208

  DIFF <- ((OSP23+0.0262)-(OSP20+0.1103))/3

  diffratio <- (dominant_height-OSP20)/DIFF

  OSP20_age2 <- ((age2+5.94064)/(2.19443+0.64260*(age2+5.94064)))^8.07005
  OSP23_age2 <- ((age2+4.89477)/(2.25222+0.55797*(age2+4.89477)))^6.30208
  Diff2 <- ((OSP23_age2+0.0262)-(OSP20_age2+0.1103))/3

  dominant_height2 <- OSP20_age2 + diffratio*Diff2

  return(
    dominant_height2
  )
}
