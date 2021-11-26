#' Height trajectories for Aspen stands in Norway from Opdahl 1991.
#'
#' @source Opdahl, H. 1991. Bonitet, vekst og produksjon hos osp (Populus tremula L.) i Sør-Norge. (Site-index, growth and yield in Aspen (Populus tremula L.) stands in South Norway.) Medd. Skogforsk. 44(11):1-44. ISBN 82-7169-527-4. ISSN 0803-2866.
#'
#' @description Functions 1:5, pages 14-15.
#'
#' @param Height Top Height, metres.
#' @param Age Age at breast height.
#' @param Age2 Age at breast height at which to calculate new height.
#'
#' @return Top Height at age at breast height == Age2
#' @export
#'
#' @examples
#' Opdahl_1991_height_trajectory_Norway_Aspen(Height=5,Age = 10,Age2 = 40)
Opdahl_1991_height_trajectory_Norway_Aspen <- function(Height, Age, Age2){

  OSP20 <- ((Age+5.94064)/(2.19443+0.64260*(Age+5.94064)))^8.07005
  OSP23 <- ((Age+4.89477)/(2.25222+0.55797*(Age+4.89477)))^6.30208

  DIFF <- ((OSP23+0.0262)-(OSP20+0.1103))/3

  diffratio <- (Height-OSP20)/DIFF

  OSP20_Age2 <- ((Age2+5.94064)/(2.19443+0.64260*(Age2+5.94064)))^8.07005
  OSP23_Age2 <- ((Age2+4.89477)/(2.25222+0.55797*(Age2+4.89477)))^6.30208
  Diff2 <- ((OSP23_Age2+0.0262)-(OSP20_Age2+0.1103))/3

  Height2 <- OSP20_Age2 + diffratio*Diff2

  return(
    Height2
  )
}
