#' Basal area increment under bark at breast height for Scots Pine from Persson (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 56. pp. 206.
#'
#' @details
#'
#' Function no. 2.
#' More and less detailed functions available.
#'
#' number of growth periods: 1163
#'
#' Coefficient of determination: 0.790
#'
#' Standard deviation about the function (sf): 0.219
#'
#' (sf)/standard deviation about the mean: 45.9%
#' @param basal_area_after_thinning Basal area at the beginning of the period, after thinning, under bark, m2/ha
#' @param age_at_breast_height Age at breast height
#' @param SI Site Index after Hägglund 1974, m.
#' @param latitude Latitude, degrees.
#'
#' @return Annual basal area increment under bark, m2/ha/yr
#' @export
Persson_1992_BA_increment_Pine <- function(
  basal_area_after_thinning,
  age_at_breast_height,
  SI,
  latitude
){

    return(
      exp(
        +4.90697+
        +0.44683*log(basal_area_after_thinning)+
        -0.63272*log(age_at_breast_height)+
        +0.30834*log(SI)+
        -1.32323*log(latitude)
      )
    )

}
