#' Heidarsson & Pukkala 2012 Individual tree model for Larix sibirica on Iceland.
#'
#' @source Heidarsson, L. & Pukkala, T. (2012). Models for simulating the
#' temporal development of Siberian larch (\emph{Larix sibirica}) plantations in
#' Hallormsstadur Iceland. Icel. Agric. Sci. 25:13-23. Available (2022-04-02):
#' \url{https://skemman.is/bitstream/1946/19764/1/Larus%20Heidarsson%20et%20al%202012.pdf}
#'
#' @param dominant_height Dominant height of stand, in metres.
#' @param age Age at breast height, 1.3 m.
#' @param age2 Age at breast height, 1.3 m at desired output
#' @param BA_m2_ha Basal Area of stand, m^2 / ha.
#' @param BAL Basal-area-in-larger-trees, m^2 / ha.
#' @param diameter_cm Diameter of tree at breast height 1.3m, in cm.
#' @param stand_age Age at breast height, 1.3 m.
#'
#' @return Dominant height at age2. (R^2 = 0.980, MSE= 0.173)
#' @export
#' @name Heidarsson_2012
Heidarsson_2012_height_trajectory_Iceland_Larix_sibirica <- function(
    dominant_height,
    age=80,
    age2){

  exp(log(dominant_height) + 9.865*((1/(age^0.604))-(1/(age2^0.604))))

}

#' @return Single tree height, m. (R^2 = 0.963, MSE= 0.433)
#' @export
#' @rdname Heidarsson_2012
Heidarsson_2012_single_tree_height_Iceland_Larix_sibirica <- function(
  diameter_cm,
  dominant_height
){
  1.3 + (3.428 + 0.931*dominant_height)/(1+8.126/diameter_cm)
}



#' @return 5 year diameter increment of a tree, cm. (R^2=0.659, MSE= 0.352)
#' @export
#' @rdname Heidarsson_2012

Heidarsson_2012_diameter_increment_Iceland_Larix_sibirica <- function(
    BA_m2_ha,
    BAL,
    diameter_cm,
    stand_age
){
  exp(2.525 - 0.0203*BA_m2_ha - 0.0705*BAL/log(diameter_cm+1) - 0.344*log(stand_age))
}



#' @return Probability of tree survival for the coming 5 years.
#' @export
#' @rdname Heidarsson_2012
Heidarsson_2012_tree_survival_Iceland_Larix_sibirica <- function(
    BAL
  ){
  1/(1+exp(-(17.871 - 0.381*BAL)))
}



#' @export
#' @rdname Heidarsson_2011
#' @return Diameter at height (?)
Heidarsson_2011_stem_taper_Iceland_Larix_sibirica <- function(
    diameter_cm,
    height_m,
    output_height
){
  q <- output_height/height_m
  X <- (1-(q^(1/3)))/(1-(1.3/height_m)^(1/3))
  Q <- (1-(q^(1/3)))

  return(
    (diameter_cm^0.928)*(height_m^0.087)*(X)*0.564*(q^4) -
      0.849*(1/exp(diameter_cm/height_m))+
      0.473*(X^0.1) +
      2.494*(1/diameter_cm)+
      0.069*(height_m^Q) -
      0.207*X
  )

}

#' Stem taper of Lodgepole Pine and Siberian Larch on Iceland
#'
#' @source Heidarsson, L. & Pukkala, T. (2011). Taper functions for lodgepole
#' pine (\emph{Pinus contorta}) and Siberian larch (\emph{Larix sibirica}) in
#' Iceland. Icel. Agric. Sci. 24:3-11. Available (2022-04-02):
#' \url{https://skemman.is/bitstream/1946/19885/1/Larus\%20Heidarsson\%20et\%20al\%202011\%20IAS.pdf}
#'
#' @details \tabular{lrr}{
#' Species \tab R^2 \tab RMSE \cr
#' Larix sibirica \tab 0.986 \tab 1.024 \cr
#' Pinus contorta \tab 0.979 \tab 0.888 \cr
#' }
#'
#' @param diameter_cm Diameter of tree at breast height 1.3m.
#' @param height_m Total height of tree.
#' @param output_height Output height.
#'
#' @return Diameter of tree in cm at output height.
#' @export
#' @name Heidarsson_2011
Heidarsson_2011_stem_taper_Iceland_Pinus_contorta <- function(
    diameter_cm,
    height_m,
    output_height
){
  q <- output_height/height_m
  X <- (1-(q^(1/3)))/(1-(1.3/height_m)^(1/3))
  Q <- (1-(q^(1/3)))

  return(
    (diameter_cm^0.997)*(X)*0.634*(q^4) -
      0.948*(1/exp(diameter_cm/height_m))+
      0.507*(X^0.1) +
      0.903*(1/diameter_cm)+
      0.032*(height_m^Q) -
      0.069*X
  )

}
