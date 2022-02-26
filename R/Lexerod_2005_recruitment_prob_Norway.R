#' Probability of recruitment (5 cm) for a 5 year period in young norwegian stands.
#'
#' @source Lexerød, N., Eid, T. (2005) Recruitment models for Norway Spruce,
#' Scots Pine, Birch and Other Broadleaves in Young Growth Forests in Norway.
#' Silva Fenn. 39(3): 391-406. Available (2022-02-26):
#' \url{https://www.silvafennica.fi/pdf/article376.pdf}
#' @description Calculates the probability of occurring ingrowth into >5 cm
#' diameter breast height class) occuring during a 5 years period.
#' @param SI40 Site Index at base-age 40 for species in question. For Broadleaves
#' , use SI40 Birch (?). e.g.
#' [forester::Tveite_1976_height_trajectory_Norway_Pine()],
#' [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()], Strand
#' (1967).
#' @param age Total age (?)
#' @param stems_ha Stems per hectare.
#' @param prop_spruce Proportion (Basal area? Stems?) of Spruce.
#' @return Probability of recruitment.
#'
#' @name Lexerod_2005
#' @details Norway spruce observations: 897. Misclassified (6.7 percent). Chi^2:
#' 13.9.
#' @export
Lexerod_2005_prob_recruitment_5cm_Norway_Norway_Spruce <- function(
  SI40,
  age,
  stems_ha,
  prop_spruce
){
  return(
    -17.5779 + 1.4185*log(SI40) + 2.0103*log(age) + 0.8111*log(stems_ha) + 0.9393*log(prop_spruce)
  )
}

#' @param prop_pine Proportion (Basal area? Stems?) of Scots Pine.
#' @rdname Lexerod_2005
#' @export
#' @details Scots Pine observations: 897. Misclassified (7.9 percent). Chi^2:
#' 5.7.
Lexerod_2005_prob_recruitment_5cm_Norway_Scots_Pine <- function(
  SI40,
  age,
  stems_ha,
  prop_pine
){
  return(
    -14.6228 + 1.1399*log(SI40) + 0.9231*log(age) + 0.7120*log(stems_ha) + 1.2627*log(prop_pine)
  )
}

#' @param prop_birch_broadleaves Sum of proportion of of birch and other
#' broadleaves. (BA% or stems?)
#' @rdname Lexerod_2005
#' @export
#' @details Birch observations: 897. Misclassified (6.9 percent). Chi^2:
#' 9.2.
Lexerod_2005_prob_recruitment_5cm_Norway_Birch <- function(
  age,
  stems_ha,
  prop_birch_broadleaves
){
  return(
    -8.2528 + 1.1822*log(age) + 0.2306*log(stems_ha) + 0.9530*log(prop_birch_broadleaves)
  )
}

#' @rdname Lexerod_2005
#' @export
#' @details Broadleaf observations: 897. Misclassified (9 percent). Chi^2:
#' 2.3.
Lexerod_2005_prob_recruitment_5cm_Norway_Broadleaves <- function(
  SI40,
  age,
  stems_ha
){
  return(
    -6.8385 + 1.0531*log(SI40) + 0.2766*log(age) + 0.2577*log(stems_ha)
  )
}
