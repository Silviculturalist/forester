#' Number of recruits to smallest diameter class (5 cm) for a 5 year period in
#' young norwegian stands.
#'
#' @source Lexerød, N., Eid, T. (2005) Recruitment models for Norway Spruce,
#' Scots Pine, Birch and Other Broadleaves in Young Growth Forests in Norway.
#' Silva Fenn. 39(3): 391-406. Available (2022-02-26):
#' \url{https://www.silvafennica.fi/pdf/article376.pdf}
#' @description Calculates the number of recruits into >5 cm
#' diameter breast height class) occuring during a 5 years period. These functions
#' are suitable for development class II (Conifers and broadleaves: max total
#' stand ages 20, 20, 25, 30, 35, 45, 55 for SI40 23, 20, 17, 14, 11, 8, 6;
#' Birch: max total stand ages 15, 15, 20, 25, 25, 25, 30 years for SI40 23, 20,
#'  17, 14, 11, 8, 6).
#' @param SI40 Site Index at base-age 40 for species in question. For Broadleaves
#' , use SI40 Birch (?). e.g.
#' [forester::Tveite_1976_height_trajectory_Norway_Pine()],
#' [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()], Strand
#' (1967).
#' @param age Stand age, breast height (1.3m)
#' @param stems_ha Stems per hectare.
#' @param percent_spruce Percent (Basal area? Stems?) of Spruce.
#' @return Number of recruits
#'
#' @name Lexerod_2005_count
#' @details Norway spruce observations: 510. Adj.R^2 = 0.396. CV(percent): 13.8.
#' @export
Lexerod_2005_recruitment_count_5cm_Norway_Norway_Spruce <- function(
  SI40,
  age,
  stems_ha,
  percent_spruce
){
  return(
    exp(-3.9717 + 0.0631*SI40 + 0.0391*age - 0.0001*log(stems_ha) + 0.8335*log(stems_ha) + 0.4374*log(percent_spruce))
  )
}

#' @param percent_pine Percent (Basal area? Stems?) of Scots Pine.
#' @rdname Lexerod_2005_count
#' @export
#' @details Scots Pine observations: 228. Adj.R^2 = 0.297. CV(percent): 16.0.
Lexerod_2005_recruitment_count_5cm_Norway_Scots_Pine <- function(
  SI40,
  age,
  stems_ha,
  percent_pine
){
  return(
    exp(-0.66805 + 0.0547*SI40 + 0.0175*age + 0.4145*log(stems_ha) + 0.4344*log(percent_pine))
  )
}

#' @param percent_birch_broadleaves Sum of Percent of of birch and other
#' broadleaves. (BA\% or stems?)
#' @rdname Lexerod_2005_count
#' @export
#' @details Birch observations: 436. Adj.R^2 = 0.147. CV(percent): 18.2.
Lexerod_2005_recruitment_count_5cm_Norway_Birch <- function(
  age,
  stems_ha,
  percent_birch_broadleaves
){
  return(
    exp(-0.8924 + 0.0294*age - 0.0001*stems_ha + 0.6204*log(stems_ha) + 0.2790*log(percent_birch_broadleaves))
  )
}

#' @param percent_birch_broadleaves Sum of Percent of of birch and other
#' broadleaves. (stems)
#' @param altitude meters above sea level.
#' @param latitude Degrees N.
#' @rdname Lexerod_2005_count
#' @export
#' @details Broadleaf observations: 166. Adj.R^2 = 0.159. CV(percent): 18.4.
Lexerod_2005_recruitment_count_5cm_Norway_Broadleaves <- function(
  altitude,
  latitude,
  SI40,
  age,
  stems_ha,
  percent_birch_broadleaves
){
  return(
    exp(15.5017 - 0.2691*log(altitude) - 3.6831*log(latitude) + 0.0444*SI40 + 0.2657*log(age) - 0.0002*stems_ha + 0.5514*log(stems_ha) + 0.3727*log(percent_birch_broadleaves))
  )
}

#' Probability of recruitment (5 cm) for a 5 year period in young norwegian stands.
#'
#' @source Lexerød, N., Eid, T. (2005) Recruitment models for Norway Spruce,
#' Scots Pine, Birch and Other Broadleaves in Young Growth Forests in Norway.
#' Silva Fenn. 39(3): 391-406. Available (2022-02-26):
#' \url{https://doi.org/10.14214/sf.376}
#' @description Calculates the probability of occurring ingrowth into >5 cm
#' diameter breast height class) occuring during a 5 years period. These functions
#' are suitable for development class II (Conifers and broadleaves: max total
#' stand ages 20, 20, 25, 30, 35, 45, 55 for SI40 23, 20, 17, 14, 11, 8, 6;
#' Birch: max total stand ages 15, 15, 20, 25, 25, 25, 30 years for SI40 23, 20,
#'  17, 14, 11, 8, 6).
#' @param SI40 Site Index at base-age 40 for species in question. For Broadleaves
#' , use SI40 Birch (?). e.g.
#' [forester::Tveite_1976_height_trajectory_Norway_Pine()],
#' [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()], Strand
#' (1967).
#' @param age Stand age, breast height (1.3m)
#' @param stems_ha Stems per hectare.
#' @param percent_spruce Percent (stems) of Spruce.
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
    percent_spruce
){
  return(
    -17.5779 + 1.4185*log(SI40) + 2.0103*log(age) + 0.8111*log(stems_ha) + 0.9393*log(percent_spruce)
  )
}

#' @param percent_pine Percent (Basal area? Stems?) of Scots Pine.
#' @rdname Lexerod_2005
#' @export
#' @details Scots Pine observations: 897. Misclassified (7.9 percent). Chi^2:
#' 5.7.
Lexerod_2005_prob_recruitment_5cm_Norway_Scots_Pine <- function(
    SI40,
    age,
    stems_ha,
    percent_pine
){
  return(
    -14.6228 + 1.1399*log(SI40) + 0.9231*log(age) + 0.7120*log(stems_ha) + 1.2627*log(percent_pine)
  )
}

#' @param percent_birch_broadleaves Sum of Percent of of birch and other
#' broadleaves. (stems)
#' @rdname Lexerod_2005
#' @export
#' @details Birch observations: 897. Misclassified (6.9 percent). Chi^2:
#' 9.2.
Lexerod_2005_prob_recruitment_5cm_Norway_Birch <- function(
    age,
    stems_ha,
    percent_birch_broadleaves
){
  return(
    -8.2528 + 1.1822*log(age) + 0.2306*log(stems_ha) + 0.9530*log(percent_birch_broadleaves)
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

