#' Height trajectories for Scots Pine in Norway from Tveite 1976
#'
#' @source Tveite, B. 1976. Bonitetskurver for furu. Unpublished. in: Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 273 function (1)
#'
#' @param dominant_height Dominant height, metres.
#' @param age Age at breast height.
#' @param age2 Age at breast height at which to calculate new height.
#'
#' @return Dominant height at age at breast height == age2
#' @export
#'
#' @examples
#'#' #What was the height of a Pine currently 17m and 40 years old when it was 20 years at breast height?.
#' Tveite_1976_height_trajectory_Norway_Pine(dominant_height = 17,age = 40,age2 = 20)
Tveite_1976_height_trajectory_Norway_Pine <- function(
  dominant_height,
  age,
  age2
){

  H14 <- 24.7*(1-exp((-0.02105*age)))^1.18029 +1.3

  Difference_at_Age <- ifelse(age<=119,
                              3 +(0.0394624*(age-40)) - (0.0649695*  (((age-40)^2)/(100))) + (0.487394*(((age-40)^3)/(100000))) - (0.141827*(((age-40)^4)/(10000000))),
                              3.913)

  Difference_at_Age <- Difference_at_Age/3

  #Ratio between anamorphic curves is the same throughout.
  number_of_differences <- (dominant_height - H14)/Difference_at_Age

  H14_2 <- 24.7*(1-exp((-0.02105*age2)))^1.18029 +1.3

  Difference_at_age2 <- ifelse(age2<=119,
                               3 +(0.0394624*(age2-40)) - (0.0649695*  (((age2-40)^2)/(100))) + (0.487394*(((age2-40)^3)/(100000))) - (0.141827*(((age2-40)^4)/(10000000))),
                               3.913)

  Difference_at_age2 <- Difference_at_age2/3


  Height2 <- H14_2 + Difference_at_age2*(number_of_differences)

  return(
    Height2
  )

}

