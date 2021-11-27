#' Height trajectories for Scots Pine in Norway from Tveite 1976
#'
#' @source Tveite, B. 1976. Bonitetskurver for furu. Unpublished. in: Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 273 function (1)
#'
#' @param Height Top Height, metres.
#' @param Age Age at breast height.
#' @param Age2 Age at breast height at which to calculate new height.
#'
#' @return Top Height at age at breast height == Age2
#' @export
#'
#' @examples
#'#' #What was the height of a Pine currently 17m and 40 years old when it was 20 years at breast height?.
#' Tveite_1976_height_trajectory_Norway_Pine(Height = 17,Age = 40,Age2 = 20)
Tveite_1976_height_trajectory_Norway_Pine <- function(
  Height,
  Age,
  Age2
){

  H14 <- 24.7*(1-exp((-0.02105*Age)))^1.18029 +1.3

  Difference_at_Age <- ifelse(Age<=119,
                              3 +(0.0394624*(Age-40)) - (0.0649695*  (((Age-40)^2)/(100))) + (0.487394*(((Age-40)^3)/(100000))) - (0.141827*(((Age-40)^4)/(10000000))),
                              3.913)

  Difference_at_Age <- Difference_at_Age/3

  #Ratio between anamorphic curves is the same throughout.
  number_of_differences <- (Height - H14)/Difference_at_Age

  H14_2 <- 24.7*(1-exp((-0.02105*Age2)))^1.18029 +1.3

  Difference_at_Age2 <- ifelse(Age2<=119,
                               3 +(0.0394624*(Age2-40)) - (0.0649695*  (((Age2-40)^2)/(100))) + (0.487394*(((Age2-40)^3)/(100000))) - (0.141827*(((Age2-40)^4)/(10000000))),
                               3.913)

  Difference_at_Age2 <- Difference_at_Age2/3


  Height2 <- H14_2 + Difference_at_Age2*(number_of_differences)

  return(
    Height2
  )

}

