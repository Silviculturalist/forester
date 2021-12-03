#' Height trajectories for Norway Spruce in Norway from Tveite 1977
#'
#' @description Based on permanent plot records from the eastern and middle regions of Norway - intended for use in whole country apart from Western regions. Data consists of 128 height/age series of different length. Construction method makes the reference age arbitrary. 40 years typical in Norway. At ages above 100, the ratio between the anamorphic curves is held constant.
#' Not recommended for use on stands younger than 15-20 years of age at breast height.
#' This function returns the regular height trajectories (Equations VII.8 and VII.9)
#'
#' @source Tveite, Bjørn (1977) Bonitetskurver for gran. (Site-index curves for Norway Spruce (Picea abies (L.) Karst).) Meddr norsk inst. skogforsk. 33:1-84.
#'
#' @param dominant_height Dominant Height, metres.
#' @param age Age at breast height.
#' @param age2 Age at breast height at which to calculate new height.
#'
#' @return Top Height at age at breast height == age2
#' @export
#'
#' @examples
#' #What was the height of a Spruce currently 17m and 40 years old when it was 20 years at breast height?.
#' Tveite_1977_height_trajectory_Norway_Norway_Spruce(Height = 17,age = 40,age2 = 20)
Tveite_1977_height_trajectory_Norway_Norway_Spruce <- function(dominant_height,age,age2){

  H17 <- ((age+5.5)/(4.30606+0.164818*(age+5.5)))^2.1

  Difference_at_age <- ifelse(age<=100,
    3 +(0.040183*(age-40)) - (0.104701*  (((age-40)^2)/(10^2))) + (0.679104*(((age-40)^3)/(10^5))) + (0.184402*(((age-40)^4)/(10^6))) - (0.224249*(((age-40)^5)/(10^8))),
    3.755)

  Difference_at_age <- Difference_at_age/3

  #Ratio between anamorphic curves is the same throughout.
  number_of_differences <- (dominant_height - H17)/Difference_at_age

  H17_2 <- ((age2+5.5)/(4.30606+0.164818*(age2+5.5)))^2.1

  Difference_at_age2 <- ifelse(age2<=100,
                              3 +(0.040183*(age2-40)) - (0.104701*  (((age2-40)^2)/(10^2))) + (0.679104*(((age2-40)^3)/(10^5))) + (0.184402*(((age2-40)^4)/(10^6))) - (0.224249*(((age2-40)^5)/(10^8))),
                              3.755)

  Difference_at_age2 <- Difference_at_age2/3


  Height2 <- H17_2 + Difference_at_age2*(number_of_differences)

    return(
      Height2
      )

}
