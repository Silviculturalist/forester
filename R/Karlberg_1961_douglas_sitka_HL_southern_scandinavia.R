#' Lorey's Mean Height (HL) development in stands of Douglas fir in southern
#' Scandinavia
#'
#' @source Karlberg, S. (1961) Development and yield of Douglas fir (Pseudotsuga
#'  taxifolia (Poir.) Britt.) and Sitka spruce (Picea sitchensis (Bong.) Carr.)
#'  in southern Scandinavia and on the Pacific Coast. Diss. Royal College of
#'  Forestry. Nr. 34. 141 pages. Emil Kihlströms Tryckeri AB. Stockholm.
#'  Available Online (2022-04-03):
#'  \url{https://pub.epsilon.slu.se/9527/1/kungl_skogshogskolans_skrifter_034.pdf}
#'
#'  @description HL was chosen to be able to appropriately compared with Möller
#'  (1933).
#'
#'  Site classes were therefore chosen as follows:
#'  \table{lr}{
#'  Site Class \tab HL50 \cr
#'  I \tab 32 (30.5-33.5) \cr
#'  II \tab 29 (27.5-30.5) \cr
#'  III \tab 26 (24.5-27.5) \cr
#'  IV \tab 23 (21.5-24.5) \cr
#'  }
#'
#' @param age Observed stand age (total)
#' @param age2 Desired stand age (total)
#' @param HL Observed Lorey's Mean Height (HL)
#'
#' @return Predicted HL at age2.
#' @export

Karlberg_1961_height_trajectory_HL_Scandinavia_Douglas_Fir <- function(
    age,
    age2,
    HL){

  #Find H50 which produces age, HL.
  h50 <- optimise(f= function(h50,age,HL) abs(HL-2*h50*pnorm((log10(age)-1.6990)/(0.24 + 0.0088*h50))),lower = 0,upper=100, age={age},HL={HL})[[1]]

  #return predicted HL at age2.
  return(
    2*h50*pnorm((log10(age2)-1.6990)/(0.24 + 0.0088*h50))
  )

}


#' Lorey's Mean Height (HL) development in stands of Sitka spruce in southern
#' Scandinavia
#'
#' @source Karlberg, S. (1961) Development and yield of Douglas fir (Pseudotsuga
#'  taxifolia (Poir.) Britt.) and Sitka spruce (Picea sitchensis (Bong.) Carr.)
#'  in southern Scandinavia and on the Pacific Coast. Diss. Royal College of
#'  Forestry. Nr. 34. 141 pages. Emil Kihlströms Tryckeri AB. Stockholm.
#'  Available Online (2022-04-03):
#'  \url{https://pub.epsilon.slu.se/9527/1/kungl_skogshogskolans_skrifter_034.pdf}
#'
#'  @description HL was chosen to be able to appropriately compared with Möller
#'  (1933).
#'
#'  Site classes were therefore chosen as follows:
#'  \table{lr}{
#'  Site Class \tab HL50 \cr
#'  I \tab 30 (28.5-31.5) \cr
#'  II \tab 27 (25.5-28.5) \cr
#'  III \tab 24 (22.5-25.5) \cr
#'  IV \tab 21 (19.5-22.5) \cr
#'  }
#'
#' @param age Observed stand age (total)
#' @param age2 Desired stand age (total)
#' @param HL Observed Lorey's Mean Height (HL)
#'
#' @return Predicted HL at age2.
#' @export

Karlberg_1961_height_trajectory_HL_southern_scandinavia_Sitka_Spruce <- function(
    age,
    age2,
    HL
){

  #Find H50 which produces age, HL.
  h50 <- optimise(f= function(h50,age,HL) abs(HL-2*h50*pnorm((log10(age)-1.6990)/(0.21 + 0.0066*h50))),lower = 0,upper=100, age={age},HL={HL})[[1]]

  #return predicted HL at age2.
  return(
    2*h50*pnorm((log10(age2)-1.6990)/(0.21 + 0.0066*h50))
  )

}

