#' Thinning response function
#'
#' @description After table 3 in Hynynen, J., Eerikäinen, K., Mäkinen, H., Valkonen, S. (2019) Growth response to cuttings in Norway Spruce stands under even-aged and uneven-aged management.
#'
#' Available at \url{https://www.sciencedirect.com/science/article/pii/S0378112718318838}
#'
#'
#' \strong{N.B. : Returns response in period, i.e. 0-5, 5-10, 10-15}
#'
#' @param degreedays Effective temperature sum, threshold +5°C
#' @param isOMT Binary, if site type is Oxalis-Myrtillus (mesic) )== 1
#' @param BasalArea stand basal area, m^2ha^-1
#' @param height_weighted_w_ba mean height weighted with basal area, m.
#' @param REM_percent removed percentage in cutting, percent of stand basal area.
#' @param management 1 if evenaged, 2 if unevenaged
#' @param CUT0.5 True or False, if between 0-5 yr since thinning
#' @param CUT5.10 True or False, if between 5-10 yr since thinning
#' @param CUT10.15 True or False, if between 10-15 yr since thinning
#'
#' @return Mean annual stand basal area growth during 5-year growth period, m^2ha^{-1}year^{-1}
#' @export
#'
#' @examples thinning_response_Hynonen_2019(BasalArea=30, height_weighted_w_ba=15, isOMT=F, degreedays=1577, CUT10.15=T)
thinning_response_Hynynen_2019 <- function(BasalArea, height_weighted_w_ba, REM_percent=33, management=1, isOMT, degreedays, CUT0.5=F, CUT5.10=F, CUT10.15=F){

  if(management==1){
    UEA <- 0
    EA <- 1
  } else if(management==2){
    UEA <- 1
    EA <- 0
  }

# Checks  -----------------------------------------------------------------



  BasalArea %>%
    assert_is_numeric()

  height_weighted_w_ba %>%
    assert_is_numeric()

  REM_percent %>%
    assert_is_numeric() %>%
    is_in_range(lower=0, upper=100)

  management %>%
    assert_is_numeric()

  degreedays %>% assert_is_numeric()

  CUT0.5 %>% assert_is_a_bool()


  CUT5.10 %>% assert_is_a_bool()

  CUT10.15 %>% assert_is_a_bool()


# Function ----------------------------------------------------------------

  -3.5181 + 0.3859*log(degreedays) + 0.09006*isOMT + 0.05175*log(BasalArea) + 29.2065*((1/height_weighted_w_ba)) + -144.18*(1/(height_weighted_w_ba^2)) + -0.2407*UEA*CUT0.5*REM_percent + -0.09355*UEA*REM_percent*CUT5.10 + 0.1080*EA*REM_percent*CUT5.10 + 0.1940*EA*REM_percent*CUT10.15




}
