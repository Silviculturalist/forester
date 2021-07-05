#' Stems per hectare for young stands of norway Spruce in southern Sweden.
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.4.
#'
#'
#' Number of sites = 183.
#'
#' Mean stems per ha: 7.6675
#'
#' Standard deviation mean stems per ha: 0.501
#'
#'
#' standard deviation about the function: 0.381
#'
#' R = 0.67
#'
#' Cp = 5 (Daniel & Wood 1971, pp. 86)
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 42. pp. 75.
#'
#' @param stand_density 0-1.
#' @param pre_commercial_thinning Has the stand been pre-commercially thinned?
#' @param SI Site index H 100.
#' @param altitude meters above sea level.
#' @param age_at_breast_height Age of stand at breast height. For example [forester::Hagglund_age_to_height()]
#' @broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#' @even_or_somewhat_uneven_aged TRUE / FALSE
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @md
#' @export
#'
#' @examples
#' ggplot()+
#'geom_function(aes(linetype="G20"), fun= function(x) Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Spruce(
#'  stand_density = x,pre_commercial_thinning = FALSE,SI = 20,age_at_breast_height = 44.6,altitude=100,broadleaves_percent_of_basal_area = 0,even_or_somewhat_uneven_aged = TRUE
#') )+
#'  geom_function(aes(linetype="G32"), fun= function(x) Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Spruce(
#'    stand_density = x,pre_commercial_thinning = FALSE,SI = 32,age_at_breast_height = 21,altitude=100,broadleaves_percent_of_basal_area = 0,even_or_somewhat_uneven_aged = TRUE
#'  ) )+
#' xlim(c(0.4,1.0))
#'

Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Spruce <- function(
  stand_density=0.65,
  pre_commercial_thinning,
  SI,
  age_at_breast_height,
  altitude,
  broadleaves_percent_of_basal_area,
  even_or_somewhat_uneven_aged
){
  if(broadleaves_percent_of_basal_area>40){
    warning("Too high percent broadleaves of basal area, outside of material.")
  } else if(broadleaves_percent_of_basal_area<0){
    stop("Broadleaves_percent_of_basal_area cannot be less than 0 %.")
  }


  stand_density <- stand_density*10
  altitude <- altitude/100

  si22 <- if(SI>22){
    TRUE
  } else {
    FALSE
  }

  return(
    exp(
      6.2064+
        +0.066*altitude+
        +0.319*stand_density+
        -0.081*((stand_density^3)/100)
        -0.286*si22+
        -0.007*age_at_breast_height+
        +0.006*broadleaves_percent_of_basal_area+
        -0.167*even_or_somewhat_uneven_aged
    )
  )


}
