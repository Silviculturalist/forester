#' Stems per hectare for young stands of norway Spruce in northern Sweden.
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.3.
#'
#'
#' Number of sites = 262.
#'
#' Mean stems per ha: 7.6012
#'
#' Standard deviation mean stems per ha: 0.533
#'
#'
#' standard deviation about the function: 0.416
#'
#' R = 0.64
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
#' @broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#' @even_or_somewhat_uneven_aged TRUE / FALSE
#' @uneven_aged TRUE/FALSE
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @export
#'
#' @examples
#'
#'ggplot()+
#'geom_function(aes(linetype=">22, 200masl"), fun= function(x)
#'  Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Spruce(stand_density = x,
#'                                                                           pre_commercial_thinning = FALSE,
#'                                                                          SI = 23,
#'                                                                          altitude = 200,
#'                                                                          broadleaves_percent_of_basal_area = 0,
#'                                                                          even_or_somewhat_uneven_aged = TRUE,
#'                                                                           uneven_aged = FALSE) )+
#'  geom_function(aes(linetype="<22, 500masl"), fun= function(x)
#'    Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Spruce(stand_density = x,
#'                                                                             pre_commercial_thinning = FALSE,
#'                                                                             SI = 21,
#'                                                                             altitude = 500,
#'                                                                             broadleaves_percent_of_basal_area = 0,
#'                                                                             even_or_somewhat_uneven_aged = TRUE,
#'                                                                             uneven_aged = FALSE) )+
#'  xlim(c(0.4,0.9))+
#'  geom_function(aes(linetype="h<22, 200masl"), fun= function (x)
#'    Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Spruce(stand_density = x,
#'                                                                             pre_commercial_thinning = FALSE,
#'                                                                            SI = 21,
#'                                                                            altitude = 200,
#'                                                                            broadleaves_percent_of_basal_area = 0,
#'                                                                             even_or_somewhat_uneven_aged = TRUE,
#'                                                                            uneven_aged = FALSE ))
#'
#'
#'
Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Spruce <- function(
  stand_density=0.65,
  pre_commercial_thinning,
  SI,
  altitude,
  broadleaves_percent_of_basal_area,
  even_or_somewhat_uneven_aged,
  uneven_aged
){
  if(broadleaves_percent_of_basal_area>40){
    warning("Too high percent broadleaves of basal area, outside of material.")
  } else if(broadleaves_percent_of_basal_area<0){
    stop("Broadleaves_percent_of_basal_area cannot be less than 0 %.")
  }

  if(uneven_aged==TRUE && even_or_somewhat_uneven_aged==TRUE){
    stop("Only one of 'even_or_somewhat_uneven_aged' or 'uneven_aged' can be TRUE")

  } else if(uneven_aged==FALSE && even_or_somewhat_uneven_aged==FALSE){
    stop("Only one of 'even_or_somewhat_uneven_aged' or 'uneven_aged' can be FALSE")
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
      6.7117+
        +0.118*altitude+
        -0.028*(altitude^2)+
        +0.175*stand_density+
        -0.189*si22+
        +0.006*broadleaves_percent_of_basal_area+
        -0.748*pre_commercial_thinning+
        -0.111*even_or_somewhat_uneven_aged+
        -0.077*uneven_aged
    )
  )


}



