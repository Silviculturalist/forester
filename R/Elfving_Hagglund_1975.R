#' Basal Area per hectare for young Pine stands in northern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.1
#'
#' N = 426
#'
#' Mean basal area per hectare  = 7.190
#'
#' Standard deviation about the mean = 0.496
#'
#' Standard deviation about the function = 0.334
#'
#' R = 0.76
#'
#'
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 53. pp. 75.
#'
#' @param altitude meters above sea level.
#' @param stand_density 0-1
#' @param stems_per_ha number of stems per hectare.
#' @param SI Site index H100 m.
#' @param dominant_height_m Dominant height of stand.
#' @param broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#'
#'ggplot()+
#'  geom_function(aes(linetype="66N 400masl 16m"), fun= function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Pine(latitude = 66,altitude = 400,
#'                                                                                                                                              stand_density = x, stems_per_ha = "function",SI = 16,dominant_height_m = 12.5,broadleaves_percent_of_basal_area = 0,pre_commercial_thinning = FALSE,even_or_somewhat_uneven_aged = TRUE)
#'  )+
#'  geom_function(aes(linetype="62N 200masl 24m"), fun= function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Pine(latitude = 62,altitude = 200,
#'                                                                                                                                              stand_density = x, stems_per_ha = "function",SI = 24,dominant_height_m = 12.5,broadleaves_percent_of_basal_area = 0,pre_commercial_thinning = FALSE,even_or_somewhat_uneven_aged = TRUE)
#'  )+
#'  xlim(c(0.4,1.0))

Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Pine <- function(
  latitude,
  altitude,
  stand_density=0.65,
  stems_per_ha="function",
  SI,
  dominant_height_m,
  broadleaves_percent_of_basal_area,
  pre_commercial_thinning,
  even_or_somewhat_uneven_aged=TRUE
){

  if(even_or_somewhat_uneven_aged==TRUE){
    uneven_aged <- FALSE
  } else if(even_or_somewhat_uneven_aged==FALSE){
    uneven_aged <- TRUE
  }

  if(stems_per_ha=="function"){
    stems_per_ha <- Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Pine(latitude = latitude,
                                                                                           altitude = altitude,
                                                                                           stand_density = stand_density,
                                                                                           dominant_height_m = dominant_height_m,
                                                                                           pre_commercial_thinning = pre_commercial_thinning,
                                                                                           even_or_somewhat_uneven_aged = even_or_somewhat_uneven_aged,
                                                                                           uneven_aged = uneven_aged)
  }

  return(
    exp(
      -1.604+
        -0.170*log((altitude+1)/10)+
        +0.00993*((altitude+1)/10)+
        +0.314*log(stand_density*10)+
        +0.467*log(stems_per_ha)+
        -0.138*log(SI*10)+
        +1.204*log(dominant_height_m*10)+
        +0.032*log(broadleaves_percent_of_basal_area+1)
    )/100
  )

}

#' Basal Area per hectare for young Spruce stands in northern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.3
#'
#' N = 262
#'
#' Mean basal area per hectare  = 7.411
#'
#' Standard deviation about the mean = 0.452
#'
#' Standard deviation about the function = 0.273
#'
#' R = 0.80
#'
#'
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 53. pp. 75.
#'
#' @param altitude meters above sea level.
#' @param stand_density 0-1
#' @param stems_per_ha number of stems per hectare. or "function".
#' @param SI Site index H100 m
#' @param dominant_height_m Dominant height of stand.
#' @param spatial_distribution 1 if even. 2 if somewhat uneven. 3 if grouped.
#' @param uneven_aged TRUE/FALSE if stand is uneven-aged.
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#' ggplot()+
#' geom_function(aes(linetype="16,500"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce(altitude = 500,stand_density = x,stems_per_ha = "function",SI = 16,broadleaves_percent_of_basal_area = 0,dominant_height_m = 12.5,spatial_distribution = 1,pre_commercial_thinning = FALSE,uneven_aged = FALSE))+
#'   geom_function(aes(linetype="16,200"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce(altitude = 200,stand_density = x,stems_per_ha = "function",SI = 16,broadleaves_percent_of_basal_area = 0,dominant_height_m = 12.5,spatial_distribution = 1,pre_commercial_thinning = FALSE,uneven_aged = FALSE))+
#'   geom_function(aes(linetype="28,200"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce(altitude = 200,stand_density = x,stems_per_ha = "function",SI = 28,broadleaves_percent_of_basal_area = 0,dominant_height_m = 12.5,spatial_distribution = 1,pre_commercial_thinning = FALSE,uneven_aged = FALSE))+
#'   xlim(c(0.4,1.0))+
#'   scale_y_continuous(limits = c(8,28),breaks=c(seq(8,28,2)))

Elfving_Hagglund_1975_initial_basal_area_young_forests_northern_Sweden_Spruce <- function(
  altitude,
  stand_density=0.65,
  stems_per_ha="function",
  SI,
  broadleaves_percent_of_basal_area=0,
  dominant_height_m,
  spatial_distribution,
  pre_commercial_thinning,
  uneven_aged=FALSE
){

  if(uneven_aged==TRUE){
    uneven_aged <- 2
    even_or_somewhat_uneven_aged <- FALSE
  } else {
    uneven_aged <- 1
    even_or_somewhat_uneven_aged <- TRUE
  }

  if(stems_per_ha=="function"){
    stems_per_ha <- Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Spruce(
      stand_density = stand_density,
      pre_commercial_thinning = pre_commercial_thinning,
      SI = SI,
      altitude = altitude,
      broadleaves_percent_of_basal_area = broadleaves_percent_of_basal_area,
      even_or_somewhat_uneven_aged = even_or_somewhat_uneven_aged,
      uneven_aged = ifelse(uneven_aged==2,TRUE,FALSE)
    )
  }

  return(
    exp(
      -1.659+
        -0.125*log(((altitude+1)/10))+
        +0.00918*(((altitude+1))/10)+
        +0.488*log((stand_density)*10)+
        +0.467*log(stems_per_ha)+
        -0.268*log(SI*10)+
        +1.219*log(dominant_height_m*10)+
        +0.153*spatial_distribution+
        -0.055*uneven_aged
    )/100
  )

}

#' Basal Area per hectare for young Pine stands in southern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.2
#'
#' N = 235
#'
#' Mean basal area per hectare  = 7.465
#'
#' Standard deviation about the mean = 0.440
#'
#' Standard deviation about the function = 0.280
#'
#' R = 0.71
#'
#'
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 53. pp. 75.
#'
#' @param altitude meters above sea level.
#' @param stand_density 0-1
#' @param stems_per_ha number of stems per hectare.
#' @param SI Site index H100
#' @param dominant_height Dominant height of stand.
#' @param uneven_aged TRUE/FALSE if stand is uneven-aged.
#' @param age_at_breast_height Age at breast height, years.
#' @param regeneration How was the stand established? One of "culture", "natural regeneration" or "unknown". Only required if age_at_breast_height=="function"
#' @param pre_commercial_thinning TRUE/FALSE, has pre-commercial thinning been done?
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#'
#' ggplot()+
#' geom_function(aes(linetype="h100=16"), fun= function (x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Pine(altitude = 300,
#'                                                                                                                                      stand_density = x,
#'                                                                                                                                      stems_per_ha = "function" ,
#'                                                                                                                                      SI = 16 ,
#'                                                                                                                                      dominant_height_m = 12.5 ,
#'                                                                                                                                      uneven_aged = FALSE,
#'                                                                                                                                      age_at_breast_height = "function" ,
#'                                                                                                                                      regeneration = "culture",
#'                                                                                                                                      pre_commercial_thinning = FALSE))+
#'   geom_function(aes(linetype="h100=24"), fun= function (x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Pine(altitude = 300,
#'                                                                                                                                        stand_density = x,
#'                                                                                                                                        stems_per_ha = "function" ,
#'                                                                                                                                        SI = 24 ,
#'                                                                                                                                        dominant_height_m = 12.5 ,
#'                                                                                                                                        uneven_aged = FALSE,
#'                                                                                                                                        age_at_breast_height = "function" ,
#'                                                                                                                                        regeneration = "culture",
#'                                                                                                                                        pre_commercial_thinning = FALSE))+
#'   xlim(c(0.4,1))+
#'   scale_y_continuous(limits=c(8,24),breaks=seq(8,24,2))
#'
Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Pine <- function(
  altitude,
  stand_density=0.65,
  stems_per_ha="function",
  SI,
  dominant_height_m,
  uneven_aged=FALSE,
  age_at_breast_height="function",
  regeneration,
  pre_commercial_thinning
){
  if(age_at_breast_height=="function"){
    age_at_breast_height <- Hagglund_age_to_height(latitude = latitude,SI = SI,dominant_height_m = dominant_height_m,species = "Pinus sylvestris",regeneration = regeneration)[[2]]
  }

  if(stems_per_ha=="function"){
    stems_per_ha <- Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Pine(stand_density = stand_density ,
                                                                                           pre_commercial_thinning = pre_commercial_thinning ,
                                                                                           age_at_breast_height = age_at_breast_height)
  }

  return(
    exp(
      +1.280+
        -0.089*log((altitude+1)/10)+
        +0.283*log(stand_density*10)+
        +0.370*log(stems_per_ha)+
        -0.174*log(SI*10)+
        +0.878*log(dominant_height_m*10)+
        -0.121*uneven_aged
    )/100
  )

}



#' Basal Area per hectare for young Spruce stands in northern Sweden from Elfving & Hagglund 1975.
#'
#' @details
#'
#' F. 6.4
#'
#' N = 183
#'
#' Mean basal area per hectare  = 7.531
#'
#' Standard deviation about the mean = 0.470
#'
#' Standard deviation about the function = 0.306
#'
#' R = 0.74
#'
#'
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 53. pp. 75.
#'
#' @param altitude meters above sea level.
#' @param stems_per_ha number of stems per hectare.
#' @param dominant_height_m Dominant height of stand.
#' @param broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#' @param spatial_distribution 1 if even. 2 if somewhat uneven. 3 if grouped.
#' @param pre_commercial_thinning TRUE if pre_commercial thinning has taken place.
#' @param stand_density 0-1. Required only if stems_per_ha or age_at_breast_height is "function".
#' @param SI Site index H100 m. Required only if stems_per_ha or age_at_breast_height is "function".
#' @param age_at_breast_height Age of stand at breast height. Required only if stems_per_ha is "function".
#' @param even_or_somewhat_uneven_aged TRUE if even_aged or slightly uneven-aged.
#' @param regeneration One of "culture", "natural regeneration" or "unknown.". Required only if age_to_breast_height is "function".
#'
#' @return Basal area per hectare, m^2.
#' @export
#'
#' @examples
#'
#' ggplot()+
#' geom_function(aes(linetype="G20"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Spruce(altitude = 300,
#'                                                                                                                                    stems_per_ha = "function" ,
#'                                                                                                                                    dominant_height_m = 12.5 ,
#'                                                                                                                                    broadleaves_percent_of_basal_area = 0,
#'                                                                                                                                    spatial_distribution = 1 ,
#'                                                                                                                                    pre_commercial_thinning = FALSE,
#'                                                                                                                                    stand_density = x,
#'                                                                                                                                    SI = 20,
#'                                                                                                                                    age_at_breast_height = "function" ,
#'                                                                                                                                    even_or_somewhat_uneven_aged = TRUE ,
#'                                                                                                                                    regeneration = "culture"))+
#'   geom_function(aes(linetype="G36"), fun = function(x) Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Spruce(altitude = 300,
#'                                                                                                                                      stems_per_ha = "function" ,
#'                                                                                                                                      dominant_height_m = 12.5 ,
#'                                                                                                                                      broadleaves_percent_of_basal_area = 0,
#'                                                                                                                                      spatial_distribution = 1 ,
#'                                                                                                                                      pre_commercial_thinning = FALSE,
#'                                                                                                                                      stand_density = x,
#'                                                                                                                                      SI = 36,
#'                                                                                                                                      age_at_breast_height = "function" ,
#'                                                                                                                                      even_or_somewhat_uneven_aged = TRUE ,
#'                                                                                                                                      regeneration = "culture"))+
#'   xlim(c(0.4,1))+
#'   scale_y_continuous(limits=c(8,28), breaks = seq(8,28,2))
Elfving_Hagglund_1975_initial_basal_area_young_forests_southern_Sweden_Spruce <- function(
  altitude,
  stems_per_ha="function",
  dominant_height_m,
  broadleaves_percent_of_basal_area,
  spatial_distribution,
  pre_commercial_thinning,
  stand_density,
  SI,
  age_at_breast_height="function",
  even_or_somewhat_uneven_aged,
  regeneration="culture"
){

  if(age_at_breast_height=="function"){
    age_at_breast_height <- Hagglund_age_to_height(latitude = latitude,
                                                   SI = SI,
                                                   dominant_height_m = dominant_height_m,
                                                   species = "Picea abies",
                                                   locality = "southern" ,
                                                   regeneration = regeneration)[[1]]
  }

  if(stems_per_ha=="function"){
    stems_per_ha <-
      Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Spruce(stand_density = stand_density,
                                                                             pre_commercial_thinning = pre_commercial_thinning ,
                                                                             SI = SI ,
                                                                             age_at_breast_height = age_at_breast_height,
                                                                             altitude = altitude,
                                                                             broadleaves_percent_of_basal_area = broadleaves_percent_of_basal_area ,
                                                                             even_or_somewhat_uneven_aged = even_or_somewhat_uneven_aged )
  }



  return(
    exp(
      -0.102+
        -0.059*log((altitude+1)/10)+
        +0.584*log(stems_per_ha)+
        +0.723*log(dominant_height_m*10)+
        -0.025*log(broadleaves_percent_of_basal_area+1)+
        -0.098*spatial_distribution
    )/100
  )

}



#' Stems per hectare for young stands of Scots Pine in northern Sweden.
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.1.
#'
#'
#' Number of sites = 426.
#'
#' Mean stems per ha: 7.2151
#'
#' Standard deviation mean stems per ha: 0.571
#'
#'
#' standard deviation about the function: 0.394
#'
#' R = 0.73
#'
#' Cp = 12. (Daniel & Wood 1971, pp. 86)
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 42. pp. 75.
#'
#' @param latitude Latitude, degrees N.
#' @param altitude Altitude, meters above sea level.
#' @param stand_density 0-1.
#' @param dominant_height_m Dominant height of stand, in metres.
#' @param pre_commercial_thinning Has the stand been pre-commercially thinned?
#' @param even_or_somewhat_uneven_aged TRUE/FALSE
#' @param uneven_aged TRUE/FALSE
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @export
Elfving_Hagglund_1975_initial_stems_young_forests_northern_Sweden_Pine<- function(
  latitude,
  altitude,
  stand_density=0.65,
  dominant_height_m,
  pre_commercial_thinning,
  even_or_somewhat_uneven_aged,
  uneven_aged
){
  if(uneven_aged==TRUE && even_or_somewhat_uneven_aged==TRUE){
    stop("Only one of 'even_or_somewhat_uneven_aged' or 'uneven_aged' can be TRUE")

  } else if(uneven_aged==FALSE && even_or_somewhat_uneven_aged==FALSE){
    stop("Only one of 'even_or_somewhat_uneven_aged' or 'uneven_aged' can be FALSE")
  }

  dominant_height_dm <- dominant_height_m*10
  altitude <- altitude/100
  stand_density <- stand_density*10

  return(
    exp(
      8.856+
        -0.033*latitude+
        -0.062*altitude+
        +0.203*stand_density+
        -0.002*dominant_height_dm+
        -0.233*pre_commercial_thinning+
        -0.220*even_or_somewhat_uneven_aged+
        -0.074*uneven_aged
    )
  )



}
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
#' @param broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#' @param even_or_somewhat_uneven_aged TRUE / FALSE
#' @param uneven_aged TRUE/FALSE
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



#' Stems per hectare for young stands of Scots Pine in southern Sweden.
#'
#'
#' @description For stems thicker than 2.5 cm at breast height.
#'
#' @details Function 5.2.
#'
#'
#' Number of sites = 235.
#'
#' Mean stems per ha: 7.3192
#'
#' Standard deviation mean stems per ha: 0.523
#'
#'
#' standard deviation about the function: 0.371
#'
#' R = 0.72
#'
#' Cp = 7. (Daniel & Wood 1971, pp. 86)
#'
#'
#'
#' @source Elfving, B., Hägglund, B. (1975) Utgångslägen för produktionsprognoser: Tall och gran i Sverige.
#' / Initial stands for yield forecasts: Scots pine and Norway spruce in Sweden. Research Notes #38. Dept. of
#' Forest Yield Research. Royal College of Forestry. Stockholm. p. 42. pp. 75.
#'
#' @param stand_density 0-1.
#' @param pre_commercial_thinning Has the stand been pre-commercially thinned?
#' @param age_at_breast_height Stand age at breast height (1.3 m). For 'function', see [forester::Hagglund_age_to_height()]
#' @param latitude Latitude in degrees. Only Required if "age_at_breast_height" is "function".
#' @param SI SI H100 m. Only Required if "age_at_breast_height" is "function".
#' @param dominant_height_m Dominant height of stand. Defaults to 12.5 m. Only Required if "age_at_breast_height" is "function".
#' @param regeneration Method of stand establishment. One of "culture","natural regeneration" or "unknown". Only Required if "age_at_breast_height" is "function".
#'
#' @return Stems thicker than 2.5 cm per ha.
#' @export
Elfving_Hagglund_1975_initial_stems_young_forests_southern_Sweden_Pine<- function(
  stand_density=0.65,
  pre_commercial_thinning,
  age_at_breast_height="function",
  latitude=60,
  SI,
  dominant_height_m=12.5,
  regeneration="culture"
){
  if(age_at_breast_height=="function"){
    age_at_breast_height <- Hagglund_age_to_height(latitude = latitude,
                                                   SI = SI,
                                                   dominant_height_m = dominant_height_m,
                                                   species = "Pinus sylvestris",
                                                   regeneration = regeneration)[[1]]
  }

  stand_density <- stand_density*10

  return(
    exp(
     6.148+
       +0.268*stand_density+
       -0.058*((stand_density^3)/100)+
       -0.006*age_at_breast_height+
       -0.310*pre_commercial_thinning
    )
  )


}

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
#' @param broadleaves_percent_of_basal_area Percent of basal area composed of broadleaves.
#' @param even_or_somewhat_uneven_aged TRUE / FALSE
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
