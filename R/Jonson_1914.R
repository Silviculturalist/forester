#' Jonson 1914 classifier
#'
#' @description Class according to Jonson (1914). OBS class 9 is given for all sites MAI < 1.2 m3sk.
#'
#' @source Jonson, Tor (1914). Om bonitering av skogsmark. Svenska skogsvårdsföreningens tidsskrift. Häfte 5. p. 369-392.
#'
#' @param bonitet MAI, m3sk.
#'
#' @return Class according to Jonson (1914)
#' @export
#'
#' @examples
#' Jonson_1914_MAI_to_class(5.3)
Jonson_1914_MAI_to_class <- function(bonitet){

  if(bonitet>=10.5){
    return(1)
  } else if(bonitet>=8 & bonitet<10.5){
    return(2)
  } else if(bonitet>=6 & bonitet<8){
    return(3)
  } else if(bonitet>=4.5 & bonitet<6){
    return(4)
  } else if(bonitet>=3.4 & bonitet<4.5){
    return(5)
  } else if(bonitet>=2.5 & bonitet<3.4){
    return(6)
  } else if(bonitet>=1.8 & bonitet<2.5){
    return(7)
  } else if(bonitet>=1.2 & bonitet<1.8){
    return(8)
  } else if(bonitet<1.2){
    return(9)
  }
}
#' Expected volume in fully stocked Pine stands according to Jonson (1914)
#'
#' @source Jonson, T. (1914) Om boniteringen av skogsmark, eng: On the
#' estimation of the productivity of forest land. Skogsvårdsföreningens
#' tidsskrift, nr. 5, p. 379.
#'
#' @description Jonson recommends Picea abies only be used as species as
#' certain support with expressed reservation.
#'
#' @param stand_mean_height metres.
#' @param species "Pinus sylvestris" or "Picea abies".
#'
#' @return Stand volume, cubic metres.
#' @export
Jonson_1914_stand_volume <- function(stand_mean_height,species="Pinus
                                      sylvestris"){

  if(species=="Pinus sylvestris"){

  return(
    6*stand_mean_height^(1 + (1/3))
  )

  } else if(species=="Picea abies"){

    return(
      4.2*stand_mean_height^(1.5)
    )

  }

}

#' Tor Jonson's smoothing of Maass' experience tables.
#'
#' @source Jonson, T. (1914) Om boniteringen av skogsmark, eng: On the
#' estimation of the productivity of forest land. Skogsvårdsföreningens
#' tidsskrift, nr. 5, p. 382.
#'
#' @description A smoothing of the experience tables from Maass on the at a
#' stand age thinned volume as a percentage of the total growth.
#'
#' @param stand_age years.
#'
#' @return Percent thinned of total volume.
#' @export
Jonson_1914_thinned_percentage <- function(stand_age){
  return(
    9*(stand_age-29)^(1/3)
  )
}
#' Maximum Mean Annual Increment expected from Jonson Site Class (1-9)
#'
#' @description  Translates the Jonson site class, an integer 1 - 8. 9 designates impediment.
#' OBS: The MAI for impediment (class 9) has been set to 0.5.
#'
#' Can also be roughly delineated according to \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf} p. 5.
#' MaiMax = 8 * 0.75^(jonson_class-2)
#'
#' @param jonson_class Integer 1 - 9
#'
#' @return Maximum mean annual increment m3sk/yr.
#' @export
Jonson_1914_to_MAImax <- function(jonson_class){

  stopifnot(jonson_class%in%1:9)

  if(jonson_class==1){
    10.5
  } else if(jonson_class==2){
    8
  } else if(jonson_class==3){
    6
  } else if(jonson_class==4){
    4.5
  } else if(jonson_class==5){
    3.4
  } else if(jonson_class==6){
    2.5
  } else if(jonson_class==7){
    1.8
  } else if(jonson_class==8){
    1.2
  } else if(jonson_class==9){
    0.5
  }

}
#' Total production including thinned volume according to Jonson (1914)
#'
#' @param stand_age Years
#' @param stand_mean_height Mean Height, meters.
#' @param species "Pinus sylvestris" or "Picea abies".
#'
#' @description The author expresses that "Picea abies" only is offered to
#' provide certain practical support, with expressed reservation.
#'
#' @return List, with element 1: Total production in cubic metres; element 2:
#' Mean Annual Increment.
#' @export
Jonson_1914_total_production <- function(stand_age, stand_mean_height,species){

  #total production
  total_production <-
    (100*Jonson_1914_stand_volume(stand_mean_height=stand_mean_height,
                                   species=species))/
    (100-Jonson_1914_thinned_percentage(stand_age=stand_age))

  #MAI
  MAI <- total_production/stand_age

  Jonson_list <- list(total_production,MAI)
  names(Jonson_list) <- c("Total production","Mean Annual Increment")


  return(
    Jonson_list
  )

}
