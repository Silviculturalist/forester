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
