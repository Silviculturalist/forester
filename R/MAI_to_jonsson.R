#' MAI to Jonsson
#'
#'@description Class according to Jonsson (1914). OBS class 9 is given for all sites MAI < 1.2 m3sk.
#'
#' @param bonitet MAI, m3sk.
#'
#' @return Class according to Jonsson (1914)
#' @export
#'
#' @examples
#' MAI_to_jonsson(5.3)
MAI_to_jonsson <- function(bonitet){

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
