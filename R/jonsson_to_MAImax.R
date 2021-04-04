
#' Maximum Mean Annual Increment expected from Jonsson Site Index (1-9)
#'
#' @description  Translates the Jonsson site index, an integer 1 - 8. 9 designates impediment.
#' OBS: The MAI for impediment (class 9) has been set to 0.5.
#'
#' Can also be roughly delineated.
#' MaiMax = 8 * 0.75^(Jonsson_si-2)
#'
#' @param Jonsson_si
#'
#' @return
#' @export
#'
#' @examples
Jonsson_to_MAImax <- function(Jonsson_si){

  assert_all_are_in_range(Jonsson_si, lower = 1,upper = 9)

  if(Jonsson_si==1){
    10.5
  } else if(Jonsson_si==2){
    8
  } else if(Jonsson_si==3){
    6
  } else if(Jonsson_si==4){
    4.5
  } else if(Jonsson_si==5){
    3.4
  } else if(Jonsson_si==6){
    2.5
  } else if(Jonsson_si==7){
    1.8
  } else if(Jonsson_si==8){
    1.2
  } else if(Jonsson_si==9){
    0.5
  }

}
