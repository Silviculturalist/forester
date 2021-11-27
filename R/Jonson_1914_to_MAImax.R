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
#'
#' @examples
Jonson_1914_to_MAImax <- function(jonson_class){

  ifelse()

  assert_all_are_in_range (jonson_class, lower = 1,upper = 9)

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
