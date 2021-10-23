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
#'
#' @examples
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
