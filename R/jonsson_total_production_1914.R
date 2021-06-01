#' Total production including thinned volume according to Jonsson (1914)
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
jonsson_total_production_1914 <- function(stand_age, stand_mean_height,species){

  #total production
  total_production <-
    (100*jonsson_stand_volume_1914(stand_mean_height=stand_mean_height,
                                   species=species))/
    (100-jonsson_thinned_percentage_1914(stand_age=stand_age))

  #MAI
  MAI <- total_production/stand_age

  jonsson_list <- list(total_production,MAI)
  names(jonsson_list) <- c("Total production","Mean Annual Increment")


  return(
    jonsson_list
  )

}
