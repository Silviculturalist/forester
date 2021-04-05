#' Create an empty managementData object.
#'
#' @description A data frame detailing the management events applied to every tree.
#'
#' @param stand_id The Stand ID
#' @param tree_id Tree ID
#' @param year Year of management event.
#' @param management_event Type of management event.
#'
#' @return
#' @export
#'
#' @examples
managementData <- function(stand_id, tree_id, species, volume, year, management_event){
  managementData <- data.frame("stand_id"=stand_id,"tree_id"=tree_id,"species"=species, "year"=year,"management_event"=management_event)
}
