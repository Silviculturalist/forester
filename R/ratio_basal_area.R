#' Ratio of basal area
#' @description This function summarises the basal area of a given tree species as a percentage of the total in the stand for a tree list.
#'
#' @param species Species of interest
#' @param basal_area Total basal area
#' @param treelist A treelist object.
#'
#' @return A ratio.
#' @export
#'
#' @examples
basal_area_ratio <- function(treelist, species, basal_area){
  sum(treelist[species==species]$basal_area) / sum(treelist$basal_area)
}
