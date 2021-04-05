#' Create a treelistData object.
#'
#' @description A dataframe containing the treelist for a stand.
#' @param treeID The unique ID of a tree.
#' @param origin_year Year of planting.
#' @param observation_year Year of observation.
#' @param generation Does this tree belong to the current generation of management? 1== current, 2== previous, 3..
#' @param species Tree species
#' @param age Tree total age
#' @param height.m Tree height in metres
#' @param diameter.cm Tree diameter in cm.
#' @param bark.mm Tree bark thickness, in mm.
#' @param double_bark.mm Tree double bark thickness, in mm.
#' @param crown.base.height.m Height to lowest living green branch on a tree.
#' @param volume Tree volume, m3sk.
#' @param coordx X coordinate
#' @param coordy Y coordinate
#'
#' @return
#' @export
#'
#' @examples
treelistData <- function(treeID, origin_year, observation_year, species, age, height.m, diameter.cm, bark.mm, double_bark.mm, crown.base.height.m, coordx, coordy){

}
