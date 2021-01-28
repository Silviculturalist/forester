#' Construct a tree.
#'
#' @description Initiates a treeInfo object, containing information about a single tree.
#'
#' @param tree_ID Unique tree identifier
#' @param year Year of measurements
#' @param species Species, latin.
#' @param age Age, years.
#' @param height.m Height, metres.
#' @param diameter.cm Diameter at breast height, cm.
#' @param bark.mm Bark thickness, mm.
#' @param double_bark.mm Double bark thickness, mm. If not provided, 2x bark.mm
#' @param crown.base.height.m Height from ground to base of tree crown.
#' @param coord_x X-coordinate
#' @param coord_y Y-coordinate
#'
#' @return
#' @export
#'
#' @examples
treeInfo <- function(tree_ID, year, species, age, height.m, diameter.cm, bark.mm, double_bark.mm, crown_base_height.m, coord_x, coord_y){

  if(!exists(bark.mm)) {
    double_bark.mm <- 2*bark.mm
  }

  value <- list(tree_ID=tree_ID,year=year,species=species,age=age,height.m=height.m,diameter.cm=diameter.cm,bark.mm=bark.mm,double_bark.mm=double_bark.mm, crown_base_height.m=crown_base_height.m, coord_x=coord_x, coord_y=coord_y)

  attr(value, "class") <- "tree"
  value
}
