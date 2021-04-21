#' Construct a tree.
#'
#' @description Initiates a treeInfo object, containing information about a single tree.
#'
#' @param tree_ID Unique tree identifier
#' @param origin_year Year of planting.
#' @param observation_year Year of observation.
#' @param species Species, latin.
#' @param age Age, years.
#' @param age_at_breast_height Age, at breast height. Total age - time to breast height.
#' @param height.m Height, metres.
#' @param diameter.cm Diameter at breast height, cm.
#' @param bark.mm Bark thickness, mm.
#' @param double_bark.mm Double bark thickness, mm. If not provided, 2x bark.mm
#' @param crown.base.height.m Height from ground to base of tree crown.
#' @param generation Tree generation
#' @param volume Volume in m3sk.
#' @param basal_area Basal area of tree in m2.
#' @param coord_x X-coordinate
#' @param coord_y Y-coordinate
#'
#' @return
#' @export
#'
#' @examples
treeInfo <- function(tree_ID, origin_year ,observation_year, species, age, height.m, diameter.cm, bark.mm, double_bark.mm, crown_base_height.m, main_cohort, coord_x, coord_y){

  if(missing(double_bark.mm)) {
    double_bark.mm <- 2*bark.mm
  }

  if(missing(age)) {
    age <- observation_year - origin_year
  }

  if(missing(origin_year)){
    origin_year <- observation_year - age
  }

  value <- list(tree_ID=tree_ID,origin_year=origin_year,observation_year=observation_year,species=species,age=age,height.m=height.m,diameter.cm=diameter.cm,bark.mm=bark.mm,double_bark.mm=double_bark.mm, crown_base_height.m=crown_base_height.m,main_cohort=main_cohort, coord_x=coord_x, coord_y=coord_y)

  attr(value, "class") <- c("tree", "list")
  value
}
