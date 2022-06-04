#' Basal area m2 per hectare before first thinning
#' @source From p. 99 in Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm.
#'
#' @description Author does not reccomend the use of this function if dominant height exceeds 16 meters, since self thinning increases rapidly if the number of stems then also is high.
#' In ranges between 1'100 and 10'000 stems per ha and a dominant height between 7 and 16 metres the function should provide acceptable results. Results may be less accurate at lower site indexes due to no underlying data.
#' @param dominant_height_m Dominant height, m.
#' @param number_of_trees_per_ha Number of trees per hectare.
#' @param planted If site was planted, TRUE. If the site was founded by pre-commercial thinning to a certain plant spacing, FALSE.
#'
#' @return
#' @export

Eriksson_1976_basal_area_before_first_thinning <- function(dominant_height_m, number_of_trees_per_ha,planted){

  b <- ifelse(planted,0.355,0.319)

  return(
    1.0111*((dominant_height_m-1.3)^1.230)*((number_of_trees_per_ha/1000)^b)
  )
}
