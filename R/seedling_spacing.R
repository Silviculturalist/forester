#' Plant Spacing
#'
#' @param distance The \strong{square} system distance between plants
#' @param distance_a The \strong{in-row} distance between plants
#' @param distance_b The \strong{between-row} distance.
#' @param system Planting system, one of \emph{square} or \emph{rectangular}. Defaults to \strong{square}
#' @param stems_per_ha The number of steams planted per hectare
#' @param area_m2 area in square metres. Defaults to 1 hectare, 10'000 sq. metres.
#' @param output One of \emph{stems per ha} or \emph{spacing}
#'
#' @return Prints the output value.
#' @export
#'
#' @examples
#'
#' #Plant spacing for 2500 seedlings per hectare.
#' plant_spacing(stems_per_ha=2500, output="spacing")
#'
plant_spacing <- function(distance, distance_a, distance_b, system="square", stems_per_ha, area_m2=10000, output="spacing"){

  if(output=="stems per ha" & system=="rectangular"){
    area_m2/(dist_a*dist_b)
  }

  if(output=="stems per ha" & system=="square"){
    area_m2/(distance^2)
  }

  if(output=="spacing" & system=="square" | output=="spacing" & missing(system)){
    sqrt(area_m2/stems_per_ha)
  }
}
