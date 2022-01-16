#' Height for uppermost root collar from Laasasenaho 1982.
#'
#' @description "When volume is calculated by means of a taper curve,
#' the stump height is needed for defining the integration limit.
#' @details Pine relative SE = 68 percent. \n Spruce relative SE = 38 percent. \n
#' Birch relative SE = 44 percent.
#'
#' @param diameter_cm Diameter at breast height 1.3 m in cm.
#' @param height_m Height of tree, in metres.
#' @param species One of "Picea abies" (default), "Pinus sylvestris",
#' "Betula pendula"
#' or "Betula pubescens"
#'
#' @return Height of the uppermost root collar (cm)
#' @export
Laasasenaho_1982_root_collar_Finland <- function(
  diameter_cm,
  height_m,
  species="Picea abies"
){
  Pine <- ifelse(species=="Pinus sylvestris",1,0)
  Spruce <- ifelse(species=="Picea abies",1,0)
  Birch <- ifelse(species=="Betula pendula" | species=="Betula pubescens",1,0)

  return(
    Pine*(0.4456*diameter_cm + 0.0952*height_m) + Spruce*(0.5089*diameter_cm + 0.5600*height_m) + Birch*(0.4862*diameter_cm + 0.4979*height_m)
  )

}
