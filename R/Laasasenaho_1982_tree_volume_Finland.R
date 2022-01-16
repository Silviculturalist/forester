#' Guideline volume curves from diameter only by Laasasenaho 1982.
#'
#' @description "The accuracy of equations based on diameter at breast height
#' alone is in any case poor, and the equations are material specific.
#' In some inventory tasks guideline volume curves, based on diameter only,
#' are required. The following equations....can be used for this type of task."
#' @details Pine relative SE = 17.2 percent. \n Spruce relative SE = 18.7 percent. \n
#' Birch relative SE = 18.8 percent.
#'
#' @param diameter_cm Diameter at breast height 1.3 m in cm.
#' @param species One of "Picea abies" (default), "Pinus sylvestris",
#' "Betula pendula"
#' or "Betula pubescens"
#'
#' @return Volume of the tree (dm^3)
#'
#' @seealso For diameter and height function, see [forester::Laasasenaho_1982_tree_volume_2_Finland()]
#'
#' @export
Laasasenaho_1982_tree_volume_Finland <- function(
  diameter_cm,
  species="Picea abies"
){
  Pine <- ifelse(species=="Pinus sylvestris",1,0)
  Spruce <- ifelse(species=="Picea abies",1,0)
  Birch <- ifelse(species=="Betula pendula" | species=="Betula pubescens",1,0)

  return(
    exp(Pine*(-5.39417 + 3.48060 * log(2+1.25*diameter_cm)-0.039884*diameter_cm) +
        Spruce*(-5.39934 + 3.46468*log(2+1.25*diameter_cm)-0.0273199*diameter_cm) +
          Birch*(-5.41948 + 3.57630*log(2+1.25*diameter_cm)-0.0395855*diameter_cm))
  )

}
