#' Volume over bark for individual Aspen trees in Norway from Opdahl 1991.
#'
#' @source Opdahl, H. 1991. Bonitet, vekst og produksjon hos osp (Populus tremula L.) i Sør-Norge. (Site-index, growth and yield in Aspen (Populus tremula L.) stands in South Norway.) Medd. Skogforsk. 44(11):1-44. ISBN 82-7169-527-4. ISSN 0803-2866. p. 23
#'
#' @param diameter_cm Diameter of tree in cm.
#' @param height_m Height of tree in m.
#' @param correction Limit as per p.23 if below 8 cm.
#' @param age Age (?)
#'
#' @return
#' @export
#'
#' @examples
Opdahl_1989_volume_over_bark_Norway_Aspen <- function(
  diameter_cm,
  height_m,
  age,
  correction=TRUE
){
  if(isTRUE(correction)){
  if(diameter_cm<8){
    return(0.025)
  }
  }

  return(
    -0.04755 + (diameter_cm*0.00699)-((diameter_cm^2)*0.0023) + ((diameter_cm^2)*height_m*age* 0.00004)
  )
}
