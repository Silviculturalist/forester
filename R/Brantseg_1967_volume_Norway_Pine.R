#' Simple Volume function for Scots Pine in Norway
#'
#' @source Brantseg, A. 1967. Volume functions and tables for Scots pine.
#'  South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#'
#' @author Modified from hansoleorka/skogR (2022-04-02)
#'
#' @param diameter_cm Diameter of tree at breast height, 1.3 m.
#' @param height_m Height of tree, m.
#' @param above_bark TRUE (default) or FALSE
#' @seealso [forester::Brantseg_1967_volume_detailed_Norway_Scots_Pine()]
#'
#' @return Volume of tree in DM^3
#' @export
Brantseg_1967_volume_Norway_Scots_Pine <- function(
  diameter_cm,
  height_m,
  above_bark=TRUE
){
  stopifnot(above_bark%in%c(TRUE,FALSE,1,0))

  # VOLUM MED BARK I DM3. D <= 12CM ER VFB1 OG D > 12CM ER VFB2.
  if(above_bark){
    v<- ifelse (diameter_cm <= 11, {VFB1 <- 2.912 + 0.039994 * diameter_cm * diameter_cm * height_m - 0.001091 * diameter_cm * height_m * height_m},
                ifelse (diameter_cm > 11, {VFB2 <- 8.6524 + 0.076844 * diameter_cm * diameter_cm + 0.031573 * diameter_cm * diameter_cm * height_m},NA))
  }


  # VOLUM UNDER BARK I DM3. D <= 12CM ER VFU1 OG D > 12CM ER VFU2.
  if(above_bark == FALSE){

    v<-	ifelse (diameter_cm <= 11, {VFU1 <- 2.2922 + 0.040072 * diameter_cm * diameter_cm * height_m + 0.00216 * diameter_cm * height_m * height_m},
                ifelse (diameter_cm > 11, {VFU2 <- -3.5425 + 0.128182 * diameter_cm * diameter_cm + 0.028268 * diameter_cm * diameter_cm * height_m + 0.008216 * diameter_cm * height_m * height_m},NA))
  }

  return(v)


}



#' Detailed volume function for Scots Pine in Norway
#'
#' @source Brantseg, A. 1967. Volume functions and tables for Scots pine.
#'  South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#'
#' @author Modified from hansoleorka/skogR (2022-04-02)  NB inconsistency in comments.
#'
#' @param diameter_cm Diameter at breast height, in cm.
#' @param height_m Total tree height, m.
#' @param crown_base_height.m Height to crown base, m.
#' @param double_bark.mm Thickness of double bark, mm.
#' @param above_bark TRUE (default) or FALSE.
#' @seealso [forester::Brantseg_1967_volume_Norway_Scots_Pine()]
#' [forester::Brantseg_1967_crown_base_height_Norway_Scots_Pine()]
#'
#' @return Volume, dm^3
#' @export
Brantseg_1967_volume_detailed_Norway_Scots_Pine <- function(
    diameter_cm,
    height_m,
    crown_base_height.m,
    double_bark.mm,
    above_bark=TRUE){
  stopifnot(above_bark%in%c(TRUE,FALSE,1,0))

  # VOLUM MED BARK I DM3. D <= 12CM ER VFB1 OG D > 12CM ER VFB2.
  if(above_bark){
    VFB2 <- -9.9793 + 0.204787*diameter_cm*diameter_cm + 0.029966*diameter_cm*diameter_cm*height_m + 0.003539*diameter_cm*diameter_cm*k - 0.002918*diameter_cm*diameter_cm*double_bark.mm # > 10 cm
    VFB1 <- 2.0044 + 0.029886*diameter_cm*diameter_cm + 0.036972*diameter_cm*diameter_cm*height_m # <12 cm
    v<- ifelse (diameter_cm < 11, VFB1,VFB2)
  }

  # VOLUM UNDER BARK I DM3. D <= 12CM ER VFU1 OG D > 12CM ER VFU2.
  if(above_bark == FALSE){
    VFB2 <- -3.7967 + 0.137902*diameter_cm*diameter_cm + 0.026031*diameter_cm*diameter_cm*height_m + 0.005498*diameter_cm*height_m*height_m + 0.006482*diameter_cm*diameter_cm*k # >= 10 cm
    VFB1 <- 2.2922 + 0.040072*diameter_cm*diameter_cm*height_m + 0.00216*diameter_cm*height_m*height_m # <12 cm
    v<- ifelse (diameter_cm < 11, VFB1,VFB2)
    }

  return(v)
}


#' Height to crown base for Scots Pine in Norway
#'
#' @source Brantseg, A. 1967. Volume functions and tables for Scots pine.
#'  South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#'
#'  @author Modified from hansoleorka/skogR (2022-04-02)
#'
#' @param diameter_cm Diameter of tree in cm at breast height, 1.3m.
#' @param height_m Total height of tree, m.
#'
#' @return Height to the tree crown base.
#' @export
Brantseg_1967_crown_base_height_Norway_Scots_Pine <- function(
    diameter_cm,
    height_m){
  return(
    4.1203 - 0.002817*diameter_cm*diameter_cm + 0.26234*height_m*height_m - 0.3184*(diameter_cm/height_m)*(diameter_cm/height_m)
  )
}
