#' Volume curves from diameter only by Laasasenaho 1982.
#'
#' @source Laasasenaho, J. 1982. Taper curve and volume functions for Pine, Spruce and Birch. Commun. Inst. For. Fenn. 108:1-74. p. 42.
#'
#' @description "The accuracy of equations based on diameter at breast height
#' alone is in any case poor, and the equations are material specific.
#' In some inventory tasks guideline volume curves, based on diameter only,
#' are required. The following equations....can be used for this type of task."
#' @details Pine relative SE with bark (without bark) = 7.10 (7.5) percent. \n
#' Spruce relative SE = 7.47 (8.1) percent. \n
#' Birch relative SE = 8.23 (8.4) percent.
#'
#' @param diameter_cm Diameter at breast height 1.3 m in cm.
#' @param height_m Height of the tree in metres.
#' @param bark TRUE (default) for volume including bark, FALSE for volume under bark. If FALSE, diameter_cm is assumed to be under bark.
#' @param species One of "Picea abies" (default), "Pinus sylvestris",
#' "Betula pendula"
#' or "Betula pubescens"
#'
#' @seealso For diameter only function, see [forester::Laasasenaho_1982_tree_volume_Finland()]
#'
#' @return Volume of the tree (dm^3)
#' @export
Laasasenaho_1982_tree_volume_2_Finland <- function(
  diameter_cm,
  height_m,
  bark=TRUE,
  species="Picea abies"
){
  if(species%in%c("Pinus sylvestris","Picea abies") & height_m< 3 | species%in%c("Betula pendula","Betula pubescens") & height_m<4){
    warning("Tree height is lower than recommended. Can cause odd results cf. p.43.")
  }

  if(!(species%in%c("Picea abies","Pinus sylvestris","Betula pendula","Betula pubescens"))){
    warning("Unknown tree species, returning NA")
    return(
      NA
      )
  }

  species <- dplyr::case_when(species=="Pinus sylvestris"~1,
                              species=="Picea abies"~2,
                              startsWith(species,"Betula")~3)

  vars <- ifelse(bark,matrix(c(0.036089,2.01395,0.99676,2.07025,-1.07209,
                   0.022927,1.91505,0.99146,2.82541,-1.53547,
                   0.011197,2.10253,0.98600,3.98519,-2.65900),ncol=5,byrow=TRUE),matrix(c(exp(-3.61554),exp(-3.88390),exp(-4.45962),2.05534,1.89496,2.06695,exp(-0.0057527),exp(-0.0091650),exp(-0.0115929),2.30886,2.98696,3.95373,-1.21013,-1.64418,-2.61998),ncol=5))



  return(
    (vars[species,1])*(d^vars[species,2])*(vars[species,3]^d)*(h^vars[species,4])*((h-1.3)^vars[species,5])
  )

}

#Vectorise
Laasasenaho_1982_tree_volume_2_Finland <- Vectorize(Laasasenaho_1982_tree_volume_2_Finland)
