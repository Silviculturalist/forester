#' Total basal area production before first thinning in stands of Norway Spruce, from Pettersson 1992.
#'
#' @description "The mean losses of basal area were small, averaging 1.7 \% for Spruce
#' and 2.5 \% for Pine up to first thinning. The first thinning was carried out at
#' an average dominant height of 12.5 m for both spruce and pine.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'
#' @param basal_area_before_first_thinning Living basal area before first thinning m2 /ha.
#'
#' @return Total basal area production before first thinning m2 /ha.
#' @export
#'
#' @examples
Pettersson_1992_total_BA_production_before_first_thinning_Spruce <- function(
  basal_area_before_first_thinning
){
  return(
    exp(
      0.044 + 0.980*log(basal_area_before_first_thinning)
    )
  )
}
