#' Individual tree basal area growth for Trivial tree species from Elfving 2004
#'
#' @description Based on data from permanent plots at the National Forest Inventory.
#'
#' The trivial tree species include mostly: Alder \emph{Alnus}, Rowan \emph{Sorbus aucuparia} and Willow \emph{Salix spp.}
#'
#' @details R^2 = 0.569
#'
#' @source Elfving, B. (2010) Translated, re-formulated Pro-Memoria for HEUREKA based on Manuscript 2004-01-26. 'Individual-tree basal area growth functions for all Swedish forests'. Available: \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param diameter_cm Diameter of the tree at breast height, 1.3 m
#' @param BA_sum_of_trees_with_larger_diameter Basal area sum of trees on the plot with larger diameters than that of the target tree.
#' @param Basal_area_plot Basal area on the plot, m2/ha.
#' @param computed_tree_age Age at breast height computed by [forester::Elfving_2003_single_tree_age_estimation()]
#' @param vegetation Vegetation type according to follows Swedish National forest inventory FALTSKIKT:
#' \tabular{ll}{
#' Code \tab Vegetation \cr
#' 1 \tab  Rich-herb without shrubs \cr
#' 2 \tab Rich-herb with shrubs/bilberry \cr
#' 3 \tab Rich-herb with shrubs/lingonberry \cr
#' 4 \tab Low-herb without shrubs \cr
#' 5 \tab Low-herb with shrubs/bilberry \cr
#' 6 \tab Low-herb with shrubs/lingonberry \cr
#' 7 \tab No field layer \cr
#' 8 \tab Broadleaved grass \cr
#' 9 \tab Thinleaved grass \cr
#' 10 \tab Sedge, high \cr
#' 11 \tab Sedge, low \cr
#' 12 \tab Horsetail, Equisetum ssp. \cr
#' 13 \tab Bilberry \cr
#' 14 \tab Lingonberry \cr
#' 15 \tab Crowberry \cr
#' 16 \tab Poor shrub \cr
#' 17 \tab Lichen, frequent occurrence \cr
#' 18 \tab Lichen, dominating \cr
#' }
#' @param SIS100 Site Index as measured from [forester::SIS_estimate()]
#' @param uneven_aged 1 if less than 80\% of the main stand volume is within a 20-year age-span.
#' @param thinned 1 if the plot has been thinned, otherwise 0.
#' @param last_thinned Number of growth seasons since the stand was last thinned.
#'
#' @return The 5 - year increase of Basal Area (m^2)
#' @export

Elfving_2004_BA_increment_Trivial <- function(
  diameter_cm,
  BA_sum_of_trees_with_larger_diameter,
  Basal_area_plot,
  computed_tree_age,
  vegetation,
  SIS100,
  uneven_aged,
  thinned,
  last_thinned

){


  if(uneven_aged==TRUE){
    assign("computed_tree_age",computed_tree_age*0.9)
  }


  herb <- ifelse(vegetation<7,1,0) #herb types.

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      2.1108+
        +0.9418*log(diameter_cm+1)+
        -0.2599*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.3026*log(computed_tree_age+20)+
        -0.2280*log(Basal_area_plot+3)+
        +0.2595*(SIS100/10)+
        +0.4392*herb+
        +0.1561*thinned_recently
    )/10000 #cm^2 to m^2

  )

}
