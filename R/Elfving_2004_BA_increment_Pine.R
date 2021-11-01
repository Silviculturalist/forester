#' Individual tree basal area growth for Pine from Elfving 2003
#'
#' @description Based on data from permanent plots at the National Forest Inventory.
#'
#' @details R^2 = 0.758
#'
#' @source Elfving, B. (2010) Translated, re-formulated Pro-Memoria for HEUREKA based on Manuscript 2004-01-26. 'Individual-tree basal area growth functions for all Swedish forests'. Available: \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param diameter_cm Diameter of the tree at breast height, 1.3 m
#' @param Basal_area_weighted_mean_diameter_cm Basal area weighted mean diameter of the trees on the plot, cm. \eqn{(\sum{diameter_cm^3} / \sum{diameter_cm^2})}
#' @param BA_sum_of_trees_with_larger_diameter Basal area sum of trees on the plot with larger diameters than that of the target tree.
#' @param Basal_area_Pine Basal area Pine on the plot. m2/ha.
#' @param Basal_area_plot Basal area on the plot, m2/ha.
#' @param Basal_area_stand Basal area in the surrounding stand, m2/ha.
#' @param computed_tree_age Age at breast height computed by [forester::Elfving_2003_single_tree_age_estimation()]
#' @param latitude Latitude, decimal degrees.
#' @param county Swedish county, e.g. [forester::county_sweden()]
#' @param SIS100 Site Index as measured from [forester::SIS_estimate()]
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
#' @param divided_plot 1 for plots described in different parts, where the other part is **not** open land.  0 for full plots (default). (divided_plot and edge_effect cannot be TRUE at the same time)
#' @param edge_effect 1 for a partitioned plot where the other part **is** open land. 0 for full plots (default). (divided_plot and edge_effect cannot be TRUE at the same time)
#' @param uneven_aged 1 if less than 80\% of the main stand volume is within a 20-year age-span.
#' @param fertilised 1 if the plot has been fertilised, otherwise 0 (default).
#' @param thinned 1 if the plot has been thinned, otherwise 0.
#' @param last_thinned Number of growth seasons since the stand was last thinned.
#'
#' @return The 5 - year increase of Basal Area (m^2)
#' @export
#'
#' @examples
Elfving_2004_BA_increment_Pine <- function(
  diameter_cm,
  Basal_area_weighted_mean_diameter_cm,
  BA_sum_of_trees_with_larger_diameter,
  Basal_area_Spruce,
  Basal_area_Pine,
  Basal_area_plot,
  Basal_area_stand,
  computed_tree_age,
  latitude,
  county,
  altitude,
  SIS100,
  vegetation,
  divided_plot=0,
  edge_effect,
  uneven_aged,
  fertilised=0,
  thinned,
  last_thinned

){

  if(divided_plot==1 & edge_effect==1){
    stop("Both divided_plot and edge_effect cannot be 1 at the same time.")
  }

  if(uneven_aged==TRUE){
    assign("computed_tree_age",computed_tree_age*0.9)
  }

  gotland <- ifelse(county=="Gotland",1,0)

  BA_quotient_Pine <- Basal_area_Pine/Basal_area_plot

  rich <- ifelse(vegetation<=9,1,0) #Grass and herb types. No field layer also?

  #TRUE if fertilised plot with shrub vegetation (vegetation<12)
  fertris <- ifelse(fertilised & vegetation<12,1,0)

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  thinned_long_ago <- ifelse(thinned == TRUE &
                              last_thinned > 10 &
                              last_thinned < 25,
                            1, 0)



  return(
    exp(
      3.4176+
        +1.0149*log(diameter_cm+1)+
        -0.3902*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.7731*log(computed_tree_age+20)+
        +0.2218*standard_tree+
        +0.1843*(Basal_area_weighted_mean_diameter_cm/10)+
        -0.3145*log(Basal_area_plot+3)+
        +0.1391*((1-BA_quotient_Pine)^2)+
        -0.0844*gotland+
        +0.1178*((4835-(57.6*latitude)-(0.9*altitude))^2)+ #Odin 1983
        +1.0890*(SIS100/10)+
        -0.2164*((SIS100^2)/100)+
        +0.1011*rich+
        +0.2790*fertris+
        +0.1245*thinned_recently+
        +0.0451*thinned_long_ago+
        +0.0487*divided_plot+
        +0.1368*edge_effect+
        +0.0842*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}
