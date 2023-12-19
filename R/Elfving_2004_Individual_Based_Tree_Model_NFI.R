#' Elfving Basal Area Increment functions for Individual Trees from NFI data.
#' @title Basal Area Increment function for Aspen
#' @description Based on data from permanent plots at the National Forest Inventory.
#'
#' @source [Elfving, B. (2010) Translated, re-formulated Pro-Memoria for HEUREKA based on Manuscript 2004-01-26. 'Individual-tree basal area growth functions for all Swedish forests'.](https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\\%28Elfving_rapportutkast\\%29.pdf)
#' @details R^2 = 0.723
#' @param diameter_cm Diameter of the tree at breast height, 1.3 m
#' @param Basal_area_weighted_mean_diameter_cm Basal area weighted mean diameter of the trees on the plot, cm. \eqn{(\sum{diameter_cm^3} / \sum{diameter_cm^2})}
#' @param BA_sum_of_trees_with_larger_diameter Basal area sum of trees on the plot with larger diameters than that of the target tree.
#' @param distance_to_coast_km Distance in kilometres to the nearest Swedish coast, e.g. [forester::coast_distance()]
#' @param stems Number of stems per hectare.
#' @param SS_diam Sum of Squared Diameters, cm.
#' @param BA_Aspen Basal area Aspen on the plot. m2/ha.
#' @param BA_Beech Basal area Beech on the plot. m2/ha.
#' @param BA_Birch Basal area Birch on the plot. m2/ha.
#' @param BA_Oak Basal area Oak on the plot. m2/ha.
#' @param BA_Pine Basal area Scots Pine on the plot. m2/ha.
#' @param BA_Spruce Basal area Norway Spruce on the plot. m2/ha.
#' @param Basal_area_plot Basal area on the plot, m2/ha.
#' @param Basal_area_stand Basal area in the surrounding stand, m2/ha.
#' @param computed_tree_age Age at breast height computed by [forester::Elfving_2003_single_tree_age_estimation()]
#' @param latitude Latitude, decimal degrees.
#' @param altitude Altitude, meters above sea level.
#' @param SIS Site Index predicted from Stand factors [forester::Hagglund_Lundmark_1979_Site_Index_Prediction()]
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
#' @param uneven_aged 1 if less than 80\% of the main stand volume is within a 20-year age-span.
#' @param fertilised 1 if the plot has been fertilised, otherwise 0 (default).
#' @param thinned 1 if the plot has been thinned, otherwise 0.
#' @param last_thinned Number of growth seasons since the stand was last thinned.
#' @param divided_plot 1 if plot has been divided, otherwise 0. Always 0 for prediction.
#' @param edge_effect 1 if plot has been divided towards open land, otherwise 0. Both divided_plot and edge_effect cannot be 1 at the same time. Always 0 for prediction.
#' @param gotland 1 if plot is on the island of Gotland, otherwise 0.
#'
#' @return The 5 - year increase of Basal Area (m^2)
#' @name ElfvingIBM
#' @export
Elfving_2004_BA_increment_Aspen <- function(
    diameter_cm,
    Basal_area_weighted_mean_diameter_cm,
    distance_to_coast_km,
    BA_sum_of_trees_with_larger_diameter,
    BA_Aspen,
    Basal_area_plot,
    Basal_area_stand,
    computed_tree_age,
    latitude,
    altitude,
    vegetation,
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

  BA_quotient_Aspen <- BA_Aspen/Basal_area_plot

  rich <- ifelse(vegetation<=9,1,0) #Grass and herb types. No field layer also?

  #TRUE if fertilised plot with shrub vegetation (vegetation<12)
  fertris <- ifelse(fertilised & vegetation<12,1,0)

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      0.9945+
        +1.9071*log(diameter_cm+1)+
        -0.3313*(diameter_cm/10)+
        -0.3040*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.4058*log(computed_tree_age+20)+
        -0.1981*log(Basal_area_plot+3)+
        -0.5967*sqrt(BA_quotient_Aspen)+
        +0.4408*(4835-(57.6*latitude)-(0.9*altitude))+#Odin 1983
        +0.4759*rich+
        +0.2143*thinned_recently+
        +0.2427*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}


#' @title Individual tree basal area growth for Beech from Elfving 2004
#' @details R^2 = 0.722
#' @export
#' @rdname ElfvingIBM
Elfving_2004_BA_increment_Beech <- function(
    diameter_cm,
    Basal_area_weighted_mean_diameter_cm,
    distance_to_coast_km,
    BA_sum_of_trees_with_larger_diameter,
    BA_Beech,
    Basal_area_plot,
    Basal_area_stand,
    computed_tree_age,
    latitude,
    altitude,
    vegetation,
    SIS,
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

  BA_quotient_Beech <- BA_Beech/Basal_area_plot

  rich <- ifelse(vegetation<=9,1,0) #Grass and herb types. No field layer also?

  #TRUE if fertilised plot with shrub vegetation (vegetation<12)
  fertris <- ifelse(fertilised & vegetation<12,1,0)

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      1.7005+
        +2.5823*log(diameter_cm+1)+
        -0.3758*(diameter_cm/10)+
        -0.2079*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.4478*log(computed_tree_age+20)+
        -0.5348*log(Basal_area_plot+3)+
        -0.9304*sqrt(BA_quotient_Beech)+
        -0.1906*(latitude-50)+
        +0.3055*(SIS/10)+
        +0.2200*thinned_recently+
        +0.2009*divided_plot+
        +0.2669*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}

#' @title Individual tree basal area growth for Birch from Elfving 2004
#' @details R^2 = 0.555
#' @export
#' @rdname ElfvingIBM
Elfving_2004_BA_increment_Birch <- function(
    diameter_cm,
    Basal_area_weighted_mean_diameter_cm,
    distance_to_coast_km,
    BA_sum_of_trees_with_larger_diameter,
    BA_Birch,
    Basal_area_plot,
    Basal_area_stand,
    computed_tree_age,
    latitude,
    altitude,
    vegetation,
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

  BA_quotient_Birch <- BA_Birch/Basal_area_plot

  rich <- ifelse(vegetation<=9,1,0) #Grass and herb types. No field layer also?

  #TRUE if fertilised plot with shrub vegetation (vegetation<12)
  fertris <- ifelse(fertilised & vegetation<12,1,0)

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      5.9648+
        +1.2217*log(diameter_cm+1)+
        -0.3998*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.9226*log(computed_tree_age+20)+
        +0.4772*standard_tree+
        -0.2090*log(Basal_area_plot+3)+
        -0.5821*sqrt(BA_quotient_Birch)+
        -0.5386*(4835-(57.6*latitude)-(0.9*altitude))+#Odin 1983
        -0.4505*(1/(((4835-(57.6*latitude)-(0.9*altitude)))-0.3))+
        +0.8801*(1/((distance_to_coast_km/10))+3)
      +0.3439*rich+
        +0.3844*fertris+
        +0.1814*thinned_recently+
        +0.2258*edge_effect+
        +0.1321*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}

#' @title Individual tree basal area growth for Noble tree species from Elfving 2004
#' @details R^2 = 0.639. The noble tree species are Ash \emph{Fraxinus excelsior}, Elm \emph{Ulmus glabra}, Linden \emph{Tilia cordata} , Maple \emph{Acer platanoides}, Hornbeam \emph{Carpinus betulus} and Wild Cherry \emph{Prunus avium}.
#' @export
#' @rdname ElfvingIBM

Elfving_2004_BA_increment_Noble <- function(
    diameter_cm,
    BA_sum_of_trees_with_larger_diameter,
    Basal_area_plot,
    Basal_area_stand,
    altitude,
    gotland=FALSE,
    vegetation,
    thinned,
    last_thinned

){
  herb <- ifelse(vegetation<7,1,0) #herb types.

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      2.3316+
        +0.8250*log(diameter_cm+1)+
        -0.2877*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.4010*log(Basal_area_plot+3)+
        -0.3809*gotland+
        +0.9397*herb+
        +0.2410*thinned_recently+
        +0.4676*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}

#' @title Individual tree basal area growth for Oak from Elfving 2004
#' @details R^2 = 0.729
#' @export
#' @rdname ElfvingIBM

Elfving_2004_BA_increment_Oak <- function(
    diameter_cm,
    BA_sum_of_trees_with_larger_diameter,
    BA_Oak,
    Basal_area_plot,
    Basal_area_stand,
    altitude,
    gotland=FALSE,
    vegetation,
    thinned,
    last_thinned,
    edge_effect

){

  BA_quotient_Oak <- BA_Oak/Basal_area_plot

  rich <- ifelse(vegetation<=9,1,0) #Grass and herb types. No field layer also?

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      1.9047+
        +1.3115*log(diameter_cm+1)+
        -0.2640*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        -0.5056*log(Basal_area_plot+3)+
        -0.6001*sqrt(BA_quotient_Oak)+
        -0.4615*gotland+
        +0.3833*(altitude/100)+
        -0.1938*((altitude/100)^2)+
        +0.2635*rich+
        +0.1034*thinned_recently+
        +0.3551*edge_effect+
        +0.1897*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}

#' @title Individual tree basal area growth for Pine from Elfving 2003
#' @details R^2 = 0.758
#' @export
#' @rdname ElfvingIBM
Elfving_2004_BA_increment_Pine <- function(
    diameter_cm,
    Basal_area_weighted_mean_diameter_cm,
    BA_sum_of_trees_with_larger_diameter,
    BA_Spruce,
    BA_Pine,
    Basal_area_plot,
    Basal_area_stand,
    computed_tree_age,
    latitude,
    gotland=FALSE,
    altitude,
    SIS,
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

  BA_quotient_Pine <- BA_Pine/Basal_area_plot

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
        +1.0890*(SIS/10)+
        -0.2164*((SIS^2)/100)+
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

#' @title Individual tree basal area growth for Spruce from Elfving 2004
#' @details R^2 = 0.753
#' @export
#' @rdname ElfvingIBM

Elfving_2004_BA_increment_Spruce <- function(
    diameter_cm,
    Basal_area_weighted_mean_diameter_cm,
    SS_diam,
    stems,
    BA_sum_of_trees_with_larger_diameter,
    BA_Spruce,
    BA_Pine,
    Basal_area_plot,
    Basal_area_stand,
    computed_tree_age,
    latitude,
    gotland=FALSE,
    altitude,
    SIS,
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

  BA_quotient_Spruce <- BA_Spruce/Basal_area_plot

  rich <- ifelse(vegetation<=9,1,0) #Grass and herb types. No field layer also?

  #TRUE if fertilised plot with shrub vegetation (vegetation<12)
  fertris <- ifelse(fertilised & vegetation<12,1,0)

  thinned_recently <-  ifelse(thinned == TRUE &
                                last_thinned <= 10,
                              1, 0)

  return(
    exp(
      3.4360+
        +1.5163*log(diameter_cm+1)+
        -0.1520*(diameter_cm/10)+
        -0.4024*(BA_sum_of_trees_with_larger_diameter/(diameter_cm+1))+
        +0.1625*(Basal_area_weighted_mean_diameter_cm/10)*(1-BA_quotient_Spruce)+
        +0.4702*(Basal_area_weighted_mean_diameter_cm/10)*(((Basal_area_weighted_mean_diameter_cm-sqrt(SS_diam/stems))/Basal_area_weighted_mean_diameter_cm)^3)  + #dif3bal
        -0.7789*log(computed_tree_age+20)+
        +0.4034*standard_tree+
        +0.1914*((Basal_area_weighted_mean_diameter_cm^2)/1000)+
        -0.2342*log(Basal_area_plot+3)+
        +0.1754*((1-BA_quotient_Spruce)^2)+
        -0.3264*gotland+
        -0.6923*(4835-(57.6*latitude)-(0.9*altitude))+#Odin 1983
        +0.2568*((4835-(57.6*latitude)-(0.9*altitude))^2)+ #Odin 1983
        +0.2903*(SIS/10)+
        +0.1965*rich+
        +0.4034*fertris+
        +0.1309*thinned_recently+
        +0.0561*divided_plot+
        +0.1126*edge_effect+
        +0.0770*log((Basal_area_plot+1)/(Basal_area_stand+1))
    )/10000 #cm^2 to m^2

  )

}

#' @title Individual tree basal area growth for Trivial tree species from Elfving 2004
#' @details R^2 = 0.569. The trivial tree species include mostly: Alder \emph{Alnus}, Rowan \emph{Sorbus aucuparia} and Willow \emph{Salix spp.}
#' @export
#' @rdname ElfvingIBM

Elfving_2004_BA_increment_Trivial <- function(
    diameter_cm,
    BA_sum_of_trees_with_larger_diameter,
    Basal_area_plot,
    computed_tree_age,
    vegetation,
    SIS,
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
        +0.2595*(SIS/10)+
        +0.4392*herb+
        +0.1561*thinned_recently
    )/10000 #cm^2 to m^2

  )

}

