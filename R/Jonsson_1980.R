#' Bark function for Birch between 5-200 yr with diameter over bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 114.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.244
#' Number of trees: 4322
#' Coefficient of multiple correlation: 0.85
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height over bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
Jonsson_1980_bark_diameter_over_bark_Birch_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Birch must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 14664E-4
  } else if(jonson_bonitet==4){
    constant <- 14937E-4
  }else if(jonson_bonitet==5){
    constant <- 15322E-4
  }else if(jonson_bonitet==6){
    constant <- 15573E-4
  }else if(jonson_bonitet>=7){
    constant <- 15934E-4
  }

  diameter_over_bark <- 80760E-7*(diameter_at_breast_height)
  diameter_over_bark_sq <- -87005E-10*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 67800E-7*age_at_breast_height
  tree_rings_breast_height_sq <- -16946E-9*age_at_breast_height^2

  lat <- -68541E-7*latitude
  alt <- 21082E-8*altitude
  lat_x_altitude <- -16573E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Bark function for Scots Pine between 5-200 yr with diameter over bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 106.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.224
#' Number of trees: 20465
#' Coefficient of multiple correlation: 0.84
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
Jonsson_1980_bark_diameter_over_Pine_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Pine must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Pine must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 46046E-4
  } else if(jonson_bonitet==4){
    constant <- 46317E-4
  }else if(jonson_bonitet==5){
    constant <- 46536E-4
  }else if(jonson_bonitet==6){
    constant <- 46692E-4
  }else if(jonson_bonitet>=7){
    constant <- 47063E-4
  }

  diameter_over_bark <- -92845E-7*(diameter_at_breast_height)
  diameter_over_bark_sq <- -10691E-9*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 97292E-8*age_at_breast_height
  tree_rings_breast_height_sq <- -37043E-10*age_at_breast_height^2

  lat <- -46868E-6*latitude
  alt <- -14275E-7*altitude
  lat_x_altitude <- 19847E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Bark function for Norway Spruce between 5-200 yr with diameter over bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 108.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.218
#' Number of trees: 23346
#' Coefficient of multiple correlation: 0.83
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height over bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
Jonsson_1980_bark_diameter_over_bark_Spruce_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Spruce must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Spruce must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- -89452E-6
  } else if(jonson_bonitet==4){
    constant <- -23383E-6
  }else if(jonson_bonitet==5){
    constant <- 62617E-6
  }else if(jonson_bonitet==6){
    constant <- 118083E-5
  }else if(jonson_bonitet>=7){
    constant <- 19254E-5
  }

  diameter_over_bark <- 67482E-7*(diameter_at_breast_height)
  diameter_over_bark_sq <- -75994E-10*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 41776E-7*age_at_breast_height
  tree_rings_breast_height_sq <- -84078E-10*age_at_breast_height^2

  lat <- 22596E-6*latitude
  alt <- 25864E-7*altitude
  lat_x_altitude <- -40797E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Bark function for Birch between 5-200 yr with diameter under bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 114.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.261
#' Number of trees: 4322
#' Coefficient of multiple correlation: 0.83
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
Jonsson_1980_bark_diameter_under_bark_Birch_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Birch must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 16147E-4
  } else if(jonson_bonitet==4){
    constant <- 16380E-4
  }else if(jonson_bonitet==5){
    constant <- 16726E-4
  }else if(jonson_bonitet==6){
    constant <- 16908E-4
  }else if(jonson_bonitet>=7){
    constant <- 17260E-4
  }

  diameter_under_bark <- 83889E-7*(diameter_at_breast_height)
  diameter_under_bark_sq <- -10556E-9*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 76257E-7*age_at_breast_height
  tree_rings_breast_height_sq <- -18290E-9*age_at_breast_height^2

  lat <- -87929E-7*latitude
  alt <- 14709E-8*altitude
  lat_x_altitude <- -72173E-11*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Bark function for Scots Pine between 5-200 yr with diameter under bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 108.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.249
#' Number of trees: 20465
#' Coefficient of multiple correlation: 0.80
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
Jonsson_1980_bark_diameter_under_bark_Pine_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Pine must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Pine must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 51110E-4
  } else if(jonson_bonitet==4){
    constant <- 51260E-4
  }else if(jonson_bonitet==5){
    constant <- 51368E-4
  }else if(jonson_bonitet==6){
    constant <- 51413E-4
  }else if(jonson_bonitet>=7){
    constant <- 51718E-4
  }

  diameter_under_bark <- -99036E-7*(diameter_at_breast_height)
  diameter_under_bark_sq <- -13657E-9*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 20925E-7*age_at_breast_height
  tree_rings_breast_height_sq <- -65979E-10*age_at_breast_height^2

  lat <- -53771E-6*latitude
  alt <- -21410E-7*altitude
  lat_x_altitude <- 31010E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Bark function for Norway Spruce between 5-200 yr with diameter under bark,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 112.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.218
#' Number of trees: 23346
#' Coefficient of multiple correlation: 0.83
#'
#'
#' @param jonson_bonitet 3-7+ are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, Double bark thickness.
#' @export
Jonsson_1980_bark_diameter_under_bark_Spruce_5_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Spruce must be at least 5 years.")
  }

  if(age_at_breast_height>200){
    stop("Spruce must be at most 200 years.")
  }

  if(jonson_bonitet>3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII+")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- -11698E-5
  } else if(jonson_bonitet==4){
    constant <- -58074E-6
  }else if(jonson_bonitet==5){
    constant <- 24840E-6
  }else if(jonson_bonitet==6){
    constant <- 77868E-6
  }else if(jonson_bonitet>=7){
    constant <- 15388E-5
  }

  diameter_under_bark <- 67411E-7*(diameter_at_breast_height)
  diameter_under_bark_sq <- -83399E-10*(diameter_at_breast_height)^2

  tree_rings_breast_height <- 49340E-7*age_at_breast_height
  tree_rings_breast_height_sq <- -10424E-9*age_at_breast_height^2

  lat <- 23718E-6*latitude
  alt <- 24536E-7*altitude
  lat_x_altitude <- -38420E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    lat+
    alt+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Birch between 5-60 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 66.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.6284
#' Number of trees: 1932
#' Coefficient of multiple correlation: 0.66
#'
#'
#' @param jonson_bonitet 1-6 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param diameter_of_largest_tree_on_plot Diameter at breast height under bark (?)
#' in cm of the largest tree on the plot.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Birch_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 5 years.")
  }

  if(age_at_breast_height>60){
    stop("Birch must be at most 60 years.")
  }

  if(jonson_bonitet>6){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VI")
    jonson_bonitet <- 6
  }

  if(jonson_bonitet%in%c(1,2,3,4)){
    constant <- 33052E-4
    diameter_under_bark <- 16514E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -27342E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 33322E-4
    diameter_under_bark <- 15521E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -27252E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 29699E-4
    diameter_under_bark <- 21563E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -53674E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -35016E-6*plot_basal_area_under_bark
    basal_area_sq <- 43131E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -20630E-6
    basal_area_sq <- -74479E-9
  }

  tree_rings_breast_height <- -83998E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 63884E-8*(age_at_breast_height^2)

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq

  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Birch between 51-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 68.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.7274
#' Number of trees: 2091
#' Coefficient of multiple correlation: 0.54
#'
#'
#' @param jonson_bonitet 1-6 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Birch_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 51 years.")
  }

  if(age_at_breast_height>60){
    stop("Birch must be at most 200 years.")
  }

  if(jonson_bonitet>6){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VI")
    jonson_bonitet <- 6
  }

  if(jonson_bonitet%in%c(1,2,3,4)){
    constant <- 20841E-4
    diameter_under_bark <- 13231E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -20870E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 15763E-4
    diameter_under_bark <- 16923E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -29529E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 14897E-4
    diameter_under_bark <- 18975E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -40374E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -20432E-6*plot_basal_area_under_bark
    basal_area_sq <- 66314E-9*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -34342E-6
    basal_area_sq <- 50541E-8
  }

  tree_rings_breast_height <- -26058E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 71890E-9*(age_at_breast_height^2)

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq

  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Scots Pine between 101-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 58.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5754
#' Number of trees: 3802
#' Coefficient of multiple correlation: 0.46
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Pine_101_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<101){
    stop("Pine must be at least 101 years.")
  }

  if(age_at_breast_height>200){
    stop("Pine must be at most 200 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 29307E-4
    diameter_under_bark <- 11050E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -11558E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 32898E-4
    diameter_under_bark <- 10572E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -13283E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 32707E-4
    diameter_under_bark <- 10625E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -14815E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 35397E-4
    diameter_under_bark <- 79090E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -10288E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 32223E-4
    diameter_under_bark <- 83615E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -94219E-10*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -50156E-6*plot_basal_area_under_bark
    basal_area_sq <- 52911E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -46408E-6
    basal_area_sq <- 70015E-8
  }

  tree_rings_breast_height <- -10870E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 24836E-9*(age_at_breast_height^2)

  rel_diameter <- 21686E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -15957E-4*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -30385E-6*latitude
  alt_meters <- -80738E-8*altitude

  Latitude_x_altitude <- 38252E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    rel_diameter+
    rel_diameter_sq+
    lat_values+
    alt_meters+
    Latitude_x_altitude

  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Scots Pine between 5-60 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 54.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.4719
#' Number of trees: 4411
#' Coefficient of multiple correlation: 0.73
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Pine_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Pine must be at least 5 years.")
  }

  if(age_at_breast_height>60){
    stop("Pine must be at most 60 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

      if(jonson_bonitet==3){
      constant <- 41417E-4
      diameter_under_bark <- 17938E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -24344E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==4){
      constant <- 38920E-4
      diameter_under_bark <- 21910E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -37469E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==5){
      constant <- 39610E-4
      diameter_under_bark <- 21936E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -40864E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==6){
      constant <- 38478E-4
      diameter_under_bark <- 23420E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -46701E-9*(diameter_at_breast_height^2)
    } else if(jonson_bonitet==7){
      constant <- 39177E-4
      diameter_under_bark <- 23165E-6*diameter_at_breast_height
      diameter_under_bark_sq <- -49942E-9*(diameter_at_breast_height^2)
    }

    basal_area <- 0
    basal_area_sq <- 0

    if(cutting_period=="a"){
      basal_area <- -65082E-6*plot_basal_area_under_bark
      basal_area_sq <- 95147E-8*(plot_basal_area_under_bark^2)
    }

    #Cutting during second half of five year period, see p. 16, Bengt Hansson
    #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
    #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
    #terrängförhållanden. Eng: "Description of some for logging operations
    #important characteristics of forest stands, trees and terrain in Sweden".
    # Royal College of Forestry. Printed by Esselte AB, Stockholm.
    if(cutting_period=="b"){
      basal_area <- -65895E-6
      basal_area_sq <- 11545E-7
    }

    tree_rings_breast_height <- -85475E-6*age_at_breast_height
    tree_rings_breast_height_sq <- 71672E-8*(age_at_breast_height^2)

    rel_diameter <- 54580E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
    rel_diameter_sq <- -78879E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

    lat_values <- -12677E-6*latitude
    alt_meters <- -62972E-7*altitude

    Latitude_x_altitude <- 94767E-9*altitude*latitude

    normal_growth_mm <- constant+
      diameter_under_bark+
      diameter_under_bark_sq+
      basal_area+
      basal_area_sq+
      tree_rings_breast_height+
      tree_rings_breast_height_sq+
      rel_diameter+
      rel_diameter_sq+
      lat_values+
      alt_meters+
      Latitude_x_altitude

    return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Scots Pine between 51-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 56.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5270
#' Number of trees: 12091
#' Coefficient of multiple correlation: 0.57
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Pine_51_125 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Pine must be at least 51 years.")
  }

  if(age_at_breast_height>125){
    stop("Pine must be at most 125 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 40431E-4
    diameter_under_bark <- 12880E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -15512E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 37042E-4
    diameter_under_bark <- 17231E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -26288E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 37794E-4
    diameter_under_bark <- 16987E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -28357E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 38093E-4
    diameter_under_bark <- 16951E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -30162E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 41873E-4
    diameter_under_bark <- 12481E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -18713E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -44433E-6*plot_basal_area_under_bark
    basal_area_sq <- 48593-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -43693E-6
    basal_area_sq <- 56681E-8
  }

  tree_rings_breast_height <- -30491E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 12405E-8*(age_at_breast_height^2)

  rel_diameter <- 16031E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -13091E-4*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -32152E-6*latitude
  alt_meters <- -51186E-7*altitude

  Latitude_x_altitude <- 72810E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    rel_diameter+
    rel_diameter_sq+
    lat_values+
    alt_meters+
    Latitude_x_altitude

  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Norway Spruce between 101-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 64.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.6249
#' Number of trees: 4503
#' Coefficient of multiple correlation: 0.47
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Spruce_101_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<101){
    stop("Spruce must be at least 101 years.")
  }

  if(age_at_breast_height>200){
    stop("Spruce must be at most 200 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 48264E-4
    diameter_under_bark <- 86958E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -10624E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 49212E-4
    diameter_under_bark <- 67177E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -53210E-10*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 47640E-4
    diameter_under_bark <- 85890E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -11233E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 46837E-4
    diameter_under_bark <- 84802E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -10156E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 45040E-4
    diameter_under_bark <- 87566E-7*diameter_at_breast_height
    diameter_under_bark_sq <- -12045E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -29742E-6*plot_basal_area_under_bark
    basal_area_sq <- 26106E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -15438E-6
    basal_area_sq <- -34275E-9
  }

  tree_rings_breast_height <- -64153E-7*age_at_breast_height
  tree_rings_breast_height_sq <- 36752E-10*(age_at_breast_height^2)

  rel_diameter <- 13709E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -89329E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -53624E-6*latitude
  alt_meters <- -54500E-7*altitude

  Latitude_x_altitude <- 80254E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    rel_diameter+
    rel_diameter_sq+
    lat_values+
    alt_meters+
    Latitude_x_altitude

  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Norway Spruce between 5-60 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 60.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5201
#' Number of trees: 6520
#' Coefficient of multiple correlation: 0.69
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Spruce_5_60 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Spruce must be at least 5 years.")
  }

  if(age_at_breast_height>60){
    stop("Spruce must be at most 60 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 47028E-4
    diameter_under_bark <- 18150E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -29595E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 47328E-4
    diameter_under_bark <- 17774E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -30561E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 45834E-4
    diameter_under_bark <- 19572E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -38271E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 43220E-4
    diameter_under_bark <- 23798E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -55628E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 44156E-4
    diameter_under_bark <- 21805E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -47630E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -33871E-6*plot_basal_area_under_bark
    basal_area_sq <- 37268E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -36137E-6
    basal_area_sq <- 59695E-8
  }

  tree_rings_breast_height <- -70993E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 53700E-8*(age_at_breast_height^2)

  rel_diameter <- 69783E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -81438E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -28051E-6*latitude
  alt_meters <- -96912E-7*altitude

  Latitude_x_altitude <- 15505E-8*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    rel_diameter+
    rel_diameter_sq+
    lat_values+
    alt_meters+
    Latitude_x_altitude

  return(exp(normal_growth_mm))

}
#' Five years diameter increment under bark, mm for Norway Spruce between 51-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 60.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.5678
#' Number of trees: 12196
#' Coefficient of multiple correlation: 0.58
#'
#'
#' @param jonson_bonitet 3-7 are available.
#' @param diameter_at_breast_height Diameter at breast height under bark in cm,
#' @param age_at_breast_height Age at breast height.
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param cutting_period "a", if cutting during first half of five year period;
#' "b", if cutting during second half of five year period.
#' @param plot_basal_area_under_bark Basal area of plot. (under bark).
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_five_year_increment_under_bark_Spruce_51_125 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  age_at_breast_height,
  diameter_of_largest_tree_on_plot,
  cutting_period,
  plot_basal_area_under_bark,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Spruce must be at least 51 years.")
  }

  if(age_at_breast_height>125){
    stop("Spruce must be at most 125 years.")
  }


  if(jonson_bonitet<3){
    message("Jonson bonitet too high, setting to maximum for function, Jonson bonitet III")
    jonson_bonitet <- 3
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet==3){
    constant <- 19384E-4
    diameter_under_bark <- 13907E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -18357E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==4){
    constant <- 21253E-4
    diameter_under_bark <- 12479E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -16357E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==5){
    constant <- 19763E-4
    diameter_under_bark <- 14570E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -23371E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==6){
    constant <- 20111E-4
    diameter_under_bark <- 13878E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -21250E-9*(diameter_at_breast_height^2)
  } else if(jonson_bonitet==7){
    constant <- 19281E-4
    diameter_under_bark <- 13958E-6*diameter_at_breast_height
    diameter_under_bark_sq <- -23284E-9*(diameter_at_breast_height^2)
  }

  basal_area <- 0
  basal_area_sq <- 0

  if(cutting_period=="a"){
    basal_area <- -25085E-6*plot_basal_area_under_bark
    basal_area_sq <- 15953E-8*(plot_basal_area_under_bark^2)
  }

  #Cutting during second half of five year period, see p. 16, Bengt Hansson
  #Ager, Nils-Erik Nilsson, Gustaf v. Segebaden (1964), "Beskrivning av vissa
  #skogstekniskt betydelsefulla bestånds- och träd- egenskaper samt
  #terrängförhållanden. Eng: "Description of some for logging operations
  #important characteristics of forest stands, trees and terrain in Sweden".
  # Royal College of Forestry. Printed by Esselte AB, Stockholm.
  if(cutting_period=="b"){
    basal_area <- -23506E-6
    basal_area_sq <- 21414E-8
  }

  tree_rings_breast_height <- -19469E-6*age_at_breast_height
  tree_rings_breast_height_sq <- 54430E-9*(age_at_breast_height^2)

  rel_diameter <- 10552E-4*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  rel_diameter_sq <- -92733E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat_values <- -49391E-7*latitude
  alt_meters <- -29436E-7*altitude

  Latitude_x_altitude <- 44066E-9*altitude*latitude

  normal_growth_mm <- constant+
    diameter_under_bark+
    diameter_under_bark_sq+
    basal_area+
    basal_area_sq+
    tree_rings_breast_height+
    tree_rings_breast_height_sq+
    rel_diameter+
    rel_diameter_sq+
    lat_values+
    alt_meters+
    Latitude_x_altitude

  return(exp(normal_growth_mm))

}
#' Height function for Birch between 101-126 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 102.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1699
#' Number of trees: 342
#' Coefficient of multiple correlation: 0.88
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_101_126 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<101){
    stop("Birch must be at least 101 years.")
  }

  if(age_at_breast_height>126){
    stop("Birch must be at most 126 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 51562E-4
  } else if(jonson_bonitet==3){
    constant <- 51562E-4
  } else if(jonson_bonitet==4){
    constant <- 51562E-4
  }else if(jonson_bonitet==5){
    constant <- 50260E-4
  }else if(jonson_bonitet==6){
    constant <- 49140E-4
  }else if(jonson_bonitet==7){
    constant <- 48275E-4
  }

  diameter_over_bark <- -20730E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 28772E-2*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 82359E-8*age_at_breast_height

  relative_diameter <- 43881E-6*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -21903E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -21892E-6*latitude
  lat_x_altitude <- -50577E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Birch between 126-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 104.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1546
#' Number of trees: 128
#' Coefficient of multiple correlation: 0.87
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_126_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<126){
    stop("Birch must be at least 126 years.")
  }

  if(age_at_breast_height>200){
    stop("Birch must be at most 200 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2,3,4,5)){
    constant <- 52791E-4
  }else if(jonson_bonitet==6){
    constant <- 51199E-4
  }else if(jonson_bonitet==7){
    constant <- 50749E-4
  }

  diameter_over_bark <- -30247E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 11285E-00*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -10013E-7*age_at_breast_height

  relative_diameter <- 48226E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -45534E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -20567E-6*latitude
  lat_x_altitude <- -90661E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Birch between 26-50 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 96.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1562
#' Number of trees: 1157
#' Coefficient of multiple correlation: 0.91
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_26_50 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<26){
    stop("Birch must be at least 26 years.")
  }

  if(age_at_breast_height>50){
    stop("Birch must be at most 50 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 45719E-4
  } else if(jonson_bonitet==3){
    constant <- 45031E-4
  } else if(jonson_bonitet==4){
    constant <- 44290E-4
  }else if(jonson_bonitet==5){
    constant <- 43507E-4
  }else if(jonson_bonitet==6){
    constant <- 42555E-4
  }else if(jonson_bonitet==7){
    constant <- 41759E-4
  }

  diameter_over_bark <- -20394E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 51818E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 22494E-7*age_at_breast_height

  relative_diameter <- 37086E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -38778E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -16554E-6*latitude
  lat_x_altitude <- -44436E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Birch between 5-25 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 94.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1513
#' Number of trees: 355
#' Coefficient of multiple correlation: 0.88
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_5_25 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Birch must be at least 5 years.")
  }

  if(age_at_breast_height>25){
    stop("Birch must be at most 25 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 41911E-4
  } else if(jonson_bonitet==3){
    constant <- 41911E-4
  } else if(jonson_bonitet==4){
    constant <- 41325E-4
  }else if(jonson_bonitet==5){
    constant <- 40798E-4
  }else if(jonson_bonitet==6){
    constant <- 40686E-4
  }else if(jonson_bonitet==7){
    constant <- 39493E-4
  }

  diameter_over_bark <- -14963E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 12704E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 11073E-6*age_at_breast_height

  relative_diameter <- -19889E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- 24952E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -16138E-6*latitude
  lat_x_altitude <- -59630E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Birch between 51-75 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 98.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1642
#' Number of trees: 960
#' Coefficient of multiple correlation: 0.91
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_51_75 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Birch must be at least 51 years.")
  }

  if(age_at_breast_height>75){
    stop("Birch must be at most 75 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 43441E-4
  } else if(jonson_bonitet==3){
    constant <- 43441E-4
  } else if(jonson_bonitet==4){
    constant <- 43219E-4
  }else if(jonson_bonitet==5){
    constant <- 42372E-4
  }else if(jonson_bonitet==6){
    constant <- 40727E-4
  }else if(jonson_bonitet==7){
    constant <- 39732E-4
  }

  diameter_over_bark <- -14313E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -38309E-2*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 99919E-8*age_at_breast_height

  relative_diameter <- 33084E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -31235E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -15880E-6*latitude
  lat_x_altitude <- -43920E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Birch between 76-100 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 100.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1690
#' Number of trees: 662
#' Coefficient of multiple correlation: 0.89
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Birch_76_100 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<76){
    stop("Birch must be at least 76 years.")
  }

  if(age_at_breast_height>100){
    stop("Birch must be at most 100 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 48179E-4
  } else if(jonson_bonitet==3){
    constant <- 48179E-4
  } else if(jonson_bonitet==4){
    constant <- 48179E-4
  }else if(jonson_bonitet==5){
    constant <- 47233E-4
  }else if(jonson_bonitet==6){
    constant <- 46009E-4
  }else if(jonson_bonitet==7){
    constant <- 45561E-4
  }

  diameter_over_bark <- -20573E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 35093E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 11984E-7*age_at_breast_height

  relative_diameter <- 72347E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -62374E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -22189E-6*latitude
  lat_x_altitude <- -62817E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Scots Pine between 101-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 78.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1155
#' Number of trees: 2124
#' Coefficient of multiple correlation: 0.88
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Pine_101_125 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<101){
    stop("Pine must be at least 101 years.")
  }

  if(age_at_breast_height>125){
    stop("Pine must be at most 125 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 35434E-4
  } else if(jonson_bonitet==3){
    constant <- 34360E-4
  } else if(jonson_bonitet==4){
    constant <- 33347E-4
  }else if(jonson_bonitet==5){
    constant <- 32227E-4
  }else if(jonson_bonitet==6){
    constant <- 30951E-4
  }else if(jonson_bonitet==7){
    constant <- 29378E-4
  }

  diameter_over_bark <- -69599E-3*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -11051E-00*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 19283E-8*age_at_breast_height

  relative_diameter <- 19104E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -16512E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -15551E-7*latitude
  lat_x_altitude <- -26758E-11*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Scots Pine between 126-200 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 80.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1408
#' Number of trees: 1697
#' Coefficient of multiple correlation: 0.83
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Pine_126_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<126){
    stop("Pine must be at least 126 years.")
  }

  if(age_at_breast_height>200){
    stop("Pine must be at most 200 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 41424E-4
  } else if(jonson_bonitet==3){
    constant <- 39993E-4
  } else if(jonson_bonitet==4){
    constant <- 39206E-4
  }else if(jonson_bonitet==5){
    constant <- 38125E-4
  }else if(jonson_bonitet==6){
    constant <- 37043E-4
  }else if(jonson_bonitet==7){
    constant <- 35667E-4
  }

  diameter_over_bark <- -11909E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -10256E-00*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -46200E-8*age_at_breast_height

  relative_diameter <- -13168E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -13990E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -47366E-7*latitude
  lat_x_altitude <- -68762E-11*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Scots Pine between 26-50 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 72.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1436
#' Number of trees: 2120
#' Coefficient of multiple correlation: 0.89
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Pine_26_50 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<26){
    stop("Pine must be at least 26 years.")
  }

  if(age_at_breast_height>50){
    stop("Pine must be at most 50 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 33815E-4
  } else if(jonson_bonitet==3){
    constant <- 33622E-4
  } else if(jonson_bonitet==4){
    constant <- 32741E-4
  }else if(jonson_bonitet==5){
    constant <- 31764E-4
  }else if(jonson_bonitet==6){
    constant <- 30756E-4
  }else if(jonson_bonitet==7){
    constant <- 29309E-4
  }

  diameter_over_bark <- -12779E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -75736E-3*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 63380E-7*age_at_breast_height

  relative_diameter <- 48965E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -38291E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -74220E-7*latitude
  lat_x_altitude <- -12051E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Scots Pine between 5-25 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 70.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1523
#' Number of trees: 676
#' Coefficient of multiple correlation: 0.88
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Pine_5_25 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
  ){
  if(age_at_breast_height<5){
    stop("Pine must be at least 5 years.")
  }

  if(age_at_breast_height>25){
    stop("Pine must be at most 25 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 39611E-4
  } else if(jonson_bonitet==3){
    constant <- 38252E-4
  } else if(jonson_bonitet==4){
    constant <- 37738E-4
  }else if(jonson_bonitet==5){
    constant <- 37207E-4
  }else if(jonson_bonitet==6){
    constant <- 36103E-4
  }else if(jonson_bonitet==7){
    constant <- 35413E-4
  }

  diameter_over_bark <- -12603E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 14574E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 23304E-1*age_at_breast_height

  relative_diameter <- -13878E-6*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -32227E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -21195E-6*latitude
  lat_x_altitude <- -46529E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Scots Pine between 51-75 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 74.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1258
#' Number of trees: 5175
#' Coefficient of multiple correlation: 0.89
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Pine_51_75 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Pine must be at least 51 years.")
  }

  if(age_at_breast_height>75){
    stop("Pine must be at most 75 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 310705E-4
  } else if(jonson_bonitet==3){
    constant <- 30477E-4
  } else if(jonson_bonitet==4){
    constant <- 29551E-4
  }else if(jonson_bonitet==5){
    constant <- 28437E-4
  }else if(jonson_bonitet==6){
    constant <- 27348E-4
  }else if(jonson_bonitet==7){
    constant <- 25517E-4
  }

  diameter_over_bark <- -11941E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -39861E-2*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 24437E-7*age_at_breast_height

  relative_diameter <- 76155E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -50558E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -16312E-7*latitude
  lat_x_altitude <- -22508E-11*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Scots Pine between 76-100 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 76.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1187
#' Number of trees: 4867
#' Coefficient of multiple correlation: 0.87
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Pine_76_100 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<76){
    stop("Pine must be at least 76 years.")
  }

  if(age_at_breast_height>100){
    stop("Pine must be at most 100 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 32046E-4
  } else if(jonson_bonitet==3){
    constant <- 31272E-4
  } else if(jonson_bonitet==4){
    constant <- 30402E-4
  }else if(jonson_bonitet==5){
    constant <- 29226E-4
  }else if(jonson_bonitet==6){
    constant <- 28057E-4
  }else if(jonson_bonitet==7){
    constant <- 26055E-4
  }

  diameter_over_bark <- -82794E-3*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -60979E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 70770E-8*age_at_breast_height

  relative_diameter <- 27226E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -20557E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- 13936E-7*latitude
  lat_x_altitude <- 54961E-11*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Norway Spruce between 101-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 90.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1358
#' Number of trees: 2476
#' Coefficient of multiple correlation: 0.90
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Spruce_101_125 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<101){
    stop("Spruce must be at least 101 years.")
  }

  if(age_at_breast_height>125){
    stop("Spruce must be at most 125 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 41060E-4
  } else if(jonson_bonitet==3){
    constant <- 39907E-4
  } else if(jonson_bonitet==4){
    constant <- 39196E-4
  }else if(jonson_bonitet==5){
    constant <- 38594E-4
  }else if(jonson_bonitet==6){
    constant <- 37762E-4
  }else if(jonson_bonitet==7){
    constant <- 36518E-4
  }

  diameter_over_bark <- -21686E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 26497E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -13147E-9*age_at_breast_height

  relative_diameter <- 49739E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -33318E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -70496E-7*latitude
  lat_x_altitude <- -23113E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Norway Spruce between 101-125 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 92.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1340
#' Number of trees: 2050
#' Coefficient of multiple correlation: 0.89
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Spruce_126_200 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<126){
    stop("Spruce must be at least 126 years.")
  }

  if(age_at_breast_height>200){
    stop("Spruce must be at most 200 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 47511E-4
  } else if(jonson_bonitet==3){
    constant <- 47511E-4
  } else if(jonson_bonitet==4){
    constant <- 46367E-4
  }else if(jonson_bonitet==5){
    constant <- 45815E-4
  }else if(jonson_bonitet==6){
    constant <- 45121E-4
  }else if(jonson_bonitet==7){
    constant <- 44052E-4
  }

  diameter_over_bark <- -25612E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 49560E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -81180E-8*age_at_breast_height

  relative_diameter <- 13814E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -63065E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -13037E-6*latitude
  lat_x_altitude <- -33171E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Norway Spruce between 26-50 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 84.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1451
#' Number of trees: 3993
#' Coefficient of multiple correlation: 0.94
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Spruce_26_50 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<26){
    stop("Spruce must be at least 26 years.")
  }

  if(age_at_breast_height>50){
    stop("Spruce must be at most 50 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 42502E-4
  } else if(jonson_bonitet==3){
    constant <- 41816E-4
  } else if(jonson_bonitet==4){
    constant <- 41327E-4
  }else if(jonson_bonitet==5){
    constant <- 40815E-4
  }else if(jonson_bonitet==6){
    constant <- 40038E-4
  }else if(jonson_bonitet==7){
    constant <- 39496E-4
  }

  diameter_over_bark <- -16634E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -91796E-2*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 29065E-7*age_at_breast_height

  relative_diameter <- 53052E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -38848E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -16215E-6*latitude
  lat_x_altitude <- -23298E-10*altitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Norway Spruce between 5-25 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 82.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1559
#' Number of trees: 781
#' Coefficient of multiple correlation: 0.92
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Spruce_5_25 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<5){
    stop("Spruce must be at least 5 years.")
  }

  if(age_at_breast_height>25){
    stop("Spruce must be at most 25 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 49577E-4
  } else if(jonson_bonitet==3){
    constant <- 49375E-4
  } else if(jonson_bonitet==4){
    constant <- 49196E-4
  }else if(jonson_bonitet==5){
    constant <- 48911E-4
  }else if(jonson_bonitet==6){
    constant <- 48466E-4
  }else if(jonson_bonitet==7){
    constant <- 48184E-4
  }

  diameter_over_bark <- -13570E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -23535E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -64104E-7*age_at_breast_height

  relative_diameter <- -37681E-6*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- 37094E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -29892E-6*latitude
  lat_x_altitude <- -43385E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Norway Spruce between 51-75 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 86.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1319
#' Number of trees: 5521
#' Coefficient of multiple correlation: 0.93
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Spruce_51_75 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<51){
    stop("Spruce must be at least 51 years.")
  }

  if(age_at_breast_height>75){
    stop("Spruce must be at most 75 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 39987E-4
  } else if(jonson_bonitet==3){
    constant <- 39325E-4
  } else if(jonson_bonitet==4){
    constant <- 38739E-4
  }else if(jonson_bonitet==5){
    constant <- 38020E-4
  }else if(jonson_bonitet==6){
    constant <- 37128E-4
  }else if(jonson_bonitet==7){
    constant <- 36030E-4
  }

  diameter_over_bark <- -18896E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- 12645E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- 16768E-7*age_at_breast_height

  relative_diameter <- 67597E-5*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -43574E-5*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -11471E-6*latitude
  lat_x_altitude <- -11785E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Height function for Norway Spruce between 76-100 yr,
#' from Jonsson (1980)
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 88.
#'
#' @description
#' Written to be used with the Swedish National Forest Inventory's 6.64 m radius
#' plots.
#'
#' @details
#' This function originally for ln( increment). Result has been re-transformed.
#' Log. bias assumed to be included in written coefficient.
#'
#' Standard Deviation: 0.1320
#' Number of trees: 4460
#' Coefficient of multiple correlation: 0.92
#'
#'
#' @param jonson_bonitet 1-7 are available.
#' @param diameter_at_breast_height Diameter at breast height above bark in cm
#' @param diameter_of_largest_tree_on_plot Diameter of thickest tree on plot.
#' @param age_at_breast_height Age at breast height.
#' @param latitude Latitude, degrees.
#' @param altitude Meters above sea level.
#'
#' @return mm, diameter increment during five years.
#' @export
Jonsson_1980_height_Spruce_76_100 <- function(
  jonson_bonitet,
  diameter_at_breast_height,
  diameter_of_largest_tree_on_plot,
  age_at_breast_height,
  latitude,
  altitude
){
  if(age_at_breast_height<76){
    stop("Pine must be at least 76 years.")
  }

  if(age_at_breast_height>100){
    stop("Pine must be at most 100 years.")
  }

  if(jonson_bonitet>7){
    message("Jonson bonitet too low, setting to minimum for function, Jonson bonitet VII")
    jonson_bonitet <- 7
  }

  if(jonson_bonitet%in%c(1,2)){
    constant <- 42332E-4
  } else if(jonson_bonitet==3){
    constant <- 41658E-4
  } else if(jonson_bonitet==4){
    constant <- 41054E-4
  }else if(jonson_bonitet==5){
    constant <- 40181E-4
  }else if(jonson_bonitet==6){
    constant <- 39311E-4
  }else if(jonson_bonitet==7){
    constant <- 38238E-4
  }

  diameter_over_bark <- -17196E-2*(diameter_at_breast_height+50)^-1
  diameter_over_bark_sq <- -20718E-1*(diameter_at_breast_height)^-2

  tree_rings_breast_height <- -38613E-8*age_at_breast_height

  relative_diameter <- 34539E-6*(diameter_at_breast_height/diameter_of_largest_tree_on_plot)
  relative_diameter_sq <- -13414E-6*((diameter_at_breast_height/diameter_of_largest_tree_on_plot)^2)

  lat <- -86201E-7*latitude
  lat_x_altitude <- -13625E-10*altitude*latitude

  normal_growth_mm <- constant+
    diameter_over_bark+
    diameter_over_bark_sq+
    tree_rings_breast_height+
    relative_diameter+
    relative_diameter_sq+
    lat+
    lat_x_altitude


  return(exp(normal_growth_mm))

}
#' Jonsson (1980) self thinning function
#'
#' @source Jonsson, B. (1980) "Functions for the long-term forecasting of the
#' size and structure of timber yields". Report #7. Section of Forest Mensuration
#' and Management. Swedish University of Agricultural Sciences.
#' ISSN: 0349-2133. ISBN: 91-576-0477-0. Umeå. p. 120.
#'
#' @details
#' Standard deviation = 30.3
#' Multiple correlation coefficient = 0.60
#' Material: Untouched parcels on permanent trial sites, 127 revisions.
#'
#' @param stem_number_per_ha Number of stems per hectare.
#' @param jonson_bonitet numeric. Jonson site classes 1-7.
#' @param annual_basal_area_increment_percent  Annual basal area increment, in percent.
#'
#' @return Self thinned volume in percent of the annual volume increment.
#' @export
#'
#' @examples
#'self_thinning_df <- data.frame("Bonitet"=c(1,1,1,1,1,
# 5,5,5,5,5,
# 6,6,6,6,6),
# "Stems per ha"=c(500,1000,2000,4000,6000,
#                  500,1000,2000,4000,6000,
#                  500,1000,2000,4000,6000)
# )
#
# self_thinning_df2 <- self_thinning_df %>% rowwise() %>%  mutate(percent_self_thinned = Jonsson_1980_self_thinning(stem_number_per_ha = Stems.per.ha,
#                                                                                                                   jonson_bonitet = Bonitet,
#                                                                                                                   annual_basal_area_increment_percent = 1.23))
#
# self_thinning_df2 %>% ggplot(aes(x=Stems.per.ha,y=percent_self_thinned))+geom_line(aes(group=Bonitet))+geom_label(aes(label=Bonitet))
#'
#'
#'
#'
#'
Jonsson_1980_self_thinning <- function(
  stem_number_per_ha,
  jonson_bonitet,
  annual_basal_area_increment_percent
){
  if(!(is.numeric(jonson_bonitet))){
    stop("Jonson bonitet must be numeric.")
  }

  if(!is.numeric(stem_number_per_ha)){
    stop("Stems per ha must be numeric.")
  }

  if(!is.numeric(annual_basal_area_increment_percent)){
    stop("Annual basal area increment in percent must be numeric.")
  }


  if(jonson_bonitet%in%c(1,2,3,4)){
    a <- 11975E-6*stem_number_per_ha
  } else if(jonson_bonitet==5){
    a <- 90206E-7*stem_number_per_ha
  } else if(jonson_bonitet>=6){
    a <- 72101E-7*stem_number_per_ha
  }

  constant <- 88985E-3

  growth_factor <- -49519E-3*annual_basal_area_increment_percent

  self_thinning_percent <- constant + growth_factor + a

  return(self_thinning_percent)


}



