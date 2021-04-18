#' Double bark thickness for individual trees
#'
#' @description Detailed in Soderberg (1992) - Functions for forest management: Height, form height and bark thickness of individual trees. Dept. of Forest Survey. Report 52. ISSN 0348-0496. 87 pages. Swedish.
#' N.B. Although the original functions detailed a variable for divided plots, such as when a plot consists '..of different land classes,
#' age classes, site index classes, density classes and cutting classes or belongs to different owners. It is included in the function for documentational purposes, but the dummy variable is set to 0.
#'
#' @param standData A standData object.
#'
#' @return 2*bark thickness, mm.
#' @export
#'
#' @examples
double_bark_thickness_Soderberg_1992 <- function(standData){
  if(!class(standData)%in%c("standData")){
    stop("Input standData is not a standData object. Please make sure it is the correct class.")
  }

  #All SI are compared as Pine sites.
  si_pine <- forester::SIS_estimate(species = "Pinus sylvestris",
                                    vegetation = standData$siteData$vegetation,
                                    ground_layer = standData$siteData$ground_layer,
                                    latitude = standData$siteData$latitude,
                                    longitude = standData$siteData$longitude,
                                    altitude = standData$siteData$altitude,
                                    aspect_main = standData$siteData$aspect_main,
                                    incline_percent = standData$siteData$incline_percent,
                                    soil_moisture = standData$siteData$soil_moisture,
                                    soil_texture = standData$siteData$soil_texture,
                                    soil_depth = standData$siteData$soil_depth,
                                    lateral_water = standData$siteData$lateral_water,
                                    ditched = standData$siteData$ditched,
                                    climate_code = standData$siteData$climate_code,
                                    county = standData$siteData$county
  )

  #Dummy variable, less than 50km to coast?
  close_to_coast <- ifelse(standData$siteData$distance_to_coast <= 50,1,0)

  #Total age of the stand
  stand_age <- standData$standSummary$Main_cohort_age

  #Diameter over bark
  diameter_over_bark_f <- 1/((standData$treelistData$diameter.cm*10)+50)

  #Diameter of largest tree on plot
  diameter_largest <- standData$treelistData %>% summarise(max(diameter.cm))

  #Diameter relative to largest tree on plot
  diameter_quotient <- standData$treelistData$diameter.cm / diameter_largest

  #Basal area Scots Pine on plot
  basal_area_pine <- standData$treelistData %>% filter(species == "Pinus sylvestris") %>% summarise(sum(basal_area))

  #Basal area Norway Spruce on plot
  basal_area_spruce <- standData$treelistData %>% filter(species== "Picea abies") %>% summarise(sum(basal_area))

  #Basal area Birch on plot
  basal_area_betula <- standData$treelistData %>% filter(str_detect(species, "^Betula")) %>% summarise(sum(basal_area))

  #Total basal area on plot
  basal_area_total <- standData$treelistData %>% summarise(sum(basal_area))

  #Basal area quotients (vs. total)
  ba_quotient_pine <- basal_area_pine / basal_area_total

  ba_quotient_spruce <- basal_area_spruce / basal_area_total

  ba_quotient_betula <- basal_area_betula / basal_area_total


  #county areas
  south_eastern_county <- ifelse(standData$siteData$county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)


  southern_region_5 <- ifelse(standData$siteData$county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  northern_sweden <- ifelse(standData$siteData$county %in%  c("Norrbottens lappmark",
                                                              "Norrbottens kustland",
                                                              "Västerbottens lappmark",
                                                              "Västerbottens kustland",
                                                              "Västernorrland - Ångermanlands landskap",
                                                              "Västernorrland - Medelpads landskap",
                                                              "Jämtland - Jämtlands landskap",
                                                              "Jämtland - Härjedalens landskap",
                                                              "Kopparberg - Sälen-Idre"), 1,0)

  middle_sweden <- ifelse(standData$siteData$county %in% c("Kopparberg - övriga",
                                                           "Gävleborg - Hälsinglands landskap",
                                                           "Gävleborg - övriga",
                                                           "Kopparberg - övriga",
                                                           "Värmland"),1,0)

  southern_sweden <- ifelse(northern_sweden == 0 && middle_sweden == 0,1,0)

  latitude <- standData$siteData$latitude
  altitude <- standData$siteData$altitude

  divided_plot <- 0

  if(standData$treelistData$species=="Pinus sylvestris" && northern_sweden==1){
    return(
      exp(
        -0.40225E3*diameter_over_bark_f+
        +0.15037E5*(diameter_over_bark_f^2)+
        +0.44577E-3*stand_age+
        -0.15147E-3*si_pine+
        -0.13581E-1*latitude+
        -0.16395E-5*latitude*altitude+
        +0.88075E-1*diameter_quotient+
        -0.11552E-1*(diameter_quotient^2)+
        -0.69739E-1*ba_quotient_pine+
        -0.82879E-1*close_to_coast+
        +0.53324E1+
        +0.02691 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Pinus sylvestris" && middle_sweden==1){
    return(
      exp(
        -0.39422E3*diameter_over_bark_f+
        +0.14040E5*(diameter_over_bark_f^2)+
        +0.30388E-3*stand_age+
        -0.92527E-3*si_pine+
        -0.64192E-1*latitude+
        -0.31573E-3*altitude+
        +0.12632E0*diameter_quotient+
        -0.46079E-1*(diameter_quotient^2)+
        +0.58621E-1*ba_quotient_spruce+
        -0.94391E-1*close_to_coast+
        +0.86428E1+
        +0.02622 #correction for logarithmic bias, appendix 5. OBSERVE error in appendix!!
      )
    )
  }

  else if(standData$treelistData$species=="Pinus sylvestris" && southern_sweden==1){
    return(
      exp(
        -0.38360E3*diameter_over_bark_f+
        +0.13442E5*(diameter_over_bark_f^2)+
        +0.20965E-2*stand_age+
        -0.88795E-5*(stand_age^2)+
        -0.74698E-3*si_pine+
        +0.10185E-1*altitude+
        -0.17023E-3*latitude*altitude+
        -0.24281E-1*ba_quotient_pine+
        -0.49230E-1*ba_quotient_betula+
        +0.57067E-1*south_eastern_county+
        +0.24619E-1*divided_plot+
        +0.19141E0*close_to_coast+
        +0.47240E1+
        +0.02832 #correction for logarithmic bias, appendix 5.

      )
    )
  }

  else if(standData$treelistData$species=="Picea abies" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.23633E3*diameter_over_bark_f+
        +0.78784E4*(diameter_over_bark_f^2)+
        +0.13589E-2*stand_age+
        +0.62227E-5*(stand_age^2)+
        -0.15491E-2*si_pine+
        +0.28379E-1*latitude+
        +0.35929E0*diameter_quotient+
        +0.57123E-1*ba_quotient_pine+
        +0.20245E-1*divided_plot+
        -0.71409E-1*close_to_coast+
        +0.16604E1+
        +0.02808 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Picea abies" && southern_sweden==1){
    return(
      exp(
        -0.30355E3*diameter_over_bark_f+
        +0.13763E5*(diameter_over_bark_f^2)
        +0.23539E-4*(stand_age^2)+
        -0.13014E-2*si_pine+
        -0.10863E-1*altitude+
        +0.19027E-3*latitude*altitude+
        +0.30230E0*diameter_quotient+
        +0.68055E-1*ba_quotient_pine+
        -0.10406E0*ba_quotient_spruce+
        +0.62182E-1*ba_quotient_betula+
        +0.27539E-1*divided_plot+
        +0.23053E0*close_to_coast+
        +0.36138E1+
        +0.03001 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.37131E3*diameter_over_bark_f+
        +0.13012E5*(diameter_over_bark_f^2)+
        +0.19655E-2*stand_age+
        -0.71109E-3*si_pine+
        +0.86881E-2*latitude+
        +0.62991E-5*latitude*altitude+
        +0.17146E0*diameter_quotient+
        +0.18594E0*ba_quotient_pine+
        +0.31740E1+
        +0.04292 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && southern_sweden==1){
    return(
      exp(
        -0.64799E3*diameter_over_bark_f+
        +0.33167E5*(diameter_over_bark_f^2)+
        +0.14517E-2*stand_age+
        -0.50779E-3*si_pine+
        +0.54445E-2*altitude+
        -0.99383E-4*latitude*altitude+
        +0.13804E0*diameter_quotient+
        +0.88745E-1*ba_quotient_pine+
        -0.14772E0*ba_quotient_betula+
        -0.51335E-1*south_eastern_county+
        +0.50104E1+
        +0.04440 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Fagus sylvatica"){
    return(
      exp(
        -0.17387E3*diameter_over_bark_f+
        +0.22597E-2*stand_age+
        +0.16350E0*diameter_quotient+
        -0.26953E0*southern_region_5+
        +0.24822E1+
        +0.03251 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Quercus")){
    return(
      exp(
        -0.29605E3*diameter_over_bark_f+
        +0.87235E4*(diameter_over_bark_f^2)+
        +0.22680E-2*stand_age+
        -0.24349E0*south_eastern_county+
        +0.44474E-1*divided_plot+
        +0.39521E1+
        +0.02354 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.17562E3*diameter_over_bark_f+
        +0.49609E-2*stand_age+
        +0.26968E0*diameter_quotient+
        +0.29703E0*ba_quotient_betula+
        -0.77013E-1*divided_plot+
        +0.86920E-1*close_to_coast+
        +0.28446E1+
        +0.075272 #Baskerville 1972, funktion was not include in appendix 5.
      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && southern_sweden==1){
    return(
      exp(
        -0.34144E3*diameter_over_bark_f+
        +0.97900E4*(diameter_over_bark_f^2)+
        +0.31101E-2*stand_age+
        -0.22562E-4*(stand_age^2)+
        -0.21013E-2*si_pine+
        +0.45835E1+
        +0.044402 #Baskerville 1972, funktion was not include in appendix 5.
      )
    )
  }









}
