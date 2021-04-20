#' Height functions for individual trees
#' @description Detailed in Soderberg (1992) - Functions for forest management: Height, form height and bark thickness of individual trees. Dept. of Forest Survey. Report 52. ISSN 0348-0496. 87 pages. Swedish.
#' N.B. Although the original functions detailed a variable for divided plots, such as when a plot consists '..of different land classes,
#' age classes, site index classes, density classes and cutting classes or belongs to different owners. It is included in the function for documentational purposes, but the dummy variable is set to 0.
#'
#' @param Input A standData object.
#'
#' @return Individual tree height, decimetres.
#' @export
#'
#' @examples
height.individual.trees.Soderberg.1992 <- function(standData){
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

  ########

  #Pine Northern Sweden height
  if(standData$treelistData$species=="Pinus sylvestris" && northern_sweden==1){
    return(
      exp(
      -0.28390E3*diameter_over_bark_f+
      +0.64168E4*(diameter_over_bark_f^2)+
      +0.63874E-2*stand_age+
      -0.30707E-4*(stand_age^2)+
      +0.12774E-2*si_pine+
      -0.15597E-1*latitude+
      -0.48527E-5*latitude*altitude+
      -0.44962E0*diameter_quotient+
      +0.70355E-1*(diameter_quotient^2)+
      +0.87350E-1*(ba_quotient_pine)+
      -0.56157E-1*divided_plot+
      -0.72392E-1*close_to_coast+
      +0.68125E1+
      +0.01155 #correction for logarithmic bias, appendix 5.
    )
    )
  }
  #Pine Middle Sweden height
  else if(standData$treelistData$species=="Pinus sylvestris" && middle_sweden==1){
    return(
      exp(
      -0.29249E3*diameter_over_bark_f+
      +0.61832E4*(diameter_over_bark_f^2)+
      +0.52675E-2*stand_age+
      -0.25358E-4*(stand_age^2)+
      +0.13721E-2*si_pine+
      +0.69771E-1*latitude+
      +0.58106E-2*altitude+
      -0.10018E-3*latitude*altitude+
      -0.61165E0*diameter_quotient+
      +0.13132E0*(diameter_quotient^2)+
      -0.22217E0*basal_area_pine+
      +0.24504E0*basal_area_spruce+
      +0.23251E0*basal_area_betula+
      -0.55749E-1*divided_plot+
      -0.10186E0*close_to_coast+
      +0.15712E1+
      +0.00938 #correction for logarithmic bias, appendix 5.
    )
    )
  }

  #Pine southern sweden height
  else if(standData$treelistData$species=="Pinus sylvestris" && southern_sweden==1){
    return(
      exp(
      -0.30345E3*diameter_over_bark_f+
      +0.88427E4*(diameter_over_bark_f^2)+
      +0.68724E-2*stand_age+
      -0.38585E-4*(stand_age^2)+
      +0.16646E-2*si_pine+
      -0.47335E-2*altitude+
      +0.82679E-4*latitude*altitude+
      +0.91429E-1*diameter_quotient+
      -0.28115E0*(diameter_quotient^2)+
      +0.20570E0*ba_quotient_pine+
      +0.29485E0*ba_quotient_spruce+
      +0.13909E0*ba_quotient_betula+
      +0.36444E-1*southern_sweden+
      -0.60312E-1*divided_plot+
      -0.19855E0*close_to_coast+
      +0.52706E1+
      +0.01264 #correction for logarithmic bias, appendix 5.
      )

    )
  }

  #Spruce northern Sweden height
  else if(standData$treelistData$species=="Picea abies" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
      -0.28663E3*diameter_over_bark_f+
      +0.47831E4*(diameter_over_bark_f^2)+
      +0.31669E-2*stand_age+
      -0.16854E-4*(stand_age^2)+
      +0.10855E-2*si_pine+
      -0.99681E-2*latitude+
      +0.51262E-3*altitude+
      -0.12449E-4*latitude*altitude+
      -0.19831E0*diameter_quotient+
      +0.60923E-1*ba_quotient_pine+
      +0.90784E-1*ba_quotient_spruce+
      -0.30688E-1*divided_plot+
      -0.62548E-1*close_to_coast+
      +0.65200E1+
      +0.01095 #correction for logarithmic bias, appendix 5.
    )
    )
  }

  #Spruce southern sweden height
  else if(standData$treelistData$species=="Picea abies" && southern_sweden==1){
    return(
      exp(
      -0.27421E3*diameter_over_bark_f+
      +0.38013E4*(diameter_over_bark_f^2)+
      +0.31094E-2*stand_age+
      -0.20764E-4*(stand_age^2)+
      +0.10161E-2*si_pine+
      +0.15166E-2*altitude+
      -0.25385E-4*latitude*altitude+
      -0.23760E0*diameter_quotient+
      +0.10172E0*ba_quotient_pine+
      +0.24012E0*ba_quotient_spruce+
      +0.68141E-1*ba_quotient_betula+
      -0.47848E-1*divided_plot+
      -0.69386E-1*close_to_coast+
      +0.57495E1+
      +0.01051 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  #Birch northern and middle sweden, height
  else if(str_detect(standData$treelistData$species, "^Betula") && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
      -0.26607E3*diameter_over_bark_f+
      +0.71415E4*(diameter_over_bark_f^2)+
      +0.32789E-2*stand_age+
      -0.22514E-4*(stand_age^2)+
      +0.85255E-3*si_pine+
      -0.18462E-1*latitude+
      -0.72180E-5*latitude*altitude+
      -0.39250E0*diameter_quotient+
      +0.76500E-1*(diameter_quotient^2)+
      -0.74398E-1*ba_quotient_pine+
      -0.22539E-1*ba_quotient_spruce+
      -0.35918E-1*divided_plot+
      +0.72446E1+
      +0.01248 #correction for logarithmic bias, appendix 5.
    )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && southern_sweden==1){
    return(
      exp(
      -0.22552E3*diameter_over_bark_f+
      +0.39171E4*(diameter_over_bark_f^2)+
      +0.17264E-2*stand_age+
      -0.11572E-4*(stand_age^2)+
      +0.89953E-3*si_pine+
      -0.90184E-2*altitude+
      +0.15804E-3*latitude*altitude+
      -0.32296E0*diameter_quotient+
      -0.44799E-1*ba_quotient_pine+
      +0.11728E0*ba_quotient_spruce+
      +0.10104E0*ba_quotient_betula+
      +0.42911E-1*southern_sweden+
      -0.68048E-1*divided_plot+
      +0.57820E1+
      +0.01901 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Fagus sylvatica"){
    return(
      exp(
    -0.14407E3*diameter_over_bark_f+
    +0.72319E-2*stand_age+
    -0.27244E-4*(stand_age^2)+
    -0.57810E-5*latitude*altitude+
    +0.18040E0*ba_quotient_spruce+
    +0.18800E0*southern_sweden+
    -0.18416E0*southern_region_5+
    -0.17410E0*divided_plot+
    +0.52974E1+
    +0.01296 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Quercus")){
    return(
      exp(
      -0.25811E3*diameter_over_bark_f+
      +0.63100E4*(diameter_over_bark_f^2)+
      +0.13039E-2*si_pine+
      -0.41543E-5*latitude*altitude+
      -0.32505E0*diameter_quotient+
      +0.59855E-1*ba_quotient_spruce+
      +0.17355E0*southern_sweden+
      -0.47987E-1*southern_region_5+
      -0.69304E-1*divided_plot+
      +0.57884E1+
      +0.01584 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
      -0.14546E3*diameter_over_bark_f+
      +0.53659E-2*stand_age+
      -0.29042E-4*(stand_age^2)+
      +0.17639E-2*si_pine+
      -0.34200E-1*latitude+
      +0.75841E-1*ba_quotient_spruce+
      -0.82953E-1*divided_plot+
      +0.15566E0*close_to_coast+
      +0.70706E1+
      +((0.191^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && southern_sweden==1){
    return(
      exp(
      -0.22078E3*diameter_over_bark_f+
      +0.53920E4*(diameter_over_bark_f^2)+
      +0.53701E-2*stand_age+
      -0.41932E-4*(stand_age^2)+
      +0.53968E-3*si_pine+
      -0.10758E-1*altitude+
      +0.18781E-3*latitude*altitude+
      -0.17045E0*diameter_quotient+
      -0.17291E0*ba_quotient_pine+
      +0.10783E0*ba_quotient_spruce+
      -0.55868E-1*ba_quotient_betula+
      -0.51870E-1*divided_plot+
      +0.56569E1+
      +((0.195^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
      )
    )
  }













}
