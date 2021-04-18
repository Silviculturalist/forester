#' Form Height functions for individual trees
#'
#' @description Detailed in Soderberg (1992) - Functions for forest management: Height, form height and bark thickness of individual trees. Dept. of Forest Survey. Report 52. ISSN 0348-0496. 87 pages. Swedish.
#' N.B. Although the original functions detailed a variable for divided plots, such as when a plot consists '..of different land classes,
#' age classes, site index classes, density classes and cutting classes or belongs to different owners. It is included in the function for documentational purposes, but the dummy variable is set to 0.
#'
#' @param standData A standData object.
#'
#' @return form height, metres.
#' @export
#'
#' @examples
tree_form_Soderberg_1992 <- function(standData){
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
        -0.24776E3*diameter_over_bark_f+
        +0.75785E4*(diameter_over_bark_f^2)+
        +0.52773E-2*stand_age+
        -0.24395E-4*(stand_age^2)+
        +0.10773E-2*si_pine+
        -0.15516E-1*latitude+
        -0.43763E-5*latitude*altitude+
        -0.36728E0*diameter_quotient+
        +0.56762E-1*(diameter_quotient^2)
        +0.74321E-1*ba_quotient_pine+
        -0.52502E-1*divided_plot+
        -0.59471E-1*close_to_coast+
        +0.36491E1+
        +0.00832 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Pinus sylvestris" && middle_sweden==1){
    return(
      exp(
        -0.25627E3*diameter_over_bark_f+
        +0.77018E4*(diameter_over_bark_f^2)+
        +0.47410E-2*stand_age+
        -0.21793E-4*(stand_age^2)+
        +0.13003E-2*si_pine+
        +0.81039E-1*latitude+
        +0.75022E-2*altitude+
        -0.12694E-3*latitude*altitude+
        -0.50469E0*diameter_quotient+
        +0.10610E0*(diameter_quotient^2)+
        +0.15691E0*ba_quotient_pine+
        +0.17465E0*ba_quotient_spruce+
        +0.17342E0*ba_quotient_betula+
        -0.48782E-1*divided_plot+
        -0.83240E-1*close_to_coast+
        -0.23076E1+
        +0.00756 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Pinus sylvestris" && southern_sweden==1){
    return(
      exp(
        -0.24722E3*diameter_over_bark_f+
        +0.96476E4*(diameter_over_bark_f^2)+
        +0.64050E-2*stand_age+
        -0.33916E-4*(stand_age^2)+
        +0.16113E-2*si_pine+
        -0.57111E-2*altitude+
        +0.98668E-4*latitude*altitude+
        +0.17639E0*diameter_quotient+
        -0.30930E0*(diameter_quotient^2)+
        +0.18507E0*ba_quotient_pine+
        +0.27249E0*ba_quotient_spruce+
        +0.12120E0*ba_quotient_betula+
        +0.21324E-1*south_eastern_county+
        -0.62357E-1*divided_plot+
        -0.19831E0*close_to_coast+
        +0.19624E1+
        +0.01080 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Picea abies" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.21522E3*diameter_over_bark_f+
        +0.32488E4*(diameter_over_bark_f^2)+
        +0.40044E-2*(stand_age)+
        -0.20320E-4*(stand_age^2)+
        +0.11681E-2*si_pine+
        -0.11238E-1*latitude+
        +0.57508E-3*altitude+
        -0.14149E-4*latitude*altitude+
        -0.21199E0*diameter_quotient+
        +0.58171E-1*ba_quotient_pine+
        +0.10093E0*ba_quotient_spruce+
        -0.35409E-1*divided_plot+
        -0.66759E-1*close_to_coast+
        +0.32511E1+
        +0.01140 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Picea abies" && southern_sweden==1){
    return(
      exp(
        -0.20201E3*diameter_over_bark_f+
        +0.16550E4*(diameter_over_bark_f^2)+
        +0.39114E-2*(stand_age)+
        -0.24311E-4*(stand_age^2)+
        +0.10805E-2*si_pine+
        +0.15779E-2*altitude+
        -0.26825E-4*latitude*altitude+
        -0.25400E0*diameter_quotient+
        +0.10089E0*ba_quotient_pine+
        +0.27052E0*ba_quotient_spruce+
        +0.64203E-1*ba_quotient_betula+
        -0.53712E-1*divided_plot+
        -0.79867E-1*close_to_coast+
        +0.23991E1+
        +0.01156 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.23369E3*diameter_over_bark_f+
        +0.66940E4*(diameter_over_bark_f^2)+
        +0.32512E-2*stand_age+
        -0.22072E-4*(stand_age^2)+
        +0.86739E-3*si_pine+
        -0.20822E-1*latitude+
        -0.74028E-5*latitude*altitude+
        -0.38620E0*diameter_quotient+
        +0.70742E-1*(diameter_quotient^2)+
        -0.82056E-1*ba_quotient_pine+
        -0.21952E-1*ba_quotient_spruce+
        -0.39112E-1*divided_plot+
        +0.41540E1+
        +0.01232 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && southern_sweden==1){
    return(
      exp(
        -0.10414E3*diameter_over_bark_f+
        -0.26202E4*(diameter_over_bark_f^2)+
        +0.15775E-2*stand_age+
        -0.10844E-4*(stand_age^2)+
        +0.94915E-3*si_pine+
        -0.10506E-1*altitude+
        +0.18430E-3*latitude*altitude+
        -0.32432E0*diameter_quotient+
        -0.49356E-1*ba_quotient_pine+
        +0.12381E0*ba_quotient_spruce+
        +0.11830E0*ba_quotient_betula+
        +0.45225E-1*south_eastern_county+
        -0.68294E-1*divided_plot+
        +0.22298E1+
        +0.02000 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(standData$treelistData$species=="Fagus sylvatica"){
    return(
      exp(
        -0.10532E3*diameter_over_bark_f+
        +0.65517E-2*stand_age+
        -0.16776E-4*(stand_age^2)+
        -0.52081E-3*si_pine+
        -0.42320E-5*latitude*altitude+
        +0.14651E0*diameter_quotient+
        +0.20009E0*ba_quotient_spruce+
        +0.19265E0*south_eastern_county+
        -0.16720E0*southern_region_5+
        -0.17627E0*divided_plot+
        +0.20763E1+
        +0.01638 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Quercus")){
    return(
      exp(
        -0.24454E3*diameter_over_bark_f+
        +0.77370E4*(diameter_over_bark_f^2)+
        +0.25633E-2*stand_age+
        -0.16976E-4*(stand_age^2)+
        +0.13153E-2*si_pine+
        -0.56851E-5*latitude*altitude+
        -0.29397E0*diameter_quotient+
        +0.82213E-1*ba_quotient_spruce+
        +0.26924E0*south_eastern_county+
        -0.65403E-2*southern_region_5+
        -0.77845E-1*divided_plot+
        +0.25409E1+
        +0.01960 #correction for logarithmic bias, appendix 5.

      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.12623E3*diameter_over_bark_f+
        +0.42804E-2*stand_age+
        -0.25316E-4*(stand_age^2)+
        +0.16703E-2*si_pine+
        -0.32185E-1*latitude+
        -0.27431E-4*altitude+
        -0.85686E-1*diameter_quotient+
        +0.68224E-1*ba_quotient_spruce+
        -0.51989E-1*ba_quotient_betula+
        -0.77704E-1*divided_plot+
        +0.13794E0*close_to_coast+
        +0.39069E1+
        +((0.188^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && southern_sweden==1){
    return(
      exp(
        -0.15868E3*diameter_over_bark_f+
        +0.30541E4*(diameter_over_bark_f^2)+
        +0.45148E-2*stand_age+
        -0.34685E-4*(stand_age^2)+
        +0.83659E-3*si_pine+
        -0.11257E-1*altitude+
        +0.19625E-3*latitude*altitude+
        -0.16890E0*diameter_quotient+
        -0.18665E0*ba_quotient_pine+
        +0.93429E-1*ba_quotient_spruce+
        -0.74098E-1*ba_quotient_betula+
        -0.47553E-1*divided_plot+
        +0.22560E1+
        +((0.200^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
      )
    )
  }









}
