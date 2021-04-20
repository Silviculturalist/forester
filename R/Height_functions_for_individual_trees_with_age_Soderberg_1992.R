#' Height functions for individual trees with Height from Soderberg 1992
#'
#' @param standData A standData object
#'
#' @return Individual tree height, decimetres.
#' @export
#'
#' @examples
heightIndividualTreesWithAgeSoderberg1992 <- function(standData){
  if(!class(standData)%in%c("standData")){
    stop("Input standData is not a standData object. Please make sure it is the correct class.")
  }

  #Sites as SI Pine
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

  #Sites as SI Spruce
  si_spruce <- forester::SIS_estimate(species = "Picea abies",
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

  #Age of tree
  tree_age <- standData$treelistData$age

  #Age at breast height is equal to age of tree minus time to reach breast height.
  time_to_breast_height <- forester::time_to_reach_breast_height(main_species = standData$treelistData$species,SIS = ,latitude = standData$siteData$latitude)

  #Total age of the stand
  stand_age <- standData$standSummary$Main_cohort_age

  #breast height function Soderberg
  age_breast_height_f <- 1/(standData$treelistData$age_at_breast_height + 10)

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

  #local climate code Sweden Ångström 1958
  climate_code <- local_climate_sweden(standData$siteData$latitude, standData$siteData$longitude)

  #1 if in maritime locality
  maritime <- ifelse(climate_code %in% c("M1","M2","M3"),1,0)

  divided_plot <- 0

  ########

  #Pine Northern Sweden height
  if(standData$treelistData$species=="Pinus sylvestris" && northern_sweden==1){
    return(
      exp(
        -0.27995E3
        +0.67816E4
        -0.80059E1
        +0.10056E3
        -0.18990E-2
        -0.66679E-1
        +0.14751E-2
        +0.14142E-2
        +0.11473E-1
        -0.12937E-3
        -0.15622E0
        +0.22920E-1
        +0.45084E0
        -0.35449E-2
        -0.20261E-6
        +0.12172E0
        +0.63897E-1
        -0.26745E-1
        -0.47547E-1
        -0.16487E-1
        -0.82981E1
        +0.00819 #correction for logarithmic bias, appendix 5.


      )
    )
  }

else if(standData$treelistData$species=="Pinus sylvestris" && middle_sweden==1){
  return(
    exp(
      -0.28536E3
      +0.75233E4
      -0.10295E2
      +0.11934E3
      -0.18296E-2
      -0.40078E-1
      +0.11538E-2
      +0.13061E-2
      +0.11572E-1
      -0.11765E-3
      -0.24016E0
      +0.63405E-1
      +0.28478E1
      -0.23177E-1
      -0.44140E-6
      +0.29354E-1
      -0.37010E-1
      -0.53233E-1
      -0.50755E-1
      -0.81303E2
      +0.00673 #correction for logarithmic bias, appendix 5.


    )
  )
}

  else if(standData$treelistData$species=="Pinus sylvestris" && southern_sweden==1){
    return(
      exp(
        -0.32824E3
        +0.10788E5
        -0.10143E2
        +0.11451E3
        -0.30959E-2
        -0.49050E-1
        +0.13824E-2
        +0.17327E-2
        +0.95731E-2
        -0.95444E-4
        +0.21060E0
        -0.21919E0
        -0.30430E0
        +0.26409E-2
        -0.59174E-4
        +0.14785E0
        +0.24375E0
        +0.12387E0
        -0.25540E-1
        -0.60927E-1
        +0.15100E-1
        -0.42302E-1
        +0.14687E2
        +0.00819 #correction for logarithmic bias, appendix 5.

      )

    )

  }

  else if(standData$treelistData$species=="Picea abies" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.28927E3
        +0.52762E4
        -0.45821E1
        +0.13600E3
        -0.10807E-2
        -0.38071E-1
        +0.74290E-3
        +0.58052E-3
        +0.11361E-1
        -0.10922E-3
        +0.28029E0
        -0.24060E0
        -0.71695E-4
        +0.11744E-3
        -0.56669E-6
        +0.95561E-1
        +0.87584E-1
        -0.41899E-1
        +0.13342E-1
        -0.39180E-1
        +0.61585E1
        +0.00832 #correction for logarithmic bias, appendix 5.
      )
    )
  }

 else if(standData$treelistData$species=="Picea abies" && southern_sweden==1){
    return(
      exp(
        -0.33048E3
        +0.73015E4
        -0.24683E1
        +0.96352E2
        -0.20097E-2
        -0.49498E-1
        +0.92562E-3
        +0.10289E-2
        +0.77592E-2
        -0.68334E-4
        +0.38474E0
        -0.34260E0
        +0.33536E-3
        -0.10375E-5
        +0.24412E-1
        +0.13087E0
        -0.43684E-1
        +0.20959E-1
        -0.16755E-1
        +0.18975E-1
        +0.60404E1
        +0.00769 #correction for logarithmic bias, appendix 5.
      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.22919E3
        +0.48986E4
        +0.52925E1
        -0.39008E-1
        +0.96299E-3
        +0.83824E-3
        +0.16511E-1
        -0.17611E-3
        -0.87603E-1
        +0.25864E-1
        -0.13252E-1
        -0.10123E-3
        -0.46130E-6
        -0.40542E-1
        -0.56039E-1
        -0.49758E-1
        +0.64427E1
        +0.01008 #correction for logarithmic bias, appendix 5.


      )
    )
  }

  else if(str_detect(standData$treelistData$species, "^Betula") && southern_sweden==1){
    return(
      exp(
        -0.21624E3
        +0.47683E4
        -0.14308E2
        -0.10377E-1
        +0.57275E-3
        +0.70769E-3
        +0.12258E-1
        -0.14025E-3
        +0.15315E0
        -0.25440E0
        +0.16736E-1
        -0.35533E-5
        -0.46067E-1
        +0.77659E-1
        +0.81706E-1
        -0.69576E-1
        -0.59341E-1
        -0.41456E-1
        +0.46194E1
        +0.01638 #correction for logarithmic bias, appendix 5.

      )
    )
  }

  else if(standData$treelistData$species=="Fagus sylvatica"){
    return(
      exp(
        -0.12884E3
        -0.21199E2
        +0.21053E3
        +0.15372E-3
        +0.97506E-2
        -0.90290E-4
        -0.39544E0
        +0.29474E0
        -0.17280E-5
        +0.15103E0
        -0.13339E0
        +0.97831E-1
        +0.56648E1
        +0.01140 #correction for logarithmic bias, appendix 5.

      )
    )
  }


  else if(str_detect(standData$treelistData$species, "^Quercus")){
    return(
      exp(
        -0.24748E3
        +0.61199E4
        -0.16569E-2
        -0.16744E-1
        +0.99719E-3
        +0.11754E-2
        +0.10960E-1
        -0.10344E-3
        -0.11226E0
        -0.23866E-3
        +0.16577E0
        -0.10511E0
        -0.54901E-1
        +0.56224E1
        +0.01462 #correction for logarithmic bias, appendix 5.

      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && any(c(northern_sweden,middle_sweden))){
    return(
      exp(
        -0.10949E3
        -0.20419E2
        +0.22966E3
        -0.21025E-2
        +0.11182E-2
        +0.12554E-2
        +0.17302E-1
        -0.16525E-3
        +0.64313E0
        -0.34368E0
        -0.23565E-1
        +0.10443E0
        +0.11788E0
        +0.26360E-1
        -0.53057E-1
        +0.13323E0
        +0.63675E1
        +((0.179^2)/2) #Baskerville 1972, funktion was not include in appendix 5.

      )
    )
  }

  else if(forester::tree_type(species = species)=="Deciduous" && southern_sweden==1){
    return(
      exp(
        -0.21737E3
        +0.66360E4
        -0.39007E2
        +0.80255E-3
        +0.76595E-3
        +0.10224E-1
        -0.94866E-4
        +0.26833E0
        -0.30654E0
        +0.24865E-1
        +0.93169E-1
        -0.29612E-1
        -0.67029E-1
        +0.39136E1
        +((0.181^2)/2) #Baskerville 1972, funktion was not include in appendix 5.

      )
    )
  }

}

