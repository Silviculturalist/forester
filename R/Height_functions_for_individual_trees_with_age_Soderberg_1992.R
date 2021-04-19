#' Height functions for individual trees with Height from Soderberg 1992
#'
#' @param standData A standData object
#'
#' @return
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
      )
    )
  }


}

