#' Stocking in regenerations
#'
#' @param altitude altitude masl
#' @param latitude latitude decimal
#' @param main_species Main species "Picea abies" or "Pinus sylvestris".
#' @param regeneration_method "natural" or "cultivated-sown" or "cultivated-planted"
#' @param age age, number of vegetation periods between cut and inventory.
#' @param proportion_cultivated percent cultivated, for partly planted sites.
#' @param N_full number seedlings demanded for full stocking. In thousands?
#' @param soil_moisture Soil moisture code (SWE-NFI 'FUKTIGHET')
#' @param county Swedish county
#' @param scarification 1 if scarified, 0 if no treatment.
#' @param burnt 1 if prescribed burning, 0 if no treatment.
#' @param no_treatment 1 if no measures were taken to promote regeneration.
#' @param uncleaned 1 if bushes and small trees remain after clear-cut.
#' @param number_seed_trees number of seed trees per hectare
#' @param SIS
#' @param SIH
#' @param area size of regeneration area in hectares.
#' @param square_spacing Average square spacing used for sowing/planting
#'
#' @return
#' @export
#'
#' @examples
regeneration_stocking <- function(altitude, latitude,age, regeneration_method, proportion_cultivated, N_full, ground_water, county="county", scarification, burnt, no_treatment, uncleaned, number_seed_trees, SIH, SIS, area){
  #SI
  if(exists(SIH)){
    si <- SIH
  } else {
    si <- SIS
  }

  #calculate Jonsson class.
  bonitet <- forester::si_to_bonitet_integrated(H100=si, altitude=altitude,county=county,vegetation=vegetation,main_species=main_species)
  jonsbon <- forester::MAI_to_jonsson(bonitet)
  rm(bonitet)

  #Indicator variable
  if(latitude >= 60){
    northern_sweden <- 1
  } else {
    northern_sweden <- 0
  }

  #calculating the corresponding swedish map..?
  swe_map <- (((geosphere::distHaversine(c(1,latitude),c(1,0))/1000)-6050)/50)

  #is there less than 1 m to the ground water level?
  if(soil_moisture%in%c(3,4,5)){
    ground_water_shallow <- 1
  } else {
    ground_water_shallow <- 0
  }

  # is there more than 2 metres to ground water level?
  if(soil_moisture %in%c(1)){
    ground_water_deep <- 1
  } else{
    ground_water_deep <- 0
  }

  #indicator for Värmland, Medelpad, Jämtland counties. S, Y, Z counties.
  if(county%in%c("Västernorrland - Medelpads landskap",
                 "Jämtland - Jämtlands landskap",
                 "Jämtland - Härjedalens landskap")){
    SYZ_county_indicator <- 1
  } else {
    SYZ_county_indicator <- 0
  }

  #indicator for Gotland.
  if(county=="Gotland"){
    gotland <- 1
  } else {
    gotland <- 0
  }

  #indicator for Örebro county
  if(county=="Örebro"){
    orebro <- 1
  } else {
    orebro <- 0
  }

  #Indicator for natural regeneration without seed-trees.
  if(number_seed_trees==0){
    no_seed_trees <- 1
  } else {
    no_seed_trees <- 0
  }


  #age function
  agef <- 2*(1/((1+exp(-0.3*age))^-0.5))

  if(regeneration_method=="natural"){
    #arcsin transformed dependent variable.
    arcsinSLH <- 1.7413 +
      -0.0163*(((altitude/100)^2)*northern_sweden)+
      0.6863*agef+
      0.6663*proportion_cultivated+
      -0.1500*N_full+
      0.0218*ground_water_deep*swe_map+
      0.2702*ground_water_shallow+
      -0.7552*gotland+
      0.3310*orebro+
      -0.1275*SYZ_county_indicator+
      0.2692*scarification+
      0.2030*burnt+
      -0.1484*no_treatment+
      -0.0947*uncleaned+
      0.1596*number_seed_trees+
      0.0175*swe_map*no_seed_trees+
      -0.0379*jonsbon+
      0.1888*(1/area)+
      0.1075*northern_sweden+
      -0.00619*swe_map

    #Re-transformation for Arc-sin Bias corrected SLH.
    SLH <- 0.056 + 0.887*(sin(arcsinSLH/2)^2)

    return(SLH)
  } else if(method%in%c("cultivated-sown","cultivated-planted")){

    if(main_species="Picea abies"){
      main_species_is_spruce <- 1
    } else {
      main_species_is_spruce <- 0
    }

    if(method=="cultivated-sown"){
      sown <- 1
    } else {
      sown <- 0
    }

    arcsinSLH <- 3.0707+
      +0.4358*(1/age)+
      -0.0614*jonsbon+
      -0.0514*(((altitude/100))*northern_sweden)+ #OBSERVE not squared!
      -0.3591*square_spacing+ #spacing squared or squared spacing?
      0.0760*main_species_is_spruce+
      0.1141*burnt+
      0.0757*scarification+
      -0.0675*sown+
      0.2597*northern_sweden+
      4.7901*(1/swe_map)+
      0.2178*(orebro)+
      -0.1500*(N_full)

    #Re-transformation for Arc-sin Bias corrected SLH.

    SLH <- 0.037 + 0.926*(sin(arcsinSLH/2)^2)
    return(SLH)
  }

}
