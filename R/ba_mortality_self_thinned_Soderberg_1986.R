#' Mortality in self-thinned stands
#'
#' @description From Table 7.2, p. 86 in Soderberg 1986,
#' "Funktioner for skogliga produktionsprognoser:
#' tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige.
#' Report 14. Dept. Forest Mensuration and Mgmt. Swedish University of Agricultural Sciences.
#' ISBN: 91-576-2634-0. Umeå.
#'
#' @param standData
#'
#' @return mortality is annual proportion of basal area
#' @export
#'
#' @examples
ba_mortality_self_thinned_Soderberg_1986 <- function(standData){
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

  #Main cohort age.
  stand_age <-  standData$standSummary$Main_cohort_age

  #Total basal area on plot
  basal_area_total <- standData$treelistData %>% summarise(sum(basal_area))

  #Basal area Norway Spruce on plot
  basal_area_spruce <- standData$treelistData %>% filter(species== "Picea abies") %>% summarise(sum(basal_area))

  #Basal area Deciduous on plot
  basal_area_spruce <- standData$standSummary$basal_area_deciduous_per_ha

  #Spruce basal area percentage
  ba_quotient_spruce <- basal_area_spruce / basal_area_total

  #Deciduous basal area percentage
  ba_quotient_deciduous <- basal_area_deciduous / basal_area_total

  return(
      +0.60949*(1/(stand_age+10))+
      -12.5903*((1/(stand_age+10))^2)+
      +0.3317E-3*basal_area_total+
      -0.01006*(log(basal_area_total))+
      +0.173E-3*si_spruce+
      +0.156E-3*si_pine+
      -0.0130*ba_quotient_spruce+
      +0.0119*(ba_quotient_spruce^2)+
      +0.0189*(ba_quotient_deciduous)+
      -0.0174*(ba_quotient_deciduous^2)+
      +0.0232
  )

}
