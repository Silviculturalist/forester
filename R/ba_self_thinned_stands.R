#' Basal area in self-thinned stands
#'
#' @description Based on data from 532 observation periods on 82 yield plots in unthinned stands. Table 7.1, p. 83
#' in Soderberg 1986,
#' "Funktioner for skogliga produktionsprognoser:
#' tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige.
#' Report 14. Dept. Forest Mensuration and Mgmt. Swedish University of Agricultural Sciences.
#' ISBN: 91-576-2634-0. Umeå.
#'
#' @param standData a standData object.
#'
#' @return basal area, m2 per ha.
#' @export
#'
#' @examples
basal_area_self_thinned_stands_Soderberg_1986 <- function(standData){
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

  #Total number of stems per ha.
  number_of_stems_ha <- standData$standSummary$N_total_trees_per_ha

  #Spruce basal area percentage
  ba_quotient_spruce <- basal_area_spruce / basal_area_total

  #Deciduous basal area percentage
  ba_quotient_deciduous <- basal_area_deciduous / basal_area_total

  return(
    exp(

    -18.612*((1/(stand_age+10)))+
    -765.295*(((1/(stand_age+10)))^2)+
    +0.04798*si_spruce+
    +0.05589*si_pine+
    +0.06717E-4*number_of_stems_ha+
    -0.2864E-8*(number_of_stems_ha^2)+
    +0.7204*ba_quotient_spruce+
    -0.4879*(ba_quotient_spruce^2)+
    +0.1062*ba_quotient_deciduous+
    -0.2073*(ba_quotient_deciduous^2)+
    +2.5225+
    +(0.15^2)/2   # logarithmic correction not included in the Heureka documentation, e.g. Baskerville 1972. (sf^2)/2
    #logarithmic correction added since dependent variable is ln(basal area), m2/ha.

    )
  )



}
