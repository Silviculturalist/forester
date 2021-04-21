standSummary <- function(siteData,treelistData, managementData){

  #Check validity of input, give warning.
  if(!class(siteData)=="siteData"){
    warning("Warning: the parameter siteData is not of siteData class. Make sure it is of the correct format.")
  }
  if(!class(treelistData)=="treelistData"){
    warning("Warning: the parameter treelistData is not of treelistData class. Make sure it is of the correct format.")
  }
  if(!class(managementData)=="managementData"){
    warning("Warning: the parameter managementData is not of managementData class. Make sure it is of the correct format.")
  }

  StandID <- siteData$standID
  Main_cohort_age <- treelistData %>% filter(generation==1) %>% summarise(max(age))
  Main_cohort_mean_height <- treelistData %>% filter(generation==1) %>% summarise(mean(height.m))
  Main_cohort_10percent_height <- treelistData %>% filter(generation==1) %>% summarise(quantile(height.m, 0.9))
  Mean_diameter <- treelistData %>% summarise(mean(diameter.cm))
  stand_basal_area_per_ha <- treelistData %>% summarise(sum(basal_area)/area)
  basal_area_species_wise_per_ha <- treelistData %>% group_by(species) %>% summarise(sum(basal_area)/area)
  basal_area_deciduous_per_ha <- basal_area_species_wise_per_ha %>% mutate(Deciduous= tree_type(species=species)) %>% filter(Deciduous=="Deciduous") %>% summarise(sum(basal_area)/area)
  N_species_wise_per_ha <- treelistData %>% filter(max(observation_year)) %>% group_by(species) %>% summarise(N = n()/siteData$area)
  N_total_trees_per_ha <- treelistData %>% filter(max(observation_year)) %>% summarise(N=n()/siteData$area)
  standing_volume_specieswise_per_ha <- treelistData %>% filter(max(observation_year)) %>% group_by(species) %>%  summarise(Volume=sum(volume)/area)
  management_list <- managementData %>% group_by(management_event, year) %>% summarise(m3sk = sum(volume))

  stand_summary <- list(StandID,
                        Main_cohort_age,
                        Main_cohort_mean_height,
                        Main_cohort_10percent_height,
                        Mean_diameter,
                        stand_basal_area_per_ha,
                        basal_area_species_wise_per_ha,
                        basal_area_deciduous_per_ha,
                        N_species_wise_per_ha,
                        N_total_trees_per_ha,
                        standing_volume_specieswise_per_ha,
                        management_list)
  class(stand_summary) <- c("list","standSummary")

  return(stand_summary)
}
