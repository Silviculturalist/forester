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
  Specieswise_per_ha <- treelistData %>% filter(max(observation_year)) %>% group_by(species) %>% summarise(N = n()/siteData$area)
  Total_trees_per_ha <- treelistData %>% filter(max(observation_year)) %>% summarise(N=n()/siteData$area)
  Management_list <- managementData %>% group_by(management_event, year) %>% summarise(m3sk = sum(volume))


}
