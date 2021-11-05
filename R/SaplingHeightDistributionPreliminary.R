SaplingHeightDistribution <- function(
  SpeciesToPlant,
  SelfRejuvenated,
  MinHeightVariation,
  MaxHeightVariation,
  Deterministic,
  Wet,
  Dry,
  vegetation,

){

  #Missing Sedge high?
  Rich <- ifelse(vegetation<=9 | vegetation==12,1,0) #If Rich.
  Herb <- ifelse(vegetation<7,1,0) #If low or high herb
  Shrubs <- ifelse(vegetation>12) #If Shrub and Lichen.
  Lichen <- ifelse(vegetation>16) #If Lichen
  Poor <- ifelse(vegetation==11 | vegetation>13) #Sedge Low or Lingonberry and worse.

  #Retrieves random if stochastic. Otherwise 0.

  gaussRandom <- if(Deterministic==0){
    runif(n=1)
  }

  #Calculate Proportion Conifer

  proportionOfConifer <- function(

  )








}
