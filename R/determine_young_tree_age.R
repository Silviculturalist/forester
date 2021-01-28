#' Determine age at breast height for young trees
#'
#' @param height_dm Height in decimetres.
#' @param species One of Pinus sylvestris, Picea abies, Betula pendula, Betula pubescens
#' @param mean_height Arithmetic mean height of future crop trees (1600 stems per ha)
#' @param si dm, SI H100 spruce for spruce, H100 Pine for pine.
#' @param source_sprout 1 for sprout, else 0
#'
#' @return age at breast height of young trees
#' @export

determine_young_tree_age <- function(height_dm, species, mean_height, si, source_sprout=0){

  height_dm %>% is_in_open_range(.,lower=13)

  if(si<100){
    rlang::warn(message="si must be in dm")
  }

  if(species=="Pinus sylvestris"){

    2.548*log(height_dm-12) + (68.23 * ((height_dm/si)^2)) + (-0.003*(height_dm*si/10)) + 24.54*((mean_height/si)^2) + 0.572*((height_dm-mean_height)/mean_height)

  } else if (species=="Picea abies"){

    2.643*log(height_dm-12) + (78.64 * ((height_dm/si)^2)) + (-0.002*(height_dm*si/10)) + 28.86*((mean_height/si)^2) + 0.510*((height_dm-mean_height)/mean_height)

  } else if (startsWith(species,"Betula")){

      peb <- ifelse(species=="Betula pendula",1,0)

      1.988*log(height_dm-12) + (61.89 * ((height_dm/si)^2)) + (-1.490*(mean_height/si)) + (-15.72*peb*((height_dm/si)^2)) + 0.014*source_sprout*(height_dm-50)

  }
}
