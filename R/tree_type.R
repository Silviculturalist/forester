#' Is a tree species deciduous or coniferous?
#'
#' @description This will try to grab the genus name of a given tree species (in latin). The first letter of family name MUST be capital.
#' It will try to match this name against a group of well known Genus and respond with if they are coniferous, or if not, deciduous. If none is true it will return NA and throw a warning.
#'
#' @param species Latin name
#'
#' @return character: "Deciduous" or "Coniferous"
#' @export
#'
#' @examples
#' tree_type("Picea abies") # Inserting the latin name for Norway spruce will return "Coniferous".
#' tree_type("Pisum sativum") # Inserting the latin name for 'pea' will throw an error and return NA.
tree_type <- function(species){
  deciduous_list <- c("Betula","Alnus","Populus",
                      "Sorbus","Salix","Ulmus",
                      "Fraxinus","Carpinus","Quercus",
                      "Tilia","Fagus","Prunus","Acer",
                      "Robinia","Corylus","Aesculus")
  coniferous_list <- c("Abies","Picea","Larix","Pseudotsuga",
                       "Tsuga","Pinus","Seqouia","Sequoiadendron",
                       "Chamaecyparis","Cupressaceae","Juniperus","Thuja",
                       "Taxus","Cedrus","Cathaya","Pseudolarix","Keteleeria","Nothotsuga")

  species <- grep("^[A-Z]", unlist(strsplit(species, " ")), value=TRUE)

  if(any(grepl(paste(coniferous_list, collapse="|"), species))){
    return("Coniferous")
  }

  else if(any(grepl(paste(deciduous_list, collapse="|"), species))){
    return("Deciduous")
  }

  else if(length(species)==0){
    warning("Did you remember to capitalize the genus name?")
    return(NA)
  }

  else{
    warning("Unknown species.")
    return(NA)
  }

}
