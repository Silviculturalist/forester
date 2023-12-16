#' Calculate the Hart-Becking Relative Spacing Index.
#'
#' @param stems_per_ha Stems per hectare
#' @param dominant_height Dominant height of the stand.
#'
#' @return Hart - Becking Spacing Index (percent).
#' @export
#'
#' @examples
#' Hart_Becking_relative_spacing_index(stems_per_ha = 1600,dominant_height =8.6 )
Hart_Becking_relative_spacing_index <- function(
  stems_per_ha,
  dominant_height
){

  return(
    (sqrt(10000/stems_per_ha)/dominant_height)*100
  )

}

#' Lorey's mean height.
#'
#' @description Lorey's mean height is calculated by dividing the sum of the
#' product of the height and basal area of each tree by the total stand basal area.
#'
#' @details This function can handle vectors.
#'
#' @param tree_height_m numeric vector. Tree height in meters.
#' @param basal_area_m2 numeric vector. Basal area in m2.
#'
#' @return Lorey's mean height.
#' @export
#'
#' @examples
#' heights <- c(15,12,14,15,10)
#' ba <- c(0.4,0.5,0.6,0.5,0.5)
#'
#' Lorey_mean_height(tree_height_m=heights,basal_area_m2=ba)
#'
Lorey_mean_height <- function(
    tree_height_m,
    basal_area_m2
){
  return(sum(tree_height_m*basal_area_m2)/sum(basal_area_m2))
}


#' Calculate basal area weighted mean stem diameter from arithmetic mean stem diameter and
#' its' standard deviation e.g. Oppermann 1905, Cajanus 1912.
#'
#' @description A relationship commonly attributed to either Oppermann (1905)
#' or Cajanus (1912).
#'
#' @source Meyer, W. 1930. Diameter distribution Series in Evenaged Forest
#' Stands. Yale School of Forestry Bulletin 28. 105 pp. p.24. Available online:
#'  \url{https://elischolar.library.yale.edu/cgi/viewcontent.cgi?article=1027&context=yale_fes_bulletin}
#' @param diameter Mean arithmetic diameter of the stand in cm.
#' @param diameter_sd Standard deviation of the stem distribution of the stand.
#' @param QMD The basal area weighted mean diameter (cm) of the stand (Dg).
#'
#' @return Stand QMD.
#'
#' @name CajanusDg
#' @export

Oppermann_Cajanus_stand_QMD <- function(
    diameter,
    diameter_sd
){
  return(
    sqrt((diameter^2 + diameter_sd^2))
  )
}


#' @rdname CajanusDg
#' @return The mean diameter of the stand, cm.
#' @source Reformulation : Eriksson, H. 1976. Yield of Norway spruce in Sweden.
#' Research Notes. Dept. of. Forest Yield Research. Nr. 41. Royal College of
#' Forestry. Stockholm. p. 165.
#' @export
Oppermann_Cajanus_mean_diameter <- function(
    QMD,
    diameter_sd
){
  return(
    sqrt((DG^2) - (diameter_sd^2))
  )
}



#' @rdname CajanusDg
#' @param stems Stems per hectare.
#' @return Stand total basal area
#' @export
Oppermann_Cajanus_stand_BA <- function(
    stems,
    diameter,
    diameter_sd
){
  return(
    stems*pi/4*(diameter^2 + diameter_sd^2)
  )
}

#' Calculate the Quadratic Mean Diameter of a stand, cm.
#'
#' @source Curtis, R.O., Marshall, D.D. 2000. Technical Note: Why Quadratic Mean Diameter? West. J. Appl. For. 15(3):137-139. Available: \url{https://www.fs.fed.us/pnw/olympia/silv/publications/opt/436_CurtisMarshall2000.pdf}
#'
#' @param Basal_area_m2_ha Basal area m2 / ha.
#' @param stems_per_ha Stems per ha.
#'
#' @return QMD, cm.
#' @export
#'
#' @examples
quadratic_mean_diameter <- function(
    Basal_area_m2_ha,
    stems_per_ha
){

  return(
    sqrt((Basal_area_m2_ha*10000)/((pi/4)*stems_per_ha))
  )

}

#' Stand Basal Area from Quadratic mean diameter and stems per hectare.
#'
#' @source Curtis, R.O., Marshall, D.D. 2000. Technical Note: Why Quadratic Mean Diameter? West. J. Appl. For. 15(3):137-139. Available: \url{https://www.fs.fed.us/pnw/olympia/silv/publications/opt/436_CurtisMarshall2000.pdf}
#'
#' @param stems_per_ha Stems per hectare.
#' @param QMD Quadratic mean diameter, cm.
#'
#' @return Basal area, m2/ha.
#' @export
#'
#' @examples
stand_basal_area_QMD <- function(
    stems_per_ha,
    QMD
){
  return(
    stems_per_ha*((pi/4)/10000)*(QMD^2)
  )
}

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

#' Plant Spacing
#'
#' @param distance The \strong{square} system distance between plants
#' @param distance_a The \strong{in-row} distance between plants
#' @param distance_b The \strong{between-row} distance.
#' @param system Planting system, one of \emph{square} or \emph{rectangular}. Defaults to \strong{square}
#' @param stems_per_ha The number of steams planted per hectare
#' @param area_m2 area in square metres. Defaults to 1 hectare, 10'000 sq. metres.
#' @param output One of \emph{stems per ha} or \emph{spacing}
#'
#' @return Prints the output value.
#' @export
#'
#' @examples
#'
#' #Plant spacing for 2500 seedlings per hectare.
#' plant_spacing(stems_per_ha=2500, output="spacing")
#'
plant_spacing <- function(distance, distance_a, distance_b, system="square", stems_per_ha, area_m2=10000, output="spacing"){

  if(output=="stems per ha" & system=="rectangular"){
    area_m2/(dist_a*dist_b)
  }

  if(output=="stems per ha" & system=="square"){
    area_m2/(distance^2)
  }

  if(output=="spacing" & system=="square" | output=="spacing" & missing(system)){
    sqrt(area_m2/stems_per_ha)
  }
}

