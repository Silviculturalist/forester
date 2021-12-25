#' AK Cajander Forest Vegetation Types.
#'
#' @description A function which prints a tibble of AK Cajander's Forest Types.
#'
#' @return Tibble.
#' @export

Finland_vegetation_types <- function(
){
  cat("Cajanders Forest Types\n")
  dplyr::tibble(
    "Code"=c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT","HMT","VT","EMT","CT","MClT","ClT"),
    "Type"=c("Sanicula","Aconitum","Vaccinium-Rubus","Oxalis-Majanthemum","Filicid","Geranium-Dryopteris","Oxalis-Myrtillus","Pyrola","Myrtillus","Hylocomium-Myrtillus","Vaccinium","Empetrum-Myrtillus","Calluna","Myrtillus-Cladina","Cladonia")
  )
}
