#' AK Cajander Forest Vegetation Types.
#'
#' @source Cajander, A.K. 1949. Forest types and their significance. Acta Forestalia Fennica. 56:5. 71 pp.
#' Available (16/03/2022): \url{https://www.silvafennica.fi/pdf/article7396.pdf}
#'
#' @description A function which prints a tibble of AK Cajander's Forest Types.
#'
#' @param short TRUE (default) or FALSE, for short or long list, respectively.
#'
#' @return Tibble.
#' @export

Finland_vegetation_types <- function(
    short=TRUE
){
  cat("Cajanders Forest Types\n")

  if(short==TRUE){
    dplyr::tibble(
      "Code"=c("LH","OMT","MT","VT","CT"),
      "Finnish"=c("Lehdot","Käenkaali-mustikkatyyppi","Mustikkatyyppi","Puolukkatyyppi","Kanervatyyppi"),
      "Type"=c("Groves","Oxalis-Myrtillus","Myrtillus","Vaccinium","Calluna")
    )
  } else

  dplyr::tibble(
    "Code"=c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT","HMT","VT","EMT","CT","MClT","ClT"),
    "Type"=c("Sanicula","Aconitum","Vaccinium-Rubus","Oxalis-Majanthemum","Filicid","Geranium-Dryopteris","Oxalis-Myrtillus","Pyrola","Myrtillus","Hylocomium-Myrtillus","Vaccinium","Empetrum-Myrtillus","Calluna","Myrtillus-Cladina","Cladonia")
  )
}
