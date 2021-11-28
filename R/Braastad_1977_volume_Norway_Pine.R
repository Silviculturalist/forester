#' Volume for Pine in Norway from Braastad 1977.
#'
#' @description Braastad uses two functions from Brantseg (1967). Lorey's height is weighted by 0.983 by basis of comparison with all experimental plots.
#'
#' @param diameter_cm Diameter corresponding the basal area mean tree.
#' @param height_m Lorey's mean height, m for the stand.
#'
#' @return Volume, m3sk.
#' @export
#'
#' @examples
#' Braastad_1977_volume_Norway_Pine(23,19)
Braastad_1977_volume_Norway_Pine <- function(
  diameter_cm,
  height_m
){

return(
  ifelse(diameter_cm>10 & diameter_cm<14, #If between 10 and 14 cm, the mean of the smaller and larger function.
              ((2.0044 + 0.029886*(diameter_cm^2) + 0.036972*(diameter_cm^2)*(height_m*0.983))+(-9.98 + 0.20480*(diameter_cm^2) + 0.029966*(diameter_cm^2)*(height_m*0.983) + 0.003539*(diameter_cm^2)*(4.12 - 0.002817*(diameter_cm^2) + 0.02623*((height_m*0.983)^2) - 0.3184*((diameter_cm/(height_m*0.983))^2)) - 0.002918*(diameter_cm^2)*(2.96 + 1.15*diameter_cm - 0.73*(diameter_cm/(height_m*0.983)))  )/2),
              ifelse(diameter_cm<12,
              2.0044 + 0.029886*(diameter_cm^2) + 0.036972*(diameter_cm^2)*(height_m*0.983), #smaller function
              -9.98 + 0.20480*(diameter_cm^2) + 0.029966*(diameter_cm^2)*(height_m*0.983) + 0.003539*(diameter_cm^2)*(4.12 - 0.002817*(diameter_cm^2) + 0.02623*((height_m*0.983)^2) - 0.3184*((diameter_cm/(height_m*0.983))^2)) - 0.002918*(diameter_cm^2)*(2.96 + 1.15*diameter_cm - 0.73*(diameter_cm/(height_m*0.983))) #larger function
              )
              )
)

}
