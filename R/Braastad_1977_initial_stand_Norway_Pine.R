#' Initial values for young Pine stands in Norway from Braastad 1977.
#'
#' @source Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 274 function (4) function (5)
#'
#' @param dominant_height Dominant height of stand, m.
#' @param stand_total_age Total age of stand.
#' @param stems_per_ha Number of stems per hectare.
#' @param SIH40 Site Index H40 , e.g. Tveite 1976 ([forester::Tveite_1976_height_trajectory_Norway_Pine()])
#'
#' @return A list containing two elements: i) Lorey's Mean height. ii) Basal Area m2 per hectare.
#' @export
Braastad_1977_initial_stand_Norway_Pine <- function(
  dominant_height,
  stand_total_age,
  stems_per_ha,
  SIH40
){
  if(stems_per_ha<1000 | stems_per_ha>6000){
    warning("Outside eligible area! 1000<=stems_per_ha<=6000")
  }
  if(dominant_height<6 | dominant_height>12){
    warning("Outside eligible area! 6<=dominant_height<=12")
  }

  HL <- dominant_height - (219.655 - 16.64*dominant_height + 0.393*(dominant_height^2) + 1.34*stand_total_age - 0.0506*stand_total_age*dominant_height - 0.0513*stems_per_ha + 0.00637*stems_per_ha*dominant_height)/100

  Basal_area_1 <- -1.4443 + 1.2430*HL + 0.0829*SIH40 + 0.0003506*HL*stems_per_ha

  return(
    list(
      "Lorey's mean height"=HL,
      "Basal area m2 / ha"=Basal_area_1
    )
  )
}
