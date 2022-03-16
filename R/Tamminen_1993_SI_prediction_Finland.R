
#' SI prediction for Norway Spruce and Scots Pine in southern Finland.
#'
#' @source Tamminen, P. 1993. Pituusboniteetin ennustaminen kasvupaikan ominaisuuksien avulla
#' Etelä-Suomen kangasmetsissä. Summary: Estimation of site index for Scots Pine and Norway
#' spruce stands in South Finland using site properties. Folia Forestalia 819. 26 pp. Available
#' (16/03/2022): \url{https://jukuri.luke.fi/handle/10024/522402}
#'
#' @param vegetation One of "OMT"<"MT"<"VT"<"CT". For Spruce one of: ("LH" equal growth as "OMT")<"MT"<"VT"
#' @param temperature_sum Effective temperature sum (>5 C), e.g. Ojansuu & Henttonen 1983.
#' @param fine_fraction_lower_than_15_percent
#' @param clay_content_higher_than_10_percent
#'
#' @name tamminen_1993

Tamminen_1993_SI_prediction_southern_Finland_Norway_Spruce <- function(
  vegetation,
  temperature_sum,
  fine_fraction_lower_than_15_percent,
  clay_content_higher_than_10_percent
){
  LH <- ifelse(vegetation=="LH",1,0)
  OMT <- ifelse(vegetatation=="OMT",1,0)
  VT <- ifelse(vegetation=="VT",1,0)

  return(
    4.35 + 2.51*OMT + 0.0179*temperature_sum + -5.98*VT + 4.50*LH + -1.94*fine_fraction_lower_than_15_percent + -1.89*clay_content_higher_than_10_percent
  )

}

#' @rdname tamminen_1993
#' @param humus_thickness_cm
#' @param rod_penetration_cm
#' @param shallow_soil_l_t_30_cm
Tamminen_1993_SI_prediction_southern_Finland_Scots_Pine <- function(
    vegetation,
    temperature_sum,
    humus_thickness_cm,
    rod_penetration_cm,
    shallow_soil_l_t_30_cm
){
  CT <- ifelse(vegetation=="CT",1,0)
  MT <- ifelse(vegetation=="MT",1,0)
  OMT <- ifelse(vegetation=="OMT",1,0)

  return(
    4.86 + -4.94*CT + 2.16*MT + 0.0134*temperature_sum + -3.60*shallow_soil_l_t_30_cm + 3.19*OMT + 0.066*rod_penetration_cm + -0.316*humus_thickness_cm
  )

}
