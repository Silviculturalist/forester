#' Basal area weighted mean diameter of dead trees from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 53.
#'
#' @param species "Pinus sylvestris" or "Picea abies" or "Betula pendula" or "Betula pubescens"
#' @param age_at_breast_height Age of stems at breast height.
#' @param basal_area_weighted_mean_diameter_cm_living Basal area weighted mean diameter of living trees (in cm)
#'
#' @return Basal area weighted mean diameter of dead trees, in cm.
#' @export
Agestam_1985_mortality_diameter_cm <- function(
  species,
  age_at_breast_height,
  basal_area_weighted_mean_diameter_cm_living
){
  quota <- dplyr::case_when(
    species=="Pinus sylvestris" & age_at_breast_height<45 ~ 0.6,
    species=="Pinus sylvestris" & age_at_breast_height>=45 ~ 0.54,
    species=="Picea abies" ~ 0.61,
    species%in%c("Betula pendula","Betula pubescens") ~ 0.69
    )

  return(
    basal_area_weighted_mean_diameter_cm_living*quota
  )

}
