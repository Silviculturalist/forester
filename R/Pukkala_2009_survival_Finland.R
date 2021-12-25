#' Probability of Survival for Individual trees in Finland
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.05}
#'
#' @name Pukkala_2009_survival_Finland_Betula_Pine
#'
#' @details For the Pine and Betula model, the area under the ROC curce was 0.93.
#'
#'
#' @param diameter_cm Diameter at breast height 1.3 m of subject tree (cm).
#' @param BA_m2_ha Total Basal Area of all trees with dbh greater than 5 cm. (m2/ha)
#' @param BAL_Spruce Basal Area of all Spruce trees with a dbh larger than that of the subject tree (m2/ha).
#' @param BAL Basal Area of all trees with a dbh larger than that of the subject tree (m2/ha).
#'
#' @md
#'
#' @return Probability to survive the following 5 year period.
#' @export
Pukkala_2009_survival_Finland_Betula_Pine <- function(
  diameter_cm,
  BA_m2_ha,
  BAL_Spruce,
  BAL
){

  a1=0.496
  a2=1.649
  a3=0
  a4=0
  a5=-0.106

    return(
      (1 / (1+ exp(-(a1 + a2*sqrt(diameter_cm) + a3*log(BA_m2_ha) + a4*BAL_Spruce + a5*BAL))))^(5/6)
    )


}

#' @rdname Pukkala_2009_survival_Finland_Betula_Pine
#' @details For the Spruce model, the area under the ROC curve was 0.89.
#' @export

Pukkala_2009_survival_Spruce <- function(
  diameter_cm,
  BA_m2_ha,
  BAL_Spruce,
  BAL
){

  a1=4.418
  a2=1.423
  a3=-1.046
  a4=-0.0954
  a5=0

  return(
    (1 / (1+ exp(-(a1 + a2*sqrt(diameter_cm) + a3*log(BA_m2_ha) + a4*BAL_Spruce + a5*BAL))))^(5/6)
  )


}

