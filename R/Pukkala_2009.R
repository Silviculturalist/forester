#' Diameter Increment for Uneven-sized Spruce forests in Finland from Pukkala 2009
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#'
#' @details Pine model is based on 15282 trees. R^2 = 0.4, SD of resid.= 0.527. Snowdon correction = 1.110
#'
#' @param BAL_Spruce Basal area of Spruce trees larger than the subject tree (m2 / ha)
#' @param BAL_other Basal area of trees other than Spruce larger than the subject tree (m2/ha)
#' @param BA Basal Area of all trees larger than 5 cm dbh. m2/ha.
#' @param diameter_cm Diameter of subject tree at breast height 1.3m, cm.
#' @param vegetation One of [forester::Finland_vegetation_types()]
#' @param temperature_sum Degree days
#'
#' @name Pukkala_2009_diameter_increment_Finland_Pine
#' @md
#'
#' @return 5 year over-bark diameter increment.
#' @export
Pukkala_2009_diameter_increment_Finland_Pine <- function(
  BAL_Spruce,
  BAL_other,
  BA,
  diameter_cm,
  vegetation,
  temperature_sum
){

  MT = vegetation=="MT"
  VT = vegetation=="VT"
  CT = vegetation=="CT"
  ClT = vegetation=="ClT"

  a1=-7.758
  a2=-0.0530
  a3=-0.0335
  a4=-0.266
  a5=0.237
  a6=-0.000901
  a7=-0.238
  a8=-0.333
  a9=-0.612
  a10=-1.201
  a11=1.229

    return(
      exp((a1+a2*BAL_Spruce + a3*BAL_other + a4*log(BA) + a5*sqrt(diameter_cm) + a6*(diameter_cm^2) + a7*MT + a8*VT + a9*CT + a10*ClT + a11*log(temperature_sum)))*1.110
    )

}

#' @details Spruce model is based on 24014 trees. R^2 = 0.568, SD of resid.= 0.604. Snowdon correction = 1.124
#' @rdname Pukkala_2009_diameter_increment_Finland_Pine
#' @export
#'
Pukkala_2009_diameter_increment_Finland_Spruce <- function(
  BAL_Spruce,
  BAL_other,
  BA,
  diameter_cm,
  vegetation,
  temperature_sum
){

  MT = vegetation=="MT"
  VT = vegetation=="VT"
  CT = vegetation=="CT"
  ClT = vegetation=="ClT"

  a1=-5.317
  a2=-0.0106
  a3=-0.0430
  a4=-0.486
  a5=0.455
  a6=-0.000927
  a7=-0.180
  a8=-0.450
  a9=-0.929
  a10=0
  a11=0.823

  return(
    exp((a1+a2*BAL_Spruce + a3*BAL_other + a4*log(BA) + a5*sqrt(diameter_cm) + a6*(diameter_cm^2) + a7*MT + a8*VT + a9*CT + a10*ClT + a11*log(temperature_sum)))*1.124
  )

}

#'@details @details Betula model is based on 8219 trees. R^2 = 0.541, SD of resid.= 0.599. Snowdon correction = 1.127
#'@rdname Pukkala_2009_diameter_increment_Finland_Pine
#'@export

Pukkala_2009_diameter_increment_Finland_Betula <- function(
  BAL_Spruce,
  BAL_other,
  BA,
  diameter_cm,
  vegetation,
  temperature_sum
){

  MT = vegetation=="MT"
  VT = vegetation=="VT"
  CT = vegetation=="CT"
  ClT = vegetation=="ClT"

  a1=-11.873
  a2=-0.0304
  a3=-0.0474
  a4=-0.173
  a5=0.446
  a6=-0.00123
  a7=-0.121
  a8=-0.227
  a9=-0.524
  a10=0
  a11=1.627

  return(
    exp((a1+a2*BAL_Spruce + a3*BAL_other + a4*log(BA) + a5*sqrt(diameter_cm) + a6*(diameter_cm^2) + a7*MT + a8*VT + a9*CT + a10*ClT + a11*log(temperature_sum)))*1.127
  )

}
#' Diameter of Ingrowth in Uneven-sized stands in Finland
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#'
#' @details Spruce model is based on 170 plot observations, R^2= 0.586, SD of resid. = 0.0473.
#' @name Pukkala_2009_diameter_ingrowth_Finland_Spruce
#' @inheritParams Pukkala_2009_ingrowth_Finland_Spruce
#'
#' @return Diameter cm dbh.
#' @export
#'
Pukkala_2009_diameter_ingrowth_Finland_Spruce <- function(
  BA,
  stems_ha_spruce,
  stems_ha_other,
  vegetation
){

  MT = vegetation=="MT"
  VT_minus = vegetation%in%c("VT","EMT","CT","MClT","ClT")

  a1=2.004
  a2=-0.101
  a3=-0.0176
  a4=-0.0646

  return(
    exp(a1 + a2*log(BA) + a3*MT + a4*VT_minus)
  )

}

#' @rdname Pukkala_2009_diameter_ingrowth_Finland_Spruce
#' @export
#' @details @details Pine & Betula model is based on 141 plot observations, R^2= 0.469, SD of resid. = 0.0730.

Pukkala_2009_diameter_ingrowth_Finland_Betula_Pine <- function(
  BA,
  stems_ha_spruce,
  stems_ha_other,
  vegetation
){

  MT = vegetation=="MT"
  VT_minus = vegetation%in%c("VT","EMT","CT","MClT","ClT")

  a1=1.958
  a2=-0.0841
  a3=-0.0425
  a4=-0.0556

  return(
    exp(a1 + a2*log(BA) + a3*MT + a4*VT_minus)
  )

}
#' Height model for Individual Trees from Pukkala 2009.
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#'
#' @param vegetation Code from [forester::Finland_vegetation_types()].
#' @param diameter_cm Diameter of the tree at breast height (1.3 m)
#' @details Pine model based on 8622 observations. R^2=0.779. SD resid. = 2.360
#'
#' @return Tree height.
#' @export
#' @name Pukkala_2009_height_model_Finland_Pine
#' @md

Pukkala_2009_height_model_Finland_Pine <- function(
  vegetation,
  diameter_cm
){

  MT_plus <- vegetation%in%c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT")
  VT <- vegetation=="VT"
  CT <- vegetation=="CT"
  ClT <- vegetation=="ClT"

  a1=25.014
  a2=7.680
  a3=6.376
  a4=-1.787
  a5=-3.296
  b1=19.260
  b2=31.721

  return(
    (a1+ (a2*MT_plus + a3*VT + a4*CT + a5*ClT))/(1+(b1/diameter_cm) + (b2/(diameter_cm^2)))
  )

}

#'
#'@details Betula model based on 1200 observations. R^2=0.802, SD resid.=2.001
#'@export
#'@rdname Pukkala_2009_height_model_Finland_Pine

Pukkala_2009_height_model_Finland_Betula <- function(
  vegetation,
  diameter_cm
){

  MT_plus <- vegetation%in%c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT")
  VT <- vegetation=="VT"
  CT <- vegetation=="CT"
  ClT <- vegetation=="ClT"

  a1=29.375
  a2=7.714
  a3=3.059
  a4=-2.870
  a5=0
  b1=22.640
  b2=-8

  return(
    (a1+ (a2*MT_plus + a3*VT + a4*CT + a5*ClT))/(1+(b1/diameter_cm) + (b2/(diameter_cm^2)))
  )

}

#' @details Norway Spruce model based on 12144 observations. R^2 = 0.856, SD resid. = 2.104.
#' @rdname Pukkala_2009_height_model_Finland_Pine
#' @export

Pukkala_2009_height_model_Finland_Norway_Spruce <- function(
  vegetation,
  diameter_cm
){

  MT_plus <- vegetation%in%c("ST","AT","VRT","OMaT","FT","GDT","OMT","PyT","MT")
  VT <- vegetation=="VT"
  CT <- vegetation=="CT"
  ClT <- vegetation=="ClT"

  a1=33.726
  a2=5.965
  a3=2.178
  a4=-1.399
  a5=0
  b1=25.683
  b2=37.785

  return(
    (a1+ (a2*MT_plus + a3*VT + a4*CT + a5*ClT))/(1+(b1/diameter_cm) + (b2/(diameter_cm^2)))
  )

}

#' Ingrowth model for Uneven-sized stands in Finland
#'
#' @source Pukkala, T., Lähde, E., Laiho, O. (2009). Growth and Yield Models of uneven-sized forest stands in Finland. For. Ecol. Manage. 258: 207-216. Available (25/12/2021) \url{https://doi.org/10.1016/j.foreco.2009.03.052}
#' @description Baskerville Correction has been added to the constant.
#' @details The Spruce model is based on 10545 living trees, 1127 dead trees. Best threshold probability is c. 0.5-0.6. Percentage of correct predictions 90.7.
#' @param stems_ha_spruce Spruce stems per hectare >5 cm dbh.
#' @param stems_ha_other Stems other than Spruce per hectare > 5 cm dbh.
#' @param vegetation One of [forester::Finland_vegetation_types()]
#' @param BA Total Basal Area (m2/ha) of trees >5 cm dbh.
#'
#' @return Number of ingrowth per hectare.
#' @name Pukkala_2009_ingrowth_Finland_Spruce
#' @export
#'
Pukkala_2009_ingrowth_Finland_Spruce <- function(
  stems_ha_spruce,
  stems_ha_other,
  vegetation,
  BA
){

  MT_minus = vegetation%in%c("MT","HMT","VT","EMT","CT","MClT","ClT")

  a1=4.688
  a2=-0.712
  a3=0
  a4=0.083
  a5=0
  a6=-0.567

  return(
  exp(a1 + a2*sqrt(BA) + a3*log(BA) + a4*sqrt(stems_ha_spruce) + a5*sqrt(stems_ha_other) + a6*MT_minus) - 1
  )


}

#' @rdname Pukkala_2009_ingrowth_Finland_Spruce
#' @details Betula and Pine model is based on 3230 living trees, 289 dead trees. Best threshold probability = 0.6. Percentage of correct predictions = 92.6
#' @export

Pukkala_2009_ingrowth_Finland_Betula_Pine <- function(
  stems_ha_spruce,
  stems_ha_other,
  vegetation,
  BA
){

  MT_minus = vegetation%in%c("MT","HMT","VT","EMT","CT","MClT","ClT")

  a1=6.154
  a2=0
  a3=-1.683
  a4=0
  a5=0.0642
  a6=0

  return(
    exp(a1 + a2*sqrt(BA) + a3*log(BA) + a4*sqrt(stems_ha_spruce) + a5*sqrt(stems_ha_other) + a6*MT_minus) -1
  )


}
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
#' @param BA Total Basal Area of all trees with dbh greater than 5 cm. (m2/ha)
#' @param BAL_Spruce Basal Area of all Spruce trees with a dbh larger than that of the subject tree (m2/ha).
#' @param BAL Basal Area of all trees with a dbh larger than that of the subject tree (m2/ha).
#'
#' @md
#'
#' @return Probability to survive the following 5 year period.
#' @export
Pukkala_2009_survival_Finland_Betula_Pine <- function(
  diameter_cm,
  BA,
  BAL_Spruce,
  BAL
){

  a1=0.496
  a2=1.649
  a3=0
  a4=0
  a5=-0.106

    return(
      (1 / (1+ exp(-(a1 + a2*sqrt(diameter_cm) + a3*log(BA) + a4*BAL_Spruce + a5*BAL))))^(5/6)
    )


}

#' @rdname Pukkala_2009_survival_Finland_Betula_Pine
#' @details For the Spruce model, the area under the ROC curve was 0.89.
#' @export

Pukkala_2009_survival_Spruce <- function(
  diameter_cm,
  BA,
  BAL_Spruce,
  BAL
){

  a1=4.418
  a2=1.423
  a3=-1.046
  a4=-0.0954
  a5=0

  return(
    (1 / (1+ exp(-(a1 + a2*sqrt(diameter_cm) + a3*log(BA) + a4*BAL_Spruce + a5*BAL))))^(5/6)
  )


}

