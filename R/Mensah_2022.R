#' Basal Area development in conifer stands in Sweden.
#'
#' @source Mensah, A. A., Holmström, E., Nyström, K., Nilsson, U. 2022.
#' Modelling potential yield capacity in conifers using Swedish long-term
#' experiments. For. Ecol. Manage. 512. Available online (2022-04-08):
#' \url{https://doi.org/10.1016/j.foreco.2022.120162}
#'
#' @description Note: BA should be given for age>40 for Norway Spruce or
#' Scots Pine.For Larch and Lodgepole Pine, age should be 30. The function will
#' throw a warning.
#'
#'
#' @param BA Basal area at breast height, m2 / ha.
#' @param age Total stand age at start.
#' @param age2 Total stand age desired.
#' @param stems Number of stems per hectare at start.
#' @param H100 Site Index H100 e.g. \cr
#' \tabular{ll}{
#' Scots Pine\tab [forester::Elfving_Kiviste_1997_height_trajectory_Sweden_Pine()]\cr
#' Norway Spruce\tab [forester::Elfving_2003_height_trajectory_Sweden_Spruce()]\cr
#' Lodgepole Pine\tab [forester::Liziniewicz_2016_height_trajectory_Sweden_Pinus_contorta()]\cr
#' Larch\tab [forester::Johansson_2013_height_trajectory_Sweden_Larch()]\cr
#' }
#' @param H50 Site Index H50, see parameter H100
#'
#' @details Table of fit-statistics for basal area functions:
#' \tabular{lrrr}{
#' Species\tab RMSE (\%)\tab MD (\%)\tab n\cr
#' Scots Pine\tab 1.09(3.39)\tab -0.15(-0.47)\tab 3447\cr
#' Norway Spruce\tab 0.97(2.14)\tab -0.10(-0.22)\tab 984\cr
#' Lodgepole pine\tab 1.22(3.29)\tab -0.01(-0.03)\tab 237\cr
#' Larch\tab 2.79(5.78)\tab -0.29(-0.59)\tab 78\cr
#' }
#'
#' @name Mensah_2022_BA
#'
#' @return Expected BA m2 / ha at age2.
#' @export

Mensah_2022_BA_Sweden_Scots_Pine <- function(
    BA,
    age,
    age2,
    stems,
    H100){

  if(age<40){warning("For stable estimates age should be above 40!")}

  #F01
  a0 <- -52.8873
  a1 <- 0.0064
  a2 <- -0.007
  stems <- sqrt(10000/stems)
  return(
  exp(log(BA)+ a0*((1/age2)-(1/age))+a1*stems+a2*H100)
  )
}
Mensah_2022_BA_Sweden_Scots_Pine <- Vectorize(Mensah_2022_BA_Sweden_Scots_Pine)


#' @export
#' @rdname Mensah_2022_BA
Mensah_2022_BA_Sweden_Norway_Spruce <- function(
    BA,
    age,
    age2,
    stems,
    H100){

  if(age<40){warning("For stable estimates age should be above 40!")}
  #F01
  a0 <- -55.1743
  a1 <- 0.0273
  a2 <- -0.0012
  stems <- sqrt(10000/stems)
  return(
  exp(log(BA)+ a0*((1/age2)-(1/age))+a1*stems+a2*H100)
  )
}
Mensah_2022_BA_Sweden_Norway_Spruce <- Vectorize(Mensah_2022_BA_Sweden_Norway_Spruce)

#' @export
#' @rdname Mensah_2022_BA
Mensah_2022_BA_Sweden_Lodgepole_Pine <- function(
    BA,
    age,
    age2,
    stems,
    H50){

  if(age!=30){warning("age should be 30 for stable estimates!")}
  #F04
  a0 <- 0.0507
  a1 <- 1.0931
  a2 <- -0.1089
  c0 <- 2.6904
  stems <- sqrt(10000/stems)
  return(
    # minus a0?
  BA*((1-exp(-a0*age2))/(1-exp(-a0*(age))))^(c0 + a1*stems + a2*H50)
  )
}
Mensah_2022_BA_Sweden_Lodgepole_Pine <- Vectorize(Mensah_2022_BA_Sweden_Lodgepole_Pine)

#' @export
#' @rdname Mensah_2022_BA
Mensah_2022_BA_Sweden_Larch <- function(
    BA,
    age,
    age2,
    stems,
    H100){
  #F04
  if(age!=30){warning("age should be 30 for stable estimates!")}

  a0 <- 0.0287
  a1 <- 0.7837
  a2 <- 0.5909
  c0 <- 0.0887
  stems <- sqrt(10000/stems)
  return(
    #minus a0?
  BA*((1-exp(-a0*age2))/(1-exp(-a0*(age))))^(c0 + a1*stems + a2*H100)
  )
}
Mensah_2022_BA_Sweden_Larch <- Vectorize(Mensah_2022_BA_Sweden_Larch)





#' Relation between yield capacity MAI max m3/ha/yr and Site Index.
#'
#' @source Mensah, A. A., Holmström, E., Nyström, K., Nilsson, U. 2022.
#' Modelling potential yield capacity in conifers using Swedish long-term
#' experiments. For. Ecol. Manage. 512. Available online (2022-04-08):
#' \url{https://doi.org/10.1016/j.foreco.2022.120162}
#'
#' @param H100 Site Index H100 e.g. \cr
#' \tabular{ll}{
#' Scots Pine\tab [forester::Elfving_Kiviste_1997_height_trajectory_Sweden_Pine()]\cr
#' Norway Spruce\tab [forester::Elfving_2003_height_trajectory_Sweden_Spruce()]\cr
#' Lodgepole Pine\tab [forester::Liziniewicz_2016_height_trajectory_Sweden_Pinus_contorta()]\cr
#' Larch\tab [forester::Johansson_2013_height_trajectory_Sweden_Larch()]\cr
#' }
#' @param H50 Site Index H50, see parameter SI100
#'
#' @details Fit-statistics \cr
#' \tabular{lrr}{
#' Species\tab RMSE(rel,\%)\tab R^2\cr
#' Scots Pine\tab 0.9757 (16.73)\tab 0.809\cr
#' Norway Spruce\tab 1.047 (11.14)\tab 0.904\cr
#' Lodgepole Pine\tab 0.436 (5.52)\tab 0.956\cr
#' Larch\tab 1.164 (10.82)\tab 0.876\cr
#' }
#'
#' @return Maximum Annual Increment (m^3 ha^{-1} yr^{-1})
#' @export
#'
#' @name Mensah_2022_MAI
Mensah_2022_MAI_max_Sweden_Scots_Pine <- function(H100){
  a0 <- 0.4613
  a1 <- 0.1006
  return(
    a0*exp(a1*H100)
  )
}

#' @export
#' @rdname Mensah_2022_MAI
Mensah_2022_MAI_max_Sweden_Norway_Spruce <- function(H100){
  a0 <- 0.5558
  a1 <- 0.0929
  return(
    a0*exp(a1*H100)
  )
}

#' @export
#' @rdname Mensah_2022_MAI
Mensah_2022_MAI_max_Sweden_Lodgepole_Pine <- function(H50){
  a0 <- 0.8291
  a1 <- 0.1038
  return(
    a0*exp(a1*H50)
  )
}

#' @export
#' @rdname Mensah_2022_MAI
Mensah_2022_MAI_max_Sweden_Larch <- function(H100){
  a0 <- 0.9583
  a1 <- 0.0695
  return(
    a0*exp(a1*H100)
  )
}



#' Species-specific volume (m^3 ha^{-1}) yield functions.
#'
#' @source Mensah, A. A., Holmström, E., Nyström, K., Nilsson, U. 2022.
#' Modelling potential yield capacity in conifers using Swedish long-term
#' experiments. For. Ecol. Manage. 512. Available online (2022-04-08):
#' \url{https://doi.org/10.1016/j.foreco.2022.120162}
#'
#' @param dominant_height \cr
#' \tabular{ll}{
#' Scots Pine\tab [forester::Elfving_Kiviste_1997_height_trajectory_Sweden_Pine()]\cr
#' Norway Spruce\tab [forester::Elfving_2003_height_trajectory_Sweden_Spruce()]\cr
#' Lodgepole Pine\tab [forester::Liziniewicz_2016_height_trajectory_Sweden_Pinus_contorta()]\cr
#' Larch\tab [forester::Johansson_2013_height_trajectory_Sweden_Larch()]\cr
#' }
#' @param BA e.g. [forester::Mensah_2022_BA_Sweden_Scots_Pine]
#'
#' @details \cr
#' \tabular{lrrr}{
#' Species\tab RMSE(\%)\tab MD(\%)\tab n\cr
#' Scots Pine\tab 24.27(11.74) \tab -0.1584(-0.0765)\tab 4405 \cr
#' Norway Spruce\tab 36.98(11.75) \tab -0.1629(-0.0517)\tab 1789 \cr
#' Lodgepole Pine\tab 22.44(10.96) \tab -0.1238(-0.0626)\tab 725 \cr
#' Larch\tab 54.18(17.72) \tab 1.1717(0.3834)\tab 453 \cr
#' }
#'
#' @return Volume Yield m^3 ha^-1
#' @export
#' @name Mensah_2022_volume

Mensah_2022_volume_yield_m3_ha_Sweden_Scots_Pine <- function(
  dominant_height,
  BA
  ){
  a0 <- 0.8013
  a1 <- 1.0208
  a2 <- 0.8062
  return(
    a0*dominant_height^a1*BA^a2
  )
}

#' @export
#' @rdname Mensah_2022_volume
Mensah_2022_volume_yield_m3_ha_Sweden_Norway_Spruce <- function(
    dominant_height,
    BA
    ){
  a0 <- 0.7498
  a1 <- 1.1781
  a2 <- 0.7169
  return(
    a0*dominant_height^a1*BA^a2
  )
}

#' @export
#' @rdname Mensah_2022_volume
Mensah_2022_volume_yield_m3_ha_Sweden_Lodgepole_Pine <- function(
    dominant_height,
    BA
    ){
  a0 <- 0.8783
  a1 <- 0.9251
  a2 <- 0.8839
  return(
    a0*dominant_height^a1*BA^a2
  )
}

#' @export
#' @rdname Mensah_2022_volume
Mensah_2022_volume_yield_m3_ha_Sweden_Larch <- function(
    dominant_height,
    BA
    ){
  a0 <- 1.4005
  a1 <- 1.6359
  a2 <- 0.1558
  return(
    a0*dominant_height^a1*BA^a2
  )
}


#' Helper functions for Mensah 2022 initial stand density.
#'
#' @source Mensah, A. A., Holmström, E., Nyström, K., Nilsson, U. 2022.
#' Modelling potential yield capacity in conifers using Swedish long-term
#' experiments. For. Ecol. Manage. 512. Available online (2022-04-08):
#' \url{https://doi.org/10.1016/j.foreco.2022.120162}
#' (Unpublished supplement, in print 2022-04-14)
#'
#' @param H100 Site Index H100
#' @param H50 Site Index H50
#' @param age Total stand age
#'
#' @details
#' \tabular{lrr}{
#' Species \tab RMSE m^2/ha (%) \cr
#' Scots Pine \tab 3.231 (11.14) \cr
#' Norway Spruce \tab 3.403 (9.72) \cr
#' Lodgepole Pine \tab 4.534 (15.63) \cr
#' Larch \tab 4.601 (12.78) \cr
#' }
#'
#'
#' @return Initial basal area.
#' @export
#' @name Mensah_initial_BA

Mensah_2022_initial_BA_Sweden_Scots_Pine <- function(H100,age){
  a0 <- 147.4
  a1 <- 11.63
  a2 <- 0.00594
  a3 <- 1.063
  return(
    (a0 + a1*H100)*(1-exp(-a2*age))^a3
  )
}

#'@export
#'@rdname Mensah_initial_BA
Mensah_2022_initial_BA_Sweden_Norway_Spruce<- function(H100,age){
  a0 <- -169.5
  a1 <- 11.23
  a2 <- 0.0098
  a3 <- 1.335
  return(
  (a0 + a1*H100)*(1-exp(-a2*age))^a3
  )
}

#'@export
#'@rdname Mensah_initial_BA
Mensah_2022_initial_BA_Sweden_Lodgepole_Pine<- function(H50,age){
  a0 <- -313.6
  a1 <- 49.86
  a2 <- 0.0014
  a3 <- 1.030
  return(
  (a0 + a1*H100)*(1-exp(-a2*age))^a3
  )
}

#'@export
#'@rdname Mensah_initial_BA
Mensah_2022_initial_BA_Sweden_Larch<- function(H100,age){
  a0 <- -283.7
  a1 <- 30.22
  a2 <- 0.00055 #typo?
  a3 <- 0.732
  return(
  (a0 + a1*H100)*(1-exp(-a2*age))^a3
  )
}
