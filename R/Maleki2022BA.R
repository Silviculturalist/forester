#' Basal Area development for main species groups in Norway.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param BA Basal Area, m^2 /ha.
#' @param age Total stand age.
#' @param age2 Desired total stand age.
#' @param stems Stems at age.
#' @param stems2 Stems at age2.
#' @param SI40 Site Index H40 (see ?forester::Maleki2022height)
#'
#' @family {Maleki2022}
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab -0.067 \tab 1.112 \tab 1.602 \tab 0.984 \cr
#' Scots Pine \tab 0.032  \tab 0.680 \tab 1.007 \tab 0.991 \cr
#' Broadleaves \tab -0.138 \tab 0.978 \tab 1.418 \tab 0.978 \cr
#' }
#'
#'
#' @return Basal Area at age2.
#' @name Maleki2022BA
#' @export
Maleki_2022_BA_m2_ha_Norway_Norway_Spruce <- function(BA,age,age2,SI40,stems,stems2){

  b1 <- 0.4159
  b2 <- 2.0096
  b3 <- 0.7521

  #Calculate dominant-height-at-age.
  H1 <- Maleki_2022_height_trajectory_Norway_Norway_Spruce(dominant_height = SI40,age = 40,age2 = age)
  H2 <- Maleki_2022_height_trajectory_Norway_Norway_Spruce(dominant_height = SI40,age = 40,age2 = age2)

  return(
    BA^((H1/H2)^b1) * exp((b2^(stems2/stems)) * b2*(1-(H1/H2)^b3))
  )
}

#' @rdname Maleki2022BA
#' @export
Maleki_2022_BA_m2_ha_Norway_Scots_Pine <- function(BA,age,age2,SI40,stems,stems2){

  b1 <- 0.5381
  b2 <- 0.96900
  b3 <- 4.1579

  #Calculate dominant-height-at-age.
  H1 <- Maleki_2022_height_trajectory_Norway_Scots_Pine(dominant_height = SI40,age = 40,age2 = age)
  H2 <- Maleki_2022_height_trajectory_Norway_Scots_Pine(dominant_height = SI40,age = 40,age2 = age2)

  return(
    BA^((H1/H2)^b1) * exp((b2^(stems2/stems)) * b2*(1-(H1/H2)^b3))
  )
}

#' @rdname Maleki2022BA
#' @export
Maleki_2022_BA_m2_ha_Norway_Broadleaves <- function(BA,age,age2,SI40,stems,stems2){

  b1 <- 0.2970
  b2 <- 3.6124
  b3 <- 0.2087

  #Calculate dominant-height-at-age.
  H1 <- Maleki_2022_height_trajectory_Norway_Broadleaves(dominant_height = SI40,age = 40,age2 = age)
  H2 <- Maleki_2022_height_trajectory_Norway_Broadleaves(dominant_height = SI40,age = 40,age2 = age2)

  return(
    BA^((H1/H2)^b1) * exp((b2^(stems2/stems)) * b2*(1-(H1/H2)^b3))
  )
}
