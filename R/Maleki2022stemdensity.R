#' Number of surviving stems at the end of a period.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param age Total stand age.
#' @param age2 Total stand age at time 2.
#' @param stems Stand stem density per hectare.
#' @param SI40 Site Index H40 (see ?forester::Maleki2022height)
#'
#' @family {Maleki2022}
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 2.626 \tab 48.467 \tab 71.777 \tab 0.988 \cr
#' Scots Pine \tab 1.561  \tab 26.192 \tab 37.531 \tab 0.994 \cr
#' Broadleaves \tab 2.117 \tab 57.370 \tab 82.531 \tab 0.986 \cr
#' }
#'
#' @return The number of survived trees at the end of the period
#' @export
#' @name Maleki2022stems

Maleki_2022_stems_surviving_Norway_Norway_Spruce <- function(age,age2,stems,SI40){

  b1 <- 0.6159
  b2 <- -0.0312
  b3 <- 1.0602

  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems
Maleki_2022_stems_surviving_Norway_Scots_Pine <- function(age,age2,stems,SI40){

  b1 <- 0.17881
  b2 <- -0.0308
  b3 <- 0.0695
  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems
Maleki_2022_stems_surviving_Norway_Broadleaves <- function(age,age2,stems,SI40){

  b1 <- 0.4592
  b2 <- -0.0534
  b3 <- 0.9466

  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}


#' Stems at age2
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param age Total age of stand.
#' @param age2 Total age of stand desired.
#' @param stems Stems at age.
#' @param SI40 Site Index H40 (see ?forester::Maleki2022height)
#'
#' @return Number of stems at age2.
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 10.928 \tab 75.122 \tab 115.755 \tab 0.970 \cr
#' Scots Pine \tab 4.924  \tab 41.354 \tab 69.141 \tab 0.981 \cr
#' Broadleaves \tab 12.065 \tab 94.132 \tab 142.359 \tab 0.963 \cr
#' }
#'
#' @export
#' @name Maleki2022stems2
#' @family {Maleki2022}
Maleki_2022_stem_density_Norway_Norway_Spruce <- function(age,age2,stems,SI40){

  b1 <- 1.5124
  b2 <- -0.00654
  b3 <- 1.4747


  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems2
Maleki_2022_stem_density_Norway_Scots_Pine <- function(age,age2,stems,SI40){

  b1 <- 0.6676
  b2 <- 0.0039 #Not negative?
  b3 <- 0.8662


  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems2
Maleki_2022_stem_density_Norway_Broadleaves <- function(age,age2,stems,SI40){

  b1 <- 1.1395
  b2 <- -0.0162
  b3 <- 1.2682


  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}
