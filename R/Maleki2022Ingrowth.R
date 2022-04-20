#' Ingrowth for the main tree species groups of Norway.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param BA Basal Area m^2 / ha.
#' @param QMD Quadratic Mean Diameter (cm).
#' @param stems Total stems per hectare.
#'
#' @family {Maleki2022}
#'
#' @details
#' \tabular{lrrrr}{
#' Total stem density predicted by surviving + ingrowth:\tab\tab\tab\tab\cr
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 2.319 \tab 76.001 \tab 114.614 \tab 0.971 \cr
#' Scots Pine \tab 1.556  \tab 43.963 \tab 69.482 \tab 0.980 \cr
#' Broadleaves \tab 2.32 \tab 98.054 \tab 143.261 \tab 0.963 \cr
#' }
#'
#' @return Probability and count of ingrowth
#' @export
#' @name Maleki2022ingrowth
Maleki_2022_ingrowth_count_Norway_Norway_Spruce <- function(BA){
  return(
    2.2919-0.3780*sqrt(BA)
  )
}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_count_Norway_Scots_Pine <- function(BA){
  return(
    0.5264-0.0889*sqrt(BA)
  )
}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_count_Norway_Norway_Spruce <- function(QMD){
  return(
    3.0411 - 0.6213*sqrt(QMD)
  )
}



#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_probability_Norway_Norway_Spruce <- function(QMD, stems){
  return(
    13.6210 - 3.0328*sqrt(QMD) - 0.6868*sqrt(stems) + 0.1483*sqrt(QMD)*sqrt(stems)
  )

}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_probability_Norway_Scots_Pine <- function(QMD, stems){
  return(
    7.6489 - 1.2823*sqrt(QMD) - 0.5173*sqrt(stems) + 0.0915*sqrt(QMD)*sqrt(stems)
  )
}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_probability_Norway_Broadleaves <- function(QMD, stems){
  return(
    7.7009 - 1.6373*sqrt(QMD) - 0.3349*sqrt(stems) + 0.5688*sqrt(QMD)*sqrt(stems)
  )
}
