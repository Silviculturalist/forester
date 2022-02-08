#' Reduction in number of trees and basal area in even-aged stands of Scots Pine in Norway
#'
#' @source Kuehne C., McLean J.P., Maleki K., Antón-Fernández C., Astrup R.
#' (2022). A stand-level growth and yield model for thinned and unthinned
#' even-aged Scots pine forests in Norway. Silva Fennica vol. 56 no. 1 article
#' id 10627. \url{https://doi.org/10.14214/sf.10627}
#'
#' @description Mean error: -0.00041, Mean absolute error: 0.03386, relative MAE: 5.17959
#'
#'
#' @param BA_before Basal area before thinning, m2 / ha
#' @param BA_after Basal area after thinning, m2 / ha
#'
#' @return Quotient
#' @export
#' @name Kuehne_quotient
#'
Kuehne_2022_stems_quotient_Norway_Scots_Pine <- function(
  BA_before,
  BA_after
){
  exp(-1.91239 + 1.94414*(BA_after/BA_before))
}


#' @rdname Kuehne_quotient
#' @export
#' @param stems_after Number of stems per ha after thinning.
#' @param stems_before Number of stems per ha before thinning
Kuehne_2022_BA_quotient_Norway_Scots_Pine <- function(
  stems_after,
  stems_before
){
  (log((stems_after/stems_before))--1.91239)/1.94414
}
