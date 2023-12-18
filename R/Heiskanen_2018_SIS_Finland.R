#' Correlation between H100 and site factors in Finland
#'
#' @source Heiskanen J., Hallikainen V., Uusitalo J., Ilvesniemi H. (2018).
#' Co-variation relations of physical soil properties and site characteristics
#' of Finnish upland forests. Silva Fennica vol. 52 no. 3 article id 9948.
#' \url{https://doi.org/10.14214/sf.9948}
#'
#' @details Multiple R^2 = 0.829. adj.R^2 = 0.813. Residual SE= 2.75.
#'
#' @description OBSERVE: H100 is *tree-species independent*.
#' Will assume that site type category 1 & 2 are aggregated & that
#' site type 6 (Barren heath forests) have equivalent H100 to site type 5.
#'
#'
#' @param site_type Numeric. 1 (Default): Herb-rich forests (groves), 2: herb-rich heath forests
#' (OMT). 3: mesic heath forests (MT). 4. sub-xeric heath forests (VT). 5. xeric
#' heath forests (CT). 6. Barren heath forests. (ClT). [forester::Finland_vegetation_types()]
#' @param fine_fraction_percentage Fine fraction (<0.063 mm, %). Min (2.9). Max (92.57). Mean (32.74). Median (30.98).
#' @param temperature_sum Degree days, at 2m, threshold 5 degrees Celsius. Min (807). Max (1746). Mean (1383.91). Median (1425.5).
#'
#' @md
#' @return H100 Estimate. (tree-species independent).
#' @export
Heiskanen_2018_SIS_Finland <- function(
  site_type=1,
  temperature_sum=1425.5,
  fine_fraction_percentage=32.74
){

  stopifnot(site_type%in%seq(1,6,1))
  stopifnot(fine_fraction_percentage>0 && fine_fraction_percentage<=100)

  mesic <- ifelse(site_type==3,1,0)
  subxeric <- ifelse(site_type==4,1,0)
  xeric <- ifelse(site_type%in%c(5,6),1,0)


  return(
    0.848+
      -5.96*mesic+
      -4.409*subxeric+
      -6.962*xeric+
      +0.017*temperature_sum+
      +0.071*fine_fraction_percentage
  )
}
