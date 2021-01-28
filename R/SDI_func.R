#' SDI function
#'
#'@param slope A species specific slope for the linear relation between the number of stems and mean diameter on a log-log scale.
#'@param SDI Stem Density index to give function for.
#'
#'@return returns the equation for the SDI. \emph{as a formula}.
#'@export
#'@examples
#'
#'SDI_func(slope=-1.605, SDI = 1000)
#'
#'
SDI_func <- function(slope, SDI){
  slope %>%
    assert_is_numeric() %>%
   is_negative()
  SDI %>%
    assert_is_numeric() %>%
    is_positive()

  as.formula(paste("~",(10^((slope*-1)+log10(SDI))), "*.x^(",slope,")"))
}
