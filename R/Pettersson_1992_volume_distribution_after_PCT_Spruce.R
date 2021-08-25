#' Mean of Volume distribution for stands after PCT of Norway Spruce from Pettersson 1992
#'
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @description
#'
#' "A Gram-Charlier-series (Kendall et al. 1987), was used by Eriksson (1976) for
#'  the description of the frequency of stems and volume by diameter classes and by
#'  Pettersson (1992) for the description of the volume distribution by diameter classes.
#'  For the application on the distributions in my material the function has the following form:
#'
#' \deqn{
#' y = \frac{V}{\sigma} [ \varphi (\frac{x-m}{\sigma}) - \frac{\lambda_1}{6}\varphi^3(\frac{x-m}{\sigma})+\frac{\lambda_2}{24}\varphi^4(\frac{x-m}{\sigma})+\frac{\lambda_1^2}{720}\varphi^6(\frac{x-m}{\sigma}  ]
#' }
#'
#' where \eqn{\varphi} = the normal frequency distribution, \eqn{\varphi^{v}}= the v:th derivate of \eqn{\varphi}, \eqn{\lambda_{1}} = the skewness of the distribution, \eqn{\lambda_{2}}=the kurtosis of the distribution, V= total volume in the distribution, y=frequency of volume for the actual class, x= middle of the class, m=mean of the distribution, and \eqn{\sigma} = standard deviation of the distribution."
#'
#'
#'  @details
#'
#'  Mean :
#'  F= 1426
#'  R^2 = 0.94
#'
#'  Standard deviation:
#'  F=45
#'  R^2 =  0.49
#'
#'  Skew + 3
#'  F= 7
#'  R^2 = 0.16
#'
#'  Kurtosis + 3
#'  F = 5
#'  R^2  = 0.10
#'
#' @param diameter_mean_basal_area_stem Diameter corresponding the mean basal area stem, cm.
#' @param stems Stems per hectare
#' @param dominant_height Dominant height, metres.
#' @param SI Site Index, m, according to Hägglund 1972, 1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce],[forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Volume distribution for stands of Norway Spruce after PCT
#' @export
#'
#' @examples
Pettersson_1992_volume_distribution_after_PCT_Spruce <- function(
  diameter_mean_basal_area_stem,
  stems,
  dominant_height,
  SI
){

  mean <- exp(0.503 + 0.839*log(diameter_mean_basal_area_stem))
  standard_deviation <- exp(-2.361 + 0.956*log(dominant_height)+0.122*log(stems))
  skew_plus_3 <- exp(2.397 + 0.158*log(dominant_height) + 0.121*log(stems)) -0.774*log(SI)
  kurtosis_plus_3 <- exp(-0.306 -0.157*log(stems) + 0.783*log(SI))

  return(
    list(
      "Mean Volume"=mean,
      "Volume Std. Deviation"=standard_deviation,
      "Volume skew + 3"=skew_plus_3,
      "Volume kurtosis + 3"=kurtosis_plus_3
    )
  )

}
