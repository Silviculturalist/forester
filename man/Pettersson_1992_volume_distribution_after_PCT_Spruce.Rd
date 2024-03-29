% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pettersson_1992.R
\name{Pettersson_1992_volume_distribution_after_PCT_Spruce}
\alias{Pettersson_1992_volume_distribution_after_PCT_Spruce}
\title{Mean of Volume distribution for stands after PCT of Norway Spruce from Pettersson 1992}
\source{
Pettersson Nils (1992) The effect on stand development of different
spacing after planting and precommercial thinning in Norway Spruce (Picea abies
 (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
 after precommercial thinning on volume and structure in Pinus sylvestris and
 Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
 ISSN 0348-7636. p. 9.

 @description

"A Gram-Charlier-series (Kendall et al. 1987), was used by Eriksson (1976) for
 the description of the frequency of stems and volume by diameter classes and by
 Pettersson (1992) for the description of the volume distribution by diameter classes.
 For the application on the distributions in my material the function has the following form:

\deqn{
y = \frac{V}{\sigma} [ \varphi (\frac{x-m}{\sigma}) - \frac{\lambda_1}{6}\varphi^3(\frac{x-m}{\sigma})+\frac{\lambda_2}{24}\varphi^4(\frac{x-m}{\sigma})+\frac{\lambda_1^2}{720}\varphi^6(\frac{x-m}{\sigma}  ]
}

where \eqn{\varphi} = the normal frequency distribution, \eqn{\varphi^{v}}= the v:th derivate of \eqn{\varphi}, \eqn{\lambda_{1}} = the skewness of the distribution, \eqn{\lambda_{2}}=the kurtosis of the distribution, V= total volume in the distribution, y=frequency of volume for the actual class, x= middle of the class, m=mean of the distribution, and \eqn{\sigma} = standard deviation of the distribution."


 @details

 Mean :
 F= 1426
 R^2 = 0.94

 Standard deviation:
 F=45
 R^2 =  0.49

 Skew + 3
 F= 7
 R^2 = 0.16

 Kurtosis + 3
 F = 5
 R^2  = 0.10
}
\usage{
Pettersson_1992_volume_distribution_after_PCT_Spruce(
  diameter_mean_basal_area_stem,
  stems,
  dominant_height,
  SI
)
}
\arguments{
\item{diameter_mean_basal_area_stem}{Diameter corresponding the mean basal area stem, cm.}

\item{stems}{Stems per hectare}

\item{dominant_height}{Dominant height, metres.}

\item{SI}{Site Index, m, according to Hägglund 1972, 1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce],[forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]}
}
\value{
Volume distribution for stands of Norway Spruce after PCT
}
\description{
Mean of Volume distribution for stands after PCT of Norway Spruce from Pettersson 1992
}
