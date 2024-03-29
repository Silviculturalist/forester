% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Braastad_1974.R
\name{Braastad_1974_initial_Basal_area_Norway_Spruce}
\alias{Braastad_1974_initial_Basal_area_Norway_Spruce}
\title{Calculate initial Basal Area for stands of Norway Spruce in Norway from Braastad 1974.}
\source{
Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 369. function (V.1)
}
\usage{
Braastad_1974_initial_Basal_area_Norway_Spruce(
  SIH40,
  stems_per_ha,
  dominant_height
)
}
\arguments{
\item{SIH40}{Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.}

\item{stems_per_ha}{Stems per hectare.}

\item{dominant_height}{Dominant height.}
}
\value{
Basal Area m2 / ha.
}
\description{
Calculate initial Basal Area for stands of Norway Spruce in Norway from Braastad 1974.
}
\details{
Multiple correlation coefficient: 0.863.

Residual spread : 3.2 m^2.

Spread about the mean: 16.7 percent of mean Basal area.
}
