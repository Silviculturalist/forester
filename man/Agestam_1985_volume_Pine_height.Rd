% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Agestam_1985.R
\name{Agestam_1985_volume_Pine_height}
\alias{Agestam_1985_volume_Pine_height}
\title{Volume function for Pine from Agestam (1985), including height.}
\source{
Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
}
\usage{
Agestam_1985_volume_Pine_height(
  basal_area_Pine_m2_ha,
  dominant_height_Pine_m,
  stems_Pine_ha,
  latitude
)
}
\arguments{
\item{basal_area_Pine_m2_ha}{Basal area over bark Pine, m2/ha.}

\item{dominant_height_Pine_m}{Dominant height of Pine stems.}

\item{stems_Pine_ha}{Number of Pine stems per hectare.}

\item{latitude}{Latitude, degrees.}
}
\value{
Volume Pine m3sk/ha.
}
\description{
Volume function for Pine from Agestam (1985), including height.
}
\details{
Function: T1

Number of plots: 37

Multiple correlation coefficient: 0.997

Standard deviation about the function (sf): 0.031

sf/standard deviation about the mean: 6.8\%
}
