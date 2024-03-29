% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Agestam_1985.R
\name{Agestam_1985_volume_Birch_SI}
\alias{Agestam_1985_volume_Birch_SI}
\title{Volume function for Birch from Agestam (1985), including SI.}
\source{
Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
}
\usage{
Agestam_1985_volume_Birch_SI(
  basal_area_Birch_m2_ha,
  age_at_breast_height_Birch,
  SI_Birch_m,
  stems_ha_Birch,
  latitude
)
}
\arguments{
\item{basal_area_Birch_m2_ha}{Basal area over bark Pine, m2/ha.}

\item{age_at_breast_height_Birch}{Age at breast height for Birch stems.}

\item{SI_Birch_m}{Site Index for Birch according to Persson (1959)}

\item{stems_ha_Birch}{Number of Birch stems per hectare.}

\item{latitude}{Latitude, degrees.}
}
\value{
Volume Birch m3sk/ha.
}
\description{
Volume function for Birch from Agestam (1985), including SI.
}
\details{
Function: B2

Number of plots: 47

Multiple correlation coefficient: 0.997

Standard deviation about the function (sf): 0.078

sf/standard deviation about the mean: 8.2\%
}
