% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odin_tsum_1983.R
\name{odin_tsum_1983}
\alias{odin_tsum_1983}
\title{odin_tsum_1983}
\usage{
odin_tsum_1983(lat_degree, lat_min = 0, lat_sec = 0, masl)
}
\arguments{
\item{masl}{Metres above sea level}

\item{lat}{Latitude in decimal degrees.}
}
\value{
Returns the estimated annual temperature sum above 5 degrees celsius.
}
\description{
Function 7 from Odin, Eriksson & Perttu (1983) Temperature Climate Maps for Swedish Forestry". Reports in Forest Ecology and Forest Soils 45. p. 45.

 \emph{Tsum = 4835 - 57,6 x LAT - 0,9 x MASL}
}
\examples{
odin_tsum_1983(lat=68.88, masl=38)
}
