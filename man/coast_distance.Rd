% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coast_distance.R
\name{coast_distance}
\alias{coast_distance}
\title{Distance to Swedish Coast}
\usage{
coast_distance(latitude, longitude, epsg = 4326)
}
\arguments{
\item{latitude}{A decimal latitude.}

\item{longitude}{A decimal longitude.}

\item{epsg}{The EPSG code of the projection, e.g. for WGS84: '4326' (default). Or for SWEREF99TM: '3006'.}
}
\value{
Distance to coast, km.
}
\description{
Wrapper for sf::st_distance to measure distance between input coordinates and Natural Earth Coastline rnaturalearth::ne_coastline of medium resolution.
The world coastline set is very coarsely cut to include mainly Swedish coast.
}
\examples{
coast_distance(56.8,14.8)
}
