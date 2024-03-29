% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gustavsen_1980_height_trajectories.R
\name{Gustavsen_1980_time_to_breast_height}
\alias{Gustavsen_1980_time_to_breast_height}
\title{Smoothing of time required to reach breast height according to Gustavsen 1980.}
\usage{
Gustavsen_1980_time_to_breast_height(SI100, species = "Picea abies")
}
\arguments{
\item{SI100}{Site index 100, according to Gustavsen 1980.}

\item{species}{One of 'Picea abies' (default) or 'Pinus sylvestris'.}
}
\value{
Number of years required to reach breast height 1.3m.
}
\description{
Smoothing of time required to reach breast height according to Gustavsen 1980.
}
\details{
Both species modelled with log-linear function.
}
\author{
Carl Vigren SLU
}
