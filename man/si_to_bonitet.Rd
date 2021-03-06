% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/si_to_bonitet.R
\name{si_to_bonitet}
\alias{si_to_bonitet}
\title{si_to_bonitet}
\usage{
si_to_bonitet(H100, fun)
}
\arguments{
\item{H100}{Estimated stand top height at 100 years age.}

\item{fun}{One of the supplied functions, see details.}
}
\value{
Estimates the mean volume growth in m3sk / ha yr-1 at the time of culmination (or if late culmination, at 150yrs of age)
}
\description{
Interface for Hägglund (1981) "Samband mellan ståndortsindex H100 och bonitet för tall och gran i Sverige", Report #26, Projekt Hugin.
}
\details{
Productive potential as a function of site index

 a - Spruce, southern Sweden.

 b - Spruce, middle Sweden, herb types, grass types and ground without field layer.

 c - Spruce, middle Sweden, bilberry type and worse.

 d - Spruce, northern Sweden, herb types, grass types and ground without field layer.

 e - Spruce, northern Sweden, bilberry type and worse.

 f - Pine, southern and middle Sweden as well as lowlands of northern Sweden.

 g - Pine, northern Sweden, more than 200 meters above sea level.
}
\examples{
si_to_bonitet(H100=14, function="a")
}
