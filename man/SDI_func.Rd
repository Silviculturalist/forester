% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SDI_func.R
\name{SDI_func}
\alias{SDI_func}
\title{SDI function}
\usage{
SDI_func(slope, SDI)
}
\arguments{
\item{slope}{A species specific slope for the linear relation between the number of stems and mean diameter on a log-log scale.}

\item{SDI}{Stem Density index to give function for.}
}
\value{
returns the equation for the SDI. \emph{as a formula}.
}
\description{
SDI function
}
\examples{

SDI_func(slope=-1.605, SDI = 1000)


}
