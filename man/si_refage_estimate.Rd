% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/si_refage_estimate.R
\name{si_refage_estimate}
\alias{si_refage_estimate}
\title{Site Index Estimations for Sweden}
\usage{
si_refage_estimate(
  species = c("NorwaySpruce", "ScotsPine", "Larch", "Beech", "Oak", "Birch",
    "AlnusIncana", "AlnusGlutinosa", "Aspen"),
  age,
  height,
  refAge = 100
)
}
\arguments{
\item{species}{One of "NorwaySpruce", "ScotsPine", "Larch", "Beech","Oak","Birch","AlnusIncana", "AlnusGlutinosa", "Aspen".}

\item{age}{Average total age in years. For birch age at breast height.}

\item{height}{Average Height in metres of dominant trees.}

\item{refAge}{Forecasted age. OBS, refAge cannot be set for Alnus spp., Aspen or Betula (==50)}

\item{data}{Tidy data frame.}

\item{outputcolumn}{Name of column to create for output.}

\item{...}{}
}
\value{
data frame with an additional column with the estimate of the height of the tree at age 100.
}
\description{
Provides interface for the site index functions from "\emph{Nya höjdutvecklingskurvor för bonitering}" (2013),
by Johansson, U., Ekö, P-M, Elfving, B., Johansson, T. and Nilsson, U.

    \strong{Reference ages by species}: Picea abies (100);Pinus sylvestris (100); Larix (100); Fagus sylvatica (100); Quercus (100); Betula (50); Alnus (50); Populus (50).

    \strong{Suitable ages for use by species}: Picea abies (10-80); Pinus sylvestris (10-80); Larix (10-100); Fagus sylvatica (20-150); Quercus (20-150);  Betula (10-90); Alnus glutinosa (<100); Alnus incana (<70); Populus tremula x tremuloides (<50); Populus tremula (<50-60)
}
\examples{
si_refage_estimate(species="NorwaySpruce",age=30, refAge=100, height=6.2)
}
