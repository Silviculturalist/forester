% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hagglund_1976_intercept_SI.R
\name{Hagglund_1976_intercept_SI}
\alias{Hagglund_1976_intercept_SI}
\title{Find Site Index H100 for Norway Spruce or Scots Pine in Sweden by Intercept method}
\source{
Hägglund, B. (1976) "Skattning av höjdboniteten i unga tall- och granbestånd:
Estimating site index in young stands of Scots pine and Norway spruce in Sweden. Report #39. Royal College of Forestry. Stockholm.
p.36
}
\usage{
Hagglund_1976_intercept_SI(
  species,
  intercept_2.5_5.m,
  vegetation,
  lateral_water,
  latitude,
  altitude
)
}
\arguments{
\item{species}{"Picea abies" or "Pinus sylvestris"}

\item{intercept_2.5_5.m}{Measured distance, in metres. from the first branch whorl above 2.5 metres height to the fifth branch whorl above (height increment from five growing seasons)}

\item{vegetation}{Variable indicating vegetation type (NFI vegetation code) scaled from -5 to +4 as follows:

\tabular{llr}{
Field Layer Code (NFI) \tab Description \tab Index \cr
1\tab Tall herbs w/o dwarf shrubs \tab 4 \cr
2\tab Tall herbs with bilberry \tab 2.5 \cr
3\tab Tall herbs with cowberry \tab 2 \cr
4\tab Low herbs w/o dwarf shrubs \tab 3 \cr
5\tab Low herbs with bilberry \tab 2.5 \cr
6\tab Low herbs with cowberry \tab 2 \cr
7\tab No field layer \tab 3 \cr
8\tab broad-leafed grasses \tab 2.5 \cr
9\tab narrow-leaved grasses \tab 1.5 \cr
10\tab Sedge, tall \tab -3 \cr
11\tab Sedge, low \tab -3 \cr
12\tab Horsetail \tab 1 \cr
13\tab Bilberry \tab 0 \cr
14\tab Cowberry \tab -0.5 \cr
15\tab Crowberry \tab -3 \cr
16\tab Poor shrub \tab -5 \cr
17\tab Lichen-rich \tab -0.5 \cr
18\tab Lichen-dominated \tab -1 \cr
}}

\item{lateral_water}{Type 1="Missing", 2="Seldom",3="shorter periods",4="longer periods",5="slope".}

\item{latitude}{Degrees. Only required when species is "Picea abies".}

\item{altitude}{Metres above sea level.}
}
\value{
SI H100
}
\description{
Demands:

- The stand shall be a Scots Pine or Norway Spruce stand with maximum 20 \% of basal area composed of other species.

- The stand must be even-aged.

- The stand must not have been hindered in its' development.

- The trees used to determine the intercept must not have had serious injuries, for example stem breakage.

- The stand must not have been high-graded.

- The stand must not have been fertilised before the five shoots included in the intercept.

- It must be possible to clearly distinguish the five shoots of interest. This can be tricky for Norway spruce.

- Obviously, the stand must have reached a level of development where five shoots exceeding 2.5 metres must exist.

- The stand must not be too old - naturally hindered by self-pruning.

- The stand must not be too old - it will at a certain point no longer be certain that the current dominant trees were previously dominant.

- By age 35 at breast height or higher, determining site index by height functions will perform better.
}
\details{
Standard deviation for the prognosis error was estimated to 16 dm for Scots Pine, and 18 dm for Norway Spruce.
In general the method is clearly less precise in Spruce stands than in Pine stands (p.54).
}
