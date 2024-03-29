% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Bengtsson_1978_mortality.R
\name{Bengtsson_1978_annual_volume_mortality_percent}
\alias{Bengtsson_1978_annual_volume_mortality_percent}
\title{Percentage of live standing volume lost to mortality per annum}
\usage{
Bengtsson_1978_annual_volume_mortality_percent(
  QMD,
  age,
  MAI,
  northern_Sweden = TRUE,
  species = "Pinus sylvestris",
  reduce = TRUE,
  floor = 0.1,
  cieling = 0.9
)
}
\arguments{
\item{QMD}{Diameter of mean basal area stem. Possibly arithmetic mean diameter.}

\item{age}{stand age.}

\item{MAI}{Mean Annual Increment max. e.g. [forester::Hagglund_1981_si_to_bonitet()],[forester::Jonson_1914_to_MAImax()]}

\item{northern_Sweden}{Northern Sweden? North of c. 60 deg. lat?}

\item{species}{One of 'Pinus sylvestris','Picea abies' or 'Betula pendula'.}

\item{reduce}{Default=TRUE. Reduce due to rather high mortality during years of data collection? See details.}

\item{floor}{Minimum annual mortality, in percent of live standing volume.}

\item{cieling}{Maximum annual mortality, in percent of live standing volume.}
}
\value{
Annual mortality in percent of live standing volume.
}
\description{
Natural mortality. Functions based on National Forest Inventory
 surveys of dead trees during 1973-1975. Dead or wind-felled trees above 5 cm
 diameter at breast height. Avoided harsh storm damages during 1969. Largest
 insecurity pertains to which season the substrate in question died.

 A 'roof' was set at a maximum of 0.9% per annum and never let the loss be less than 0.1% per annum.

 The mortality level during the observed period was judged to have been
 higher than normal so at application the mortality given by the functions
 was reduced with 40 % in northern Sweden and 25 % in southern Sweden.

 \href{https://lagen.nu/sou/1978:7/sid287.png}{Available Online (Swedish)}
}
