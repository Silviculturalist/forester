% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Carbonnier_1975.R
\name{Carbonnier_1975_compound_basal_area_growth_Oak}
\alias{Carbonnier_1975_compound_basal_area_growth_Oak}
\title{Basal area increment percentage according to compound interest}
\usage{
Carbonnier_1975_compound_basal_area_growth_Oak(
  SIH100,
  silt_fraction,
  total_age,
  BA_before_thinning,
  BA_before_thinning_understory,
  mean_basal_area_diameter_after_thinning,
  mean_height_Lorey_after_thinning,
  stems_after_thinning,
  removal_last_thinning_BA,
  removal_last_thinning_BA_understory,
  removal_second_last_thinning_BA,
  removal_second_last_thinning_BA_understory,
  removal_third_last_thinning_BA,
  removal_third_last_thinning_BA_understory
)
}
\arguments{
\item{SIH100}{SIH 100 Oak, e.g. [forester::Carbonnier_1975_Sweden_height_trajectories_Oak]}

\item{silt_fraction}{Percent, Soil fraction composed of particles less than 0.06 mm.}

\item{total_age}{total age}

\item{BA_before_thinning}{Basal Area before thinning.}

\item{BA_before_thinning_understory}{Basal area of understory before thinning.}

\item{mean_basal_area_diameter_after_thinning}{Diameter corresponding to the mean basal area after thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]}

\item{mean_height_Lorey_after_thinning}{Basal area weighted mean height of stand, per Loreys formula, e.g. [forester::Lorey_mean_height]}

\item{stems_after_thinning}{Stems remaining per hectare after thinning.}

\item{removal_last_thinning_BA}{BA removed by thinning during the last thinning.}

\item{removal_last_thinning_BA_understory}{BA removed by thinning from the understory during the last thinning.}

\item{removal_second_last_thinning_BA}{BA removed by thinning during the second last thinning.}

\item{removal_second_last_thinning_BA_understory}{BA removed by thinning from the understory during the second last thinning.}

\item{removal_third_last_thinning_BA}{BA removed by thinning during the third last thinning.}

\item{removal_third_last_thinning_BA_understory}{BA removed by thinning from the understory during the third last thinning.}
}
\value{
Basal area increment percentage according to compound interest.
}
\description{
Basal area increment percentage according to compound interest
}
