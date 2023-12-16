#' Bark addition function for Birch from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 20
#'
#' Multiple correlation coefficient: 0.966
#'
#' Standard deviation about the function (sf) : 0.271
#'
#' sf/standard deviation about the mean:  28\%
#'
#'
#' @param basal_area_under_bark_Birch Basal Area Birch under bark, m2/ha
#' @param age_at_breast_height Age at breast height for Pine stems.
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height, Birch, m2/ha.
#' @export

Agestam_1985_bark_addition_Birch <- function(
  basal_area_under_bark_Birch,
  age_at_breast_height,
  latitude
){
  return(
    exp(
      +19.276+
      +0.818*log(basal_area_under_bark_Birch)+
      +0.269*log(age_at_breast_height)+
      -5.25*log(latitude)
    )
  )
}
#' Bark addition function for Pine from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 143
#'
#' Multiple correlation coefficient: 0.968
#'
#' Standard deviation about the function (sf) : 0.112
#'
#' sf/standard deviation about the mean:  26\%
#'
#'
#'
#' @param basal_area_under_bark_Pine Basal area Pine under bark, m2/ha.
#' @param SI_Pine Site Index H100 for Scots Pine from Hägglund (1974)
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height for Pine, m2/ha.
#' @export
Agestam_1985_bark_addition_Pine <- function(
  basal_area_under_bark_Pine,
  SI_Pine,
  latitude
){
  return(
    exp(
      15.98+
      +1.013*log(basal_area_under_bark_Pine)+
      -0.197*log(SI_Pine)+
      -3.928*log(latitude)
    )
  )
}
#' Bark addition function for Spruce from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 119
#'
#' Multiple correlation coefficient: 0.941
#'
#' Standard deviation about the function (sf) : 0.156
#'
#' sf/standard deviation about the mean:  34\%
#'
#'
#'
#' @param basal_area_under_bark_Spruce Basal area under bark m2/ha.
#' @param SI_Spruce SI H100 Spruce from Hägglund 1972,1973.
#'
#' @return Basal area bark at breast height, m2/ha.
#' @export

Agestam_1985_bark_addition_Spruce <- function(
  basal_area_under_bark_Spruce,
  SI_Spruce
){
  return(
    exp(
      1.366+
      +0.934*log(basal_area_under_bark_Spruce)+
      -0.562*log(SI_Spruce)
    )
  )
}
#' Bark subtraction function for Birch from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 20
#'
#' Multiple correlation coefficient: 0.977
#'
#' Standard deviation about the function (sf) : 0.233
#'
#' sf/standard deviation about the mean:  23\%
#'
#'
#'
#'
#' @param basal_area_above_bark_Birch Basal Area Birch including bark, m2/ha
#' @param age_at_breast_height Age at breast height for Pine stems.
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height, Birch, m2/ha.
#' @export

Agestam_1985_bark_subtraction_Birch <- function(
  basal_area_above_bark_Birch,
  age_at_breast_height,
  latitude
){
  return(
    exp(
      +0.861*log(basal_area_above_bark_Birch)+
      +0.217*log(age_at_breast_height)+
      -4.47*log(latitude)+
      +16.01
    )
  )

}
#' Bark subtraction function for Pine from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 143
#'
#' Multiple correlation coefficient: 0.982
#'
#' Standard deviation about the function (sf) : 0.085
#'
#' sf/standard deviation about the mean:  19\%
#'
#'
#'
#' @param basal_area_over_bark_Pine Basal area Pine including bark, m2/ha.
#' @param SI_Pine Site Index H100 for Scots Pine from Hägglund (1974)
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height for Pine, m2/ha.
#' @export

Agestam_1985_bark_subtraction_Pine <- function(
  basal_area_over_bark_Pine,
  SI_Pine,
  latitude
){
  return(
    exp(
      11.69+
      +1.025*log(basal_area_over_bark_Pine)+
      -0.151*log(SI_Pine)+
      -3.020*log(latitude)
    )
  )
}
#' Bark subtraction function for Spruce from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 46.
#'
#' @details Material:
#'
#' Number of plots: 119
#'
#' Multiple correlation coefficient: 0.954
#'
#' Standard deviation about the function (sf) : 0.138
#'
#' sf/standard deviation about the mean:  30\%
#'
#'
#' @param basal_area_above_bark_Spruce Basal area including bark m2/ha.
#' @param SI_Spruce SI H100 Spruce from Hägglund 1972,1973.
#'
#' @return Basal area bark at breast height, m2/ha.
#' @export
Agestam_1985_bark_subtraction_Spruce <- function(
  basal_area_above_bark_Spruce,
  SI_Spruce
){
  return(
    exp(
      0.808+
      +0.956*log(basal_area_above_bark_Spruce)+
      -0.495*log(SI_Spruce)
    )
  )
}
#' Basal area growth function for Birch from Agestam 1985
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 29.
#'
#' @details Material:
#'
#' Number of growth periods: 126
#'
#' Multiple correlation coefficient: 0.928
#'
#' Standard deviation about the function (sf) : 0.396
#'
#' sf/standard deviation about the mean:  38\%
#'
#'
#' @param basal_area_Birch_m2_ha_after_thinning Basal area of Birch after thinning, under bark. m2/ha.
#' @param stem_count_Birch Number of Birch stems per ha after thinning.
#' @param age_at_breast_height_period_start Age at breast height for Birch at the period start.
#' @param SI_H100_Birch Site index H100 for Birch according to height trajectory defined in Persson (1959)
#' @param basal_area_other_species_m2_ha_after_thinning Basal area under bark of other species after thinning, under bark. m2/ha.
#' @param increment_period_years Length of increment period, in years.
#'
#' @return Annual basal area growth under bark for Birch. m2/ha.
#' @export
Agestam_1985_basal_area_annual_increment_under_bark_Birch <- function(
  basal_area_Birch_m2_ha_after_thinning,
  stem_count_Birch,
  age_at_breast_height_period_start,
  SI_H100_Birch,
  basal_area_other_species_m2_ha_after_thinning,
  increment_period_years
)
{

  return(
    exp(
      +0.63464*log(basal_area_Birch_m2_ha_after_thinning*100)+
        +0.17829*log(stem_count_Birch)+
        -0.73932*log(age_at_breast_height_period_start+(increment_period_years/2))+
        +0.2557E-2*SI_H100_Birch*10+
        -0.2670E-3*(basal_area_other_species_m2_ha_after_thinning*100)+
        +0.4298
    )/100
  )

}
#' Basal area growth function for Pine from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 29.
#'
#' @details Material:
#'
#' Number of growth periods: 505
#'
#' Multiple correlation coefficient: 0.888
#'
#' Standard deviation about the function (sf) : 0.245
#'
#' sf/standard deviation about the mean:  46\%
#'
#'
#' @param basal_area_Pine_m2_ha_after_thinning Basal area of Pine after thinning, under bark. m2/ha.
#' @param stem_count_Pine Number of Pine stems per ha after thinning.
#' @param age_at_breast_height_period_start Age at breast height for Pine at the period start.
#' @param SI_H100_Pine Site index H100 for Pine according to height trajectory defined in Hägglund, 1974.
#' @param basal_area_other_species_m2_ha_after_thinning Basal area under bark of other species after thinning, under bark. m2/ha.
#' @param diameter_mean_basal_area_stem_Pine_cm Diameter corresponding to mean basal area under bark, Pine. cm.
#' @param diameter_mean_basal_area_stem_all_species_cm Diameter corresponding to mean basal area under bark, all species on plot. cm.
#' @param increment_period_years Length of increment period, in years.
#'
#' @return Annual basal area growth under bark for Pine. m2/ha.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot()+geom_function(fun= function(x) Agestam_1985_basal_area_annual_increment_under_bark_Pine(basal_area_Pine_m2_ha_after_thinning = 15,stem_count_Pine = 800,age_at_breast_height_period_start = x,SI_H100_Pine = 25,basal_area_other_species_m2_ha_after_thinning = 15,diameter_mean_basal_area_stem_Pine_cm = 1,diameter_mean_basal_area_stem_all_species_cm = 1,increment_period_years = 10))+xlim(c(10,140))+ylim(c(0,0.5))
#
Agestam_1985_basal_area_annual_increment_under_bark_Pine <- function(
  basal_area_Pine_m2_ha_after_thinning,
  stem_count_Pine,
  age_at_breast_height_period_start,
  SI_H100_Pine,
  basal_area_other_species_m2_ha_after_thinning,
  diameter_mean_basal_area_stem_Pine_cm,
  diameter_mean_basal_area_stem_all_species_cm,
  increment_period_years
)
{
  #Age during middle of increment period.
  age_at_breast_height_period_start <- age_at_breast_height_period_start+(increment_period_years/2)

  a <- dplyr::case_when(
    age_at_breast_height_period_start<=40 ~ 0.37398,
    age_at_breast_height_period_start<=60 ~ 0.38475,
    age_at_breast_height_period_start<=80 ~ 0.38733,
    TRUE ~ 0.39222

  )


  return(
    exp(
      -2.0065+
        -0.1328E-3*(basal_area_Pine_m2_ha_after_thinning*100)+
        +0.42115*log(basal_area_Pine_m2_ha_after_thinning*100)+
        +a*log(stem_count_Pine)+
        +0.4362E-2*SI_H100_Pine*10+
        -0.19424*log(age_at_breast_height_period_start)+
        -0.2130E-3*(basal_area_other_species_m2_ha_after_thinning*100)+
        +0.10502*log((diameter_mean_basal_area_stem_Pine_cm)/(diameter_mean_basal_area_stem_all_species_cm))
    )/100
  )

}
#' Basal area growth function for Spruce from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 29.
#'
#' @details Material:
#'
#' Number of growth periods: 586
#'
#' Multiple correlation coefficient: 0.938
#'
#' Standard deviation about the function (sf) : 0.293
#'
#' sf/standard deviation about the mean:  35\%
#'
#'
#'
#' @param basal_area_Spruce_m2_ha_after_thinning Basal area of Spruce after thinning, under bark. m2/ha.
#' @param stem_count_Spruce Number of Spruce stems per ha after thinning.
#' @param age_at_breast_height_period_start Age at breast height for Spruce at the period start.
#' @param SI_H100_Spruce Site index H100 for Spruce according to height trajectory defined in Hägglund, 1972, 1973.
#' @param basal_area_other_species_m2_ha_after_thinning Basal area under bark of other species after thinning, under bark. m2/ha.
#' @param increment_period_years Length of increment period, in years.
#' @param removal_basal_area_m2_ha_at_start_of_period Removed basal area at the start of the growth period, under bark, Spruce. m2/ha.
#'
#' @return Annual basal area growth under bark for Spruce. m2/ha.
#' @export

Agestam_1985_basal_area_annual_increment_under_bark_Spruce <- function(
  basal_area_Spruce_m2_ha_after_thinning,
  stem_count_Spruce,
  age_at_breast_height_period_start,
  SI_H100_Spruce,
  basal_area_other_species_m2_ha_after_thinning,
  increment_period_years,
  removal_basal_area_m2_ha_at_start_of_period
)
{

  return(
    exp(
      -0.2309E-3*(basal_area_Spruce_m2_ha_after_thinning*100)+
        +0.67033*log((basal_area_Spruce_m2_ha_after_thinning*100))+
        +0.23347*log(stem_count_Spruce)+
        -0.34588*log(age_at_breast_height_period_start+(increment_period_years/2))+
        +0.2659E-2*SI_H100_Spruce*10+
        -0.3366E-3*(basal_area_other_species_m2_ha_after_thinning*100)+
        -0.02174*log(removal_basal_area_m2_ha_at_start_of_period*100)+
        -1.3933
    )/100
  )

}
#' Mortality for Birch per annum according to Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 50.
#'
#' @details Material:
#'
#' Number of plots: 87
#'
#' Multiple correlation coefficient: 0.484
#'
#' Standard deviation about the function (sf) : 1.58
#'
#' sf/standard deviation about the mean:  92\%
#'
#' @param stems_ha_all_species Total number of stems per hectare.
#' @param basal_area_m2_ha_Birch Basal area m2/ha of Birch.
#'
#' @return m2/ha mortality above bark per annum.
#' @export
Agestam_1985_mortality_above_bark_Birch <- function(
  stems_ha_all_species,
  basal_area_m2_ha_Birch
){
  return(
    exp(
      -8.3764+
      +0.4199*log(stems_ha_all_species)+
      +0.3236*log(basal_area_m2_ha_Birch)
    )
  )
}
#' Mortality for Pine per annum according to Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 50.
#'
#' @details Material:
#'
#' Number of plots: 134
#'
#' Multiple correlation coefficient: 0.400
#'
#' Standard deviation about the function (sf) : 1.99
#'
#' sf/standard deviation about the mean:  93\%
#'
#' @param stems_Pine_ha Number of Pine stems per hectare.
#' @param basal_area_above_bark_all_species Basal area above bark for all tree species. m2/ha.
#' @param SI_Pine Site Index H100 for Pine according to Hägglund (1974), meters.
#' @param age_at_breast_height Age at breast height of Pine stems.
#'
#' @return m2/ha mortality above bark per annum.
#' @export

Agestam_1985_mortality_above_bark_Pine <- function(
  stems_Pine_ha,
  basal_area_above_bark_all_species,
  SI_Pine,
  age_at_breast_height
){
  return(
    exp(
      16.039+
      +0.4809*log(stems_Pine_ha)+
      +2.2526*log(basal_area_above_bark_all_species)+
      -3.5439*log(SI_Pine*10)+
      -2.6191*log(age_at_breast_height)
    )
  )
}
#' Mortality for Spruce per annum according to Agestam 1985.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 50.
#'
#' @details Material:
#'
#' Number of plots: 134
#'
#' Multiple correlation coefficient: 0.447
#'
#' Standard deviation about the function (sf) : 1.74
#'
#' sf/standard deviation about the mean:  90\%
#'
#' @param stems_ha Number of Spruce stems per hectare.
#' @param basal_area_above_bark_all_species Total basal area including bark, m2/ha.
#'
#' @return m2/ha mortality above bark per annum.
#' @export

Agestam_1985_mortality_above_bark_Spruce <- function(
  stems_ha,
  basal_area_above_bark_all_species
){
  return(
    exp(
      -16.996+
      +1.4344*log(stems_ha)+
      +0.9816*log(basal_area_above_bark_all_species)
    )
  )
}
#' Basal area weighted mean diameter of dead trees from Agestam 1985
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 53.
#'
#' @param species "Pinus sylvestris" or "Picea abies" or "Betula pendula" or "Betula pubescens"
#' @param age_at_breast_height Age of stems at breast height.
#' @param basal_area_weighted_mean_diameter_cm_living Basal area weighted mean diameter of living trees (in cm)
#'
#' @return Basal area weighted mean diameter of dead trees, in cm.
#' @export
Agestam_1985_mortality_diameter_cm <- function(
  species,
  age_at_breast_height,
  basal_area_weighted_mean_diameter_cm_living
){
  quota <- dplyr::case_when(
    species=="Pinus sylvestris" & age_at_breast_height<45 ~ 0.6,
    species=="Pinus sylvestris" & age_at_breast_height>=45 ~ 0.54,
    species=="Picea abies" ~ 0.61,
    species%in%c("Betula pendula","Betula pubescens") ~ 0.69
    )

  return(
    basal_area_weighted_mean_diameter_cm_living*quota
  )

}
#' Volume function for Birch from Agestam (1985), including height.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: B1
#'
#' Number of plots: 50
#'
#' Multiple correlation coefficient: 0.998
#'
#' Standard deviation about the function (sf): 0.069
#'
#' sf/standard deviation about the mean: 6.4\%
#'
#'
#'
#' @param basal_area_Birch_m2_ha Basal area over bark Pine, m2/ha.
#' @param dominant_height_Birch_m Dominant height of Birch stems, m2 /ha.
#' @param stems_ha_Birch Number of Birch stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Birch m3sk/ha.
#' @export
Agestam_1985_volume_Birch_height <- function(
  basal_area_Birch_m2_ha,
  dominant_height_Birch_m,
  stems_ha_Birch,
  latitude
){

  return(
    exp(
      +1.0419*log(basal_area_Birch_m2_ha)+
      +0.9020*log(dominant_height_Birch_m)+
      +1.2264*(1/stems_ha_Birch)+
      +0.6799*log(latitude)+
        -5.5828
    )
  )
}
#' Volume function for Birch from Agestam (1985), including SI.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: B2
#'
#' Number of plots: 47
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.078
#'
#' sf/standard deviation about the mean: 8.2\%
#'
#'
#' @param basal_area_Birch_m2_ha Basal area over bark Pine, m2/ha.
#' @param age_at_breast_height_Birch Age at breast height for Birch stems.
#' @param SI_Birch_m Site Index for Birch according to Persson (1959)
#' @param stems_ha_Birch Number of Birch stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Birch m3sk/ha.
#' @export
Agestam_1985_volume_Birch_SI <- function(
  basal_area_Birch_m2_ha,
  age_at_breast_height_Birch,
  SI_Birch_m,
  stems_ha_Birch,
  latitude
){
  return(
    exp(
      +1.0789*log(basal_area_Birch_m2_ha)+
      +0.9233*log(SI_Birch_m)+
      +0.4574*log(age_at_breast_height_Birch)+
      -0.04264*log(stems_ha_Birch)+
      +0.6905*log(latitude)+
      -7.3526
    )
  )
}
#' Volume function for Pine from Agestam (1985), including height.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: T1
#'
#' Number of plots: 37
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.031
#'
#' sf/standard deviation about the mean: 6.8\%
#'
#'
#' @param basal_area_Pine_m2_ha Basal area over bark Pine, m2/ha.
#' @param dominant_height_Pine_m Dominant height of Pine stems.
#' @param stems_Pine_ha Number of Pine stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Pine m3sk/ha.
#' @export
Agestam_1985_volume_Pine_height <- function(
  basal_area_Pine_m2_ha,
  dominant_height_Pine_m,
  stems_Pine_ha,
  latitude
){
  return(
    exp(
      +1.0509*log(basal_area_Pine_m2_ha)+
      +0.8074*log(dominant_height_Pine_m*10)+
      -0.04356*log(stems_Pine_ha)+
      +0.5957*log(latitude)+
        -4.4313
    )
  )
}
#' Volume function for Pine from Agestam (1985), including SI.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: T2
#'
#' Number of plots: 141
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.038
#'
#' sf/standard deviation about the mean: 7.4\%
#'
#'
#' @param basal_area_Pine_m2_ha Basal area over bark Pine, m2/ha.
#' @param SI_Pine_m Site Index H100 of Pine, meters,  from Hägglund 1974.
#' @param age_at_breast_height_Pine Age at breast height of Pine stems.
#' @param stems_Pine_ha Pine stems per hectare.
#' @param latitude Latitude, degrees.
#'
#' @return Volume Pine m3sk/ha.
#' @export
Agestam_1985_volume_Pine_SI <- function(
  basal_area_Pine_m2_ha,
  SI_Pine_m,
  age_at_breast_height_Pine,
  stems_Pine_ha,
  latitude
){
  SI_Pine <- SI_Pine_m*10
return(
  exp(
    +1.0772*log(basal_area_Pine_m2_ha)+
    +0.7859*log(SI_Pine)+
    +0.3131*log(age_at_breast_height_Pine)+
    -7.4681*(1/age_at_breast_height_Pine)+
    -0.06444*log(stems_Pine_ha)+
    +0.4741*log(latitude)+
    -5.0965
  )
)

}
#' Volume function for Spruce from Agestam (1985), including height.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: G1
#'
#' Number of plots: 115
#'
#' Multiple correlation coefficient: 0.995
#'
#' Standard deviation about the function (sf): 0.058
#'
#' sf/standard deviation about the mean: 10.2%
#'
#'
#' @param basal_area_Spruce_m2_ha Basal area over bark Spruce, m2/ha.
#' @param dominant_height_Spruce_m Dominant height of Spruce stems, m.
#' @param stems_Spruce_ha Spruce stems per hectare.
#' @param age_at_breast_height_Spruce Age at breast height of Spruce stems.
#'
#' @return Volume Spruce m3sk/ha.
#' @export
Agestam_1985_volume_Spruce_height <- function(
  basal_area_Spruce_m2_ha,
  dominant_height_Spruce_m,
  stems_Spruce_ha,
  age_at_breast_height_Spruce
){
  return(
    exp(
      +1.0509*log(basal_area_Spruce_m2_ha)+
      +0.7386*log(dominant_height_Spruce_m)+
      -0.008218*sqrt(stems_Spruce_ha)+
      -0.01225*sqrt(age_at_breast_height_Spruce)+
      -1.8422

    )
  )
}
#' Volume function for Spruce from Agestam (1985), including SI.
#'
#' @source Agestam, E. (1985) 'A growth simulator for mixed stands of pine, spruce
#' and birch in Sweden. Diss. Swedish University of Agricultural Sciences. Report no. 15. Dept. of Forest Yield Research. ISSN 0348-7636.
#' ISBN 91-576-2528-x. 150 pp. Garpenberg, Sweden; page. 38.
#'
#' @details Function: G2
#'
#' Number of plots: 143
#'
#' Multiple correlation coefficient: 0.997
#'
#' Standard deviation about the function (sf): 0.060
#'
#' sf/standard deviation about the mean: 8.3%
#'
#' @param basal_area_Spruce_m2_ha Basal area over bark Spruce, m2/ha.
#' @param SI_Spruce Site Index H100 of Spruce, meters,  from Hägglund 1972,1973.
#' @param age_at_breast_height_Spruce Age at breast height of Spruce stems.
#' @param stems_ha_Spruce Spruce stems per hectare.
#' @param stems_ha_other Stems per hectare of other tree species.
#'
#' @return Volume Spruce m3sk/ha.
#' @export
Agestam_1985_volume_Spruce_SI <- function(
  basal_area_Spruce_m2_ha,
  SI_Spruce,
  age_at_breast_height_Spruce,
  stems_ha_Spruce,
  stems_ha_other
){
  return(
    exp(
      +1.2010*log(basal_area_Spruce_m2_ha)+
      +0.6476*log(SI_Spruce*10)+
      +0.1581*log(age_at_breast_height_Spruce)+
      -0.01084*sqrt(stems_ha_Spruce)+
      -0.002148*sqrt(stems_ha_other)+
      -11.1378*(1/age_at_breast_height_Spruce)+
      -2.1739
    )
  )
}
