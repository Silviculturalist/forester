#' Mortality in self-thinned stands
#'
#' @source From Table 7.2, Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 86.
#'
#' @description
#'
#' Material from Unthinned permanent plots.
#'
#' \strong{N.B. As unthinned plots} present a relatively extreme case, this function is not suggested to be used other than in plots with very high basal area. (p. 86 & p. 90)
#'
#' @details
#'
#' Multiple correlation coefficient R = 0.40
#'
#' Spread about the function sf = 0.0059
#'
#' sf/Spread about the mean = 92.7
#'
#' Number of observations = 532.
#'
#' @param SI_species Species for which SIH100 was estimated. One of 'Picea abies' or 'Pinus sylvestris'.
#' @param SIH100 Site Index H100, metres.
#' @param total_age_stand Total age of the stand.
#' @param Basal_area_plot_m2_ha Basal area of all tree species on the plot, m^2 / ha.
#' @param Basal_area_Spruce_m2_ha Basal area Spruce on the plot, m^2 / ha.
#' @param Basal_area_Deciduous_m2_ha Basal area Deciduous trees on the plot, m^2 / ha.
#'
#' @return Mortality as annual proportion of basal area.
#' @export

Soderberg_1986_BA_percentual_mortality_per_annum <- function(SI_species,
                                                     SIH100,
                                                     total_age_stand,
                                                     Basal_area_plot_m2_ha,
                                                     Basal_area_Spruce_m2_ha,
                                                     Basal_area_Deciduous_m2_ha
                                                     ){

  BA_quotient_Spruce <- Basal_area_Spruce_m2_ha / Basal_area_plot_m2_ha
  BA_quotient_Deciduous <- Basal_area_Deciduous_m2_ha / Basal_area_plot_m2_ha

  spruce <- ifelse(SI_species=="Picea abies",1,0)
  pine <- ifelse(SI_species=="Pinus sylvestris",1,0)

  return(
      +0.60949*(1/(total_age_stand+10))+
      -12.5903*((1/(total_age_stand+10))^2)+
      +0.3317E-3*Basal_area_plot_m2_ha+
      -0.01006*(log(Basal_area_plot_m2_ha))+
      +0.173E-3*spruce*SIH100+
      +0.156E-3*pine*SIH100+
      -0.0130*BA_quotient_Spruce+
      +0.0119*(BA_quotient_Spruce^2)+
      +0.0189*(BA_quotient_Deciduous)+
      -0.0174*(BA_quotient_Deciduous^2)+
      +0.0232
  )

}
#' Basal area in self-thinned stands

#' @source From Table 7.1, Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser - Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. / Functions for forecasting of timber yields - Increment and form height for individual trees of native species in Sweden. Report 14. Section of Forest Mensuration and Management. Swedish University of Agricultural Sciences. Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251. p. 83.
#'
#' @description
#'
#' Material from Unthinned permanent plots.
#'
#' @details
#'
#' Multiple correlation coefficient R = 0.87
#'
#' Spread about the function sf = 0.15
#'
#' sf/Spread about the mean = 50.2
#'
#' Number of observations = 532.
#'
#' @param SI_species Species for which SIH100 was estimated. One of 'Picea abies' or 'Pinus sylvestris'.
#' @param SIH100 Site Index H100, metres.
#' @param total_age_stand Total age of the stand.
#' @param BA_quotient_Spruce Basal area Spruce / Basal area
#' @param BA_quotient_Deciduous Basal area Deciduous trees / Basal area
#' @param stems_per_ha Number of stems per hectare.
#'
#' @return Basal area m2/ha.
#' @export

Soderberg_1986_BA_self_thinned_stands <- function(
  SI_species,
  SIH100,
  total_age_stand,
  BA_quotient_Spruce,
  BA_quotient_Deciduous,
  stems_per_ha
){

  spruce <- ifelse(SI_species=="Picea abies",1,0)
  pine <- ifelse(SI_species=="Pinus sylvestris",1,0)


  return(
    exp(

    -18.612*((1/(total_age_stand+10)))+
    -765.295*(((1/(total_age_stand+10)))^2)+
    +0.04798*SIH100*spruce+
    +0.05589*SIH100*pine+
    +0.06717E-4*stems_per_ha+
    -0.2864E-8*(stems_per_ha^2)+
    +0.7204*BA_quotient_Spruce+
    -0.4879*(BA_quotient_Spruce^2)+
    +0.1062*BA_quotient_Deciduous+
    -0.2073*(BA_quotient_Deciduous^2)+
    +2.5225

    )
  )

}

Soderberg_1986_BA_self_thinned_stands <- Vectorize(Soderberg_1986_BA_self_thinned_stands)
#' Double bark for Individual trees in Sweden from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#' Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#' Functions for forecasting of timber yields - Increment and form height for
#' individual trees of native species in Sweden. Report 14. Section of Forest
#' Mensuration and Management. Swedish University of Agricultural Sciences.
#' Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251.
#'
#' @description
#' \strong{Applicable counties:}
#'  \tabular{cc}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#'
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been
#' distinguished by Sylvén 1917. The distribution of the different forms of
#' Scots Pine is assumed to depend on the expansion-history of the Pine,
#' according to which the Pine immigrated both from the north and the south
#' with a transitional zone in central Sweden, where both forms are present.
#' For other species, the change in genotype is more continuous over the
#' country (Kiellander 1974). For Norway Spruce, it is assumed that it has had
#' time for 30-50 generations in northern Sweden, contrasted against only 10-20
#' in southern Sweden (Kiellander 1966). Therefore the same regional division
#' is used for Norway Spruce as for Scots Pine. For the other species, the
#' regional division is limited to two regions, wherein the northern and central
#' regions have been merged.
#'
#' **NB in Söderberg (1986), no correction for logarithmic bias was introduced**
#' , as, (freely translated) p. 114: "At the presentation of the functions we
#' lightly touched on the effects of the measurement errors in the variables
#' which had been included in the regression. It was then concluded
#' that several factors contrived to that the spread about the functions is
#' overestimated. Therefore no correction for logarithmic bias was carried out,
#' primarily because the effect of the errors in the indipendent variables on
#' the spread about the function are difficult to establish. If one were to
#' assume that the spread about the function was overestimated by 10 percent,
#' the predicted growth from the functions would be increased by roughly 2
#' percent. For 20 percents overestimation, an increase of predicted growth by
#' 4.5 percent."
#'
#' @details
#' **Oak**
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.89 \cr
#' Spread about the function sf \tab 0.20 \cr
#' sf/Spread about the mean \tab 0.38 \cr
#' Number of observations \tab 315 \cr
#' }
#'
#' **Beech**
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.21 \cr
#' sf/Spread about the mean \tab 0.41 \cr
#' Number of observations \tab 291 \cr
#' }
#'
#' **Norway Spruce**
#' \strong{For Norway Spruce, single function is reported area-wise due to use
#' of dummy variables in only some locations. Regression statistics for entire
#' country}
#'
#' **Norway Spruce in Sweden**
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.22 \cr
#' sf/Spread about the mean \tab 0.48 \cr
#' Number of observations \tab 12279 \cr
#' }
#'
#'
#' **Scots Pine in Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.87 \cr
#' Spread about the function sf \tab 0.22 \cr
#' sf/Spread about the mean \tab 0.43 \cr
#' Number of observations \tab 2316 \cr
#' }
#'
#' **Scots Pine in Central Sweden** ##
#'
#'\tabular{ll}{
#' Multiple correlation coefficient R \tab 0.85 \cr
#' Spread about the function sf \tab 0.23 \cr
#' sf/Spread about the mean \tab 0.46 \cr
#' Number of observations \tab 2014 \cr
#' }
#'
#' **Scots Pine in southern Sweden**
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.24 \cr
#' sf/Spread about the mean \tab 0.49 \cr
#' Number of observations \tab 4833 \cr
#' }
#'
#'
#' **Broadleaves in southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.82 \cr
#' Spread about the function sf \tab 0.30 \cr
#' sf/Spread about the mean \tab 0.53 \cr
#' Number of observations \tab 403 \cr
#' }
#'
#' **Broadleaves in Northern and Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.29 \cr
#' sf/Spread about the mean \tab 0.52 \cr
#' Number of observations \tab 244 \cr
#' }
#'
#'
#' ***Birch species: Betula pendula**
#'
#' NB: \emph{Betula pendula} is in the original source referred as
#' \emph{Betula verrucosa}
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.32 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 846 \cr
#' }
#'
#' ***Birch species: Betula pubescens**
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.23 \cr
#' sf/Spread about the mean \tab 0.46 \cr
#' Number of observations \tab 1852 \cr
#' }
#'
#'
#' **Birch in Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.85 \cr
#' Spread about the function sf \tab 0.22 \cr
#' sf/Spread about the mean \tab 0.43 \cr
#' Number of observations \tab 1093 \cr
#' }
#'
#' **Birch in southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.81 \cr
#' Spread about the function sf \tab 0.27 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 1605 \cr
#' }
#'
#'
#' @param DBH.u.b.cm Diameter under bark of tree, in cm.
#' @param age Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param continental TRUE, if the plot is situated in a continental climatic
#' region. cf. Ångström 1958. e.g.
#' [forester::Angstrom_1958_local_climate_Sweden()]. Otherwise FALSE.
#' @param soil_moisture 1-5. e.g. [forester::Sweden_soil_types('moisture')]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g.
#' [forester::coast_distance()]
#' @param SI100 Site Index H100, m. TO DO: Ask P-M Ekö for clarification.
#' @param vegetation Vegetation type according to follows Swedish National
#' forest inventory FALTSKIKT. e.g. [forester::Sweden_vegetation_types()]
#'
#'
#' @md
#'
#' @return Double bark thickness, mm.
#' @export
#' @name Soderberg1986doublebark
Soderberg_1986_double_bark_central_Sweden_Pine <- function(
    DBH.u.b.cm,
    age,
    latitude,
    altitude,
    soil_moisture,
    distance_to_coast_km,
    SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  dry <- ifelse(soil_moisture==1,1,0)

  B1 <- ifelse(SI100>=14.1 & SI100 <=22.0,1,0)

  B2 <- ifelse(SI100>=22.1,1,0)

  return(
    exp(
      +0.99156*10^(-2)*(DBH.u.b.cm*10)+
        -0.13367*10^(-4)*((DBH.u.b.cm*10)^2)+
        +0.33766*10^(-2)*age+
        -0.13367*10^(-4)*(age^2)+
        -0.56911*10^(-1)*latitude+
        +0.15138*10^(-2)*altitude+
        -0.29419*10^(-4)*latitude*altitude+
        -0.23967*10^(-1)*dry+
        +0.11524*far_from_coast+
        -0.11249*10^(-1)*B1+
        -0.55964*10^(-1)*B2+
        +5.1825
    )
  )
}



#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_central_Sweden_Spruce <- function(
    DBH.u.b.cm,
    age,
    soil_moisture,
    distance_to_coast_km,
    SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  dry <- ifelse(soil_moisture==1,1,0)

  return(
    exp(
      +0.55670*10^(-2)*(DBH.u.b.cm*10)+
        -0.52109*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.45036*10^(-2)*age+
        -0.58820*10^(-5)*(age^2)+
        -0.15400*10^(-2)*(SI100*10)+
        +0.88803*10^(-1)*dry+
        +0.96139*10^(-1)*far_from_coast+
        +1.73903
    )
  )
}


#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_northern_central_Sweden_Birch <- function(
    DBH.u.b.cm,
    age,
    latitude,
    vegetation,
    soil_moisture,
    SI100,
    distance_to_coast_km
){


  dry <- ifelse(soil_moisture==1,1,0)

  herb <- ifelse(vegetation<7,1,0)

  B1 <- ifelse(SI100>=14.1 & SI100<=18.0,1,0)
  B2 <- ifelse(SI100>=18.1,1,0)


  return(
    exp(
      +0.82582*10^(-2)*(DBH.u.b.cm*10)+
        -0.75897*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.89957*10^(-2)*age+
        -0.22729*10^(-4)*(age^2)+
        -0.60489*latitude+
        +0.47242*10^(-2)*(latitude^2)+
        -0.42615*10^(-1)*herb+
        +0.24851*dry+
        -0.75997*10^(-1)*B1+
        -0.13227*B2+
        +20.421
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_northern_central_Sweden_Broadleaves <- function(
    DBH.u.b.cm,
    age,
    latitude,
    altitude,
    distance_to_coast_km,
    SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  B2 <- ifelse(SI100>=22.1,1,0)

  return(
    exp(
      +0.59990*10^(-2)*(DBH.u.b.cm*10)+
        +0.13983*10^(-1)*age+
        -0.38181*10^(-4)*(age^2)+
        +0.24932*10^(-1)*latitude+
        +0.24780*10^(-2)*altitude+
        -0.74190*10^(-1)*(altitude^(1/2))+
        -0.18749*far_from_coast+
        +0.16729*B2+
        +0.49494*10^(-1)
    )
  )
}


#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_northern_Sweden_Pine <- function(
    DBH.u.b.cm,
    age,
    latitude,
    altitude,
    soil_moisture,
    distance_to_coast_km,
    vegetation,
    continental,
    SI100
){

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0)

  herb <- ifelse(vegetation<7,1,0)

  dry <- ifelse(soil_moisture==1,1,0)

  B1 <- ifelse(SI100>=14.1 & SI100 <=22.0,1,0)

  B2 <- ifelse(SI100>=22.1,1,0)

  return(
    exp(
      +0.10562*10^(-1)*(DBH.u.b.cm*10)+
        -0.13895*10^(-4)*((DBH.u.b.cm*10)^2)+
        +0.21565*10^(-2)*age+
        -0.54700*10^(-5)*(age^2)+
        -0.30599*10^(-1)*latitude+
        -0.57487*10^(-3)*altitude+
        +0.73755*10^(-5)*latitude*altitude+
        -0.47475*10^(-1)*dry+
        +0.61779*10^(-1)*far_from_coast+
        +0.59693*10^(-1)*herb+
        -0.44162*10^(-1)*B1+
        -0.57575*10^(-1)*B2+
        +3.48671
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_northern_Sweden_Spruce <- function(
    DBH.u.b.cm,
    age,
    latitude,
    soil_moisture,
    SI100
){


  dry <- ifelse(soil_moisture==1,1,0)
  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      +0.55670*10^(-2)*(DBH.u.b.cm*10)+
        -0.52109*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.45036*10^(-2)*age+
        -0.58820*10^(-5)*(age^2)+
        -0.15400*10^(-2)*(SI100*10)+
        -0.20080*10^(-1)*latitude+
        +0.11903*moist+
        +0.55123
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_southern_Sweden_Birch <- function(
    DBH.u.b.cm,
    age,
    latitude,
    altitude,
    vegetation,
    soil_moisture,
    SI100,
    distance_to_coast_km
){


  dry <- ifelse(soil_moisture==1,1,0)

  herb <- ifelse(vegetation<7,1,0)

  B2 <- ifelse(SI100>=18.1,1,0)


  return(
    exp(
      +0.10551*10^(-1)*(DBH.u.b.cm*10)+
        -0.10367*10^(-4)*((DBH.u.b.cm*10)^2)+
        +0.35263*10^(-2)*age+
        +2.4541*latitude+
        -0.21789*10^(-1)*(latitude^2)+
        -0.15337*10^(-1)*altitude+
        +0.25626*10^(-3)*latitude*altitude+
        -0.13123*herb+
        +0.17776*dry+
        -0.57322*10^(-1)*B2+
        -67.617
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_southern_Sweden_Broadleaves <- function(
    DBH.u.b.cm,
    age,
    latitude,
    altitude,
    soil_moisture,
    vegetation
){

  herb <- ifelse(vegetation<7,1,0)

  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      +0.99684*10^(-2)*(DBH.u.b.cm*10)+
        -0.13735*10^(-4)*((DBH.u.b.cm*10)^2)+
        +0.75334*10^(-2)*age+
        -0.32951*10^(-4)*(age^2)+
        +1.8722*latitude+
        -0.15950*10^(-1)*(latitude^2)+
        -0.91041*10^(-1)*herb+
        +0.29149*moist+
        -53.455
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_central_Sweden_Pine <- function(
    DBH.u.b.cm,
    age,
    latitude,
    altitude,
    soil_moisture,
    vegetation,
    continental,
    SI100
){

  dry <- ifelse(soil_moisture==1,1,0)

  herb <- ifelse(vegetation<7,1,0)


  B1 <- ifelse(SI100>=14.1 & SI100 <=22.0,1,0)

  B2 <- ifelse(SI100>=22.1,1,0)

  return(
    exp(
      +0.84207*10^(-2)*(DBH.u.b.cm*10)+
        -0.10834*10^(-4)*((DBH.u.b.cm*10)^2)+
        +0.62648*10^(-2)*age+
        -0.25723*10^(-4)*(age^2)+
        +5.22630*latitude+
        -0.44767*10^(-1)*(latitude^2)+
        +0.21438*10^(-1)*altitude+
        -0.37426*10^(-3)*latitude*altitude+
        -0.20067*10^(-1)*dry+
        +0.15725*10^(-1)*herb+
        -0.30724*10^(-1)*continental+
        -0.37919*10^(-1)*B1+
        -0.59201*10^(-1)*B2+
        -150.42
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_southern_Sweden_Spruce <- function(
    DBH.u.b.cm,
    age,
    latitude,
    soil_moisture,
    SI100
){
  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      +0.55670*10^(-2)*(DBH.u.b.cm*10)+
        -0.52109*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.45036*10^(-2)*age+
        -0.58820*10^(-5)*(age^2)+
        -0.15400*10^(-2)*(SI100*10)+
        -0.20080*10^(-1)*latitude+
        +0.11903*moist+
        +0.76203
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_Sweden_Beech <- function(
    DBH.u.b.cm,
    age,
    latitude
){

  return(
    exp(
      +0.56775*10^(-2)*(DBH.u.b.cm*10)+
        -0.53487*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.36356*10^(-2)*age+
        +0.15406*latitude+
        -7.8258
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_Sweden_Betula_pendula <- function(
    DBH.u.b.cm,
    age,
    latitude,
    vegetation,
    soil_moisture,
    SI100,
    distance_to_coast_km
){


  dry <- ifelse(soil_moisture==1,1,0)
  moist <- ifelse(soil_moisture>3,1,0)

  herb <- ifelse(vegetation<7,1,0)

  far_from_coast <- ifelse(distance_to_coast_km>50,1,0 )

  B3 <- ifelse(SI100>=26.1,1,0)


  return(
    exp(
      +0.12948*10^(-1)*(DBH.u.b.cm*10)+
        -0.15211*10^(-4)*((DBH.u.b.cm*10)^2)+
        +0.56088*10^(-2)*age+
        -0.17412*10^(-4)*(age^2)+
        -0.42668*10^(-1)*latitude+
        -0.10724*herb+
        +0.57244*dry+
        -0.43211*moist+
        +0.12738*far_from_coast+
        -0.88281*10^(-1)*B3+
        +3.4646
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_Sweden_Betula_pubescens <- function(
    DBH.u.b.cm,
    age,
    latitude,
    vegetation,
    soil_moisture,
    SI100,
    distance_to_coast_km
){


  dry <- ifelse(soil_moisture==1,1,0)

  herb <- ifelse(vegetation<7,1,0)


  B1 <- ifelse(SI100>=14.1 & SI100<=18.0,1,0)
  B2 <- ifelse(SI100>=18.1,1,0)


  return(
    exp(
      +0.83895*10^(-2)*(DBH.u.b.cm*10)+
        -0.92737*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.70117*10^(-2)*age+
        -0.10746*10^(-4)*(age^2)+
        -0.54713*latitude+
        +0.42428*10^(-2)*(latitude^2)+
        -0.52477*10^(-1)*herb+
        +0.24091*dry+
        -0.64082*10^(-1)*B1+
        -0.12616*B2+
        +18.769
    )
  )
}

#' @rdname Soderberg1986doublebark
#' @export
Soderberg_1986_double_bark_Sweden_Oak <- function(
    DBH.u.b.cm,
    age,
    latitude
){

  return(
    exp(
      +0.64773*10^(-2)*(DBH.u.b.cm*10)+
        -0.63950*10^(-5)*((DBH.u.b.cm*10)^2)+
        +0.95725*10^(-2)*age+
        -0.54041*10^(-4)*(age^2)+
        +3.2234*latitude+
        -0.27582*10^(-1)*(latitude^2)+
        -92.368
    )
  )
}
#' Form factor for Individual trees in Sweden from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#' Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#' Functions for forecasting of timber yields - Increment and form height for
#'  individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'   Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251.
#'
#' @description
#' \strong{Applicable counties:}
#'  \tabular{lr}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#'
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been
#' distinguished by Sylvén 1917. The distribution of the different forms of
#' Scots Pine is assumed to depend on the expansion-history of the Pine,
#' according to which the Pine immigrated both from the north and the south
#' with a transitional zone in central Sweden, where both forms are present.
#' For other species, the change in genotype is more continuous over the
#' country (Kiellander 1974). For Norway Spruce, it is assumed that it has had
#' time for 30-50 generations in northern Sweden, contrasted against only 10-20
#' in southern Sweden (Kiellander 1966). Therefore the same regional division
#' is used for Norway Spruce as for Scots Pine. For the other species, the
#' regional division is limited to two regions, wherein the northern and central
#' regions have been merged.
#'
#' **NB in Söderberg (1986), no correction for logarithmic bias was introduced**
#' , as, (freely translated) p. 114: "At the presentation of the functions we
#' lightly touched on the effects of the measurement errors in the variables
#' which had been included in the regression. It was then concluded
#' that several factors contrived to that the spread about the functions is
#' overestimated. Therefore no correction for logarithmic bias was carried out,
#' primarily because the effect of the errors in the indipendent variables on
#' the spread about the function are difficult to establish. If one were to
#' assume that the spread about the function was overestimated by 10 percent,
#' the predicted growth from the functions would be increased by roughly 2
#' percent. For 20 percents overestimation, an increase of predicted growth by
#' 4.5 percent."
#'
#' @details
#' **Oak**
#'
#' \strong{OBSERVE:} The function has a typo in the book. Although the
#' variable description states that it is Altitude, the symbol shows LAT. I have
#' interpreted this as that it should be Altitude, since the size of the
#' coefficient is most alike coefficients for ALT in other related functions.
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.86 \cr
#' Spread about the function sf \tab 0.180 \cr
#' sf/Spread about the mean \tab 0.516 \cr
#' Number of observations \tab 292 \cr
#' }
#'
#' **Beech**
#'
#' \strong{OBSERVE:} The function has a typo in the book. Although the
#' variable description states that it is Altitude^2, the symbol shows LAT^2.
#' I have interpreted this as that it should be Altitude^2, since the size of
#' the coefficient is most alike coefficients for ALT^2 in other related
#' functions.
#'
#' \strong{OBSERVE:} The following interpretation was made of the function -
#' since only a factor was given for the Site Index in the case that the
#' indicating species was Picea abies, this was interpreted as mandatory.
#' Therefore, SI_Species and SI100 have been replaced by SI100_Spruce.
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.90 \cr
#' Spread about the function sf \tab 0.165 \cr
#' sf/Spread about the mean \tab 0.442 \cr
#' Number of observations \tab 219 \cr
#' }
#'
#' **Norway Spruce in southern Sweden**
#'
#' \strong{OBSERVE:} As per p. 64 , if county is equal to "Gotland": "Residual
#' studies for these species indicate the functions for the mainland (Spruce
#' south, Birch south) can be used with a reduction of the constant term
#' amounting to -0.089 and -0.067, respectively".
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.94 \cr
#' Spread about the function sf \tab 0.128 \cr
#' sf/Spread about the mean \tab 0.346 \cr
#' Number of observations \tab 7311 \cr
#' }
#'
#' **Norway Spruce in Central or Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.93 \cr
#' Spread about the function sf \tab 0.132 \cr
#' sf/Spread about the mean \tab 0.365 \cr
#' Number of observations \tab 7410 \cr
#' }
#'
#'
#'
#' **Scots Pine in Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.94 \cr
#' Spread about the function sf \tab 0.107 \cr
#' sf/Spread about the mean \tab 0.352 \cr
#' Number of observations \tab 3785 \cr
#' }
#'
#' **Scots Pine in Central Sweden**
#'
#'\tabular{ll}{
#' Multiple correlation coefficient R \tab 0.94 \cr
#' Spread about the function sf \tab 0.105 \cr
#' sf/Spread about the mean \tab 0.328 \cr
#' Number of observations \tab 2248 \cr
#' }
#'
#' **Scots Pine in southern Sweden**
#'
#' \strong{NB for the island of Gotland, use
#' [forester::Soderberg_1986_form_factor_Gotland_Sweden_Pine()]}
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.92 \cr
#' Spread about the function sf \tab 0.120 \cr
#' sf/Spread about the mean \tab 0.395 \cr
#' Number of observations \tab 4231 \cr
#' }
#'
#' **Scots Pine on the island Gotland**
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.150 \cr
#' sf/Spread about the mean \tab 0.545 \cr
#' Number of observations \tab 351 \cr
#' }
#'
#' **Broadleaves in southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.80 \cr
#' Spread about the function sf \tab 0.187 \cr
#' sf/Spread about the mean \tab 0.609 \cr
#' Number of observations \tab 831 \cr
#' }
#'
#' **Broadleaves in Central or Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.87 \cr
#' Spread about the function sf \tab 0.174 \cr
#' sf/Spread about the mean \tab 0.499 \cr
#' Number of observations \tab 317 \cr
#' }
#'
#'
#' **Birch in southern Sweden**
#'
#' \strong{OBSERVE:} As per p. 64 , if county is equal to "Gotland": "Residual
#' studies for these species indicate the functions for the mainland (Spruce
#' south, Birch south) can be used with a reduction of the constant term
#' amounting to -0.089 and -0.067, respectively".
#'
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.85 \cr
#' Spread about the function sf \tab 0.161 \cr
#' sf/Spread about the mean \tab 0.533 \cr
#' Number of observations \tab 1725 \cr
#' }
#'
#' **Birch in Central or Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.91 \cr
#' Spread about the function sf \tab 0.138 \cr
#' sf/Spread about the mean \tab 0.416 \cr
#' Number of observations \tab 1735 \cr
#' }
#'
#' @param DBH.cm Diameter at breast height
#' @param DBH_largest_tree_on_plot.cm Diameter at breast height of the
#' largest tree on the plot.
#' @param BA_Beech.m2_ha Basal area Beech on the plot, m^2 / ha.
#' @param BA_Pine.m2_ha Basal area Scots Pine on the plot, m^2 / ha.
#' @param BA_Spruce.m2_ha Basal area Norway Spruce on the plot, m^2 / ha.
#' @param BA_Birch.m2_ha Basal area Birch on the plot, m^2 / ha.
#' @param BA_Oak.m2_ha Basal area Oak on the plot, m^2 / ha.
#' @param BA.m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age Age at breast height of the tree.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, meters above sea level.
#' @param SI_species Species for which SIH100 was estimated. One of :
#' 'Picea abies' or 'Pinus sylvestris'.
#' @param SI100 Site Index H100, m.
#' @param SI100_Spruce Site Index H100, m for Norway Spruce. See description.#'
#' @param soil_moisture 1-5. e.g. [forester::Sweden_soil_types()]
#' @param lateral_water \tabular{cl}{
#' Code \tab Description \cr
#' 1 \tab Missing \cr
#' 2 \tab Seldom \cr
#' 3 \tab Shorter periods \cr
#' 4 \tab Longer periods \cr
#' 5 \tab Slope \cr
#' }
#' @param divided_plot 1 for plots described in different parts, which appears
#' when the original plot consists of different land classes, density classes
#' or cutting classes or belongs to different owners. 0 for full plots
#' (default).
#' @param fertilised_plot 1 for fertilised plots, 0 for others (default).
#' @param aspect If more than 2:20 / 5\%, one of the following. Otherwise 0.
#' \tabular{cl}{
#' 1 \tab North  \cr
#' 2 \tab North-East \cr
#' 3 \tab East \cr
#' 4 \tab South-East \cr
#' 5 \tab South \cr
#' 6 \tab South-West \cr
#' 7 \tab West \cr
#' 8 \tab North-West \cr
#' }
#' @param maritime TRUE, if the plot is situated in a maritime climatic region.
#'  cf. Ångstrom 1958. e.g. [forester::Angstrom_1958_local_climate_Sweden()] .
#'  Otherwise FALSE.
#' @param vegetation 1-18. See [forester::Sweden_vegetation_types()] for
#' description.
#' @param divided_plot 1 for plots described in different parts,
#' which appears when the original plot consists of different land classes,
#' density classes or cutting classes or belongs to different owners. 0 for
#' full plots (default).
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param continental TRUE, if the plot is situated in a continental climatic region.
#'  cf. Ångstrom 1958. e.g. [forester::Angstrom_1958_local_climate_Sweden()] .
#'  Otherwise FALSE.
#' @param county Character string. One of:
#'  \tabular{cc}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#' @md
#'
#' @return Form quotient of tree
#' @export
#' @name Soderberg1986formfactor
Soderberg_1986_form_factor_Sweden_Oak <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_Beech.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    age,
    vegetation,
    soil_moisture,
    lateral_water,
    altitude,
    divided_plot=0,
    county
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  BA_quotient_Beech <- BA_Beech.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)
  herb <- ifelse(vegetation<7,1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      -0.27070E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.11454E+05*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.16097E-02*((1/(age+10))^2)+
        -0.17742E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.39008E-03*spruce*SI100*10+#m to dm.
        +0.36858E-03*pine*SI100*10+ #m to dm
        +0.15098E-01*BA.m2_ha+
        -0.14109E-03*(BA.m2_ha^2)+
        +0.57341E+00*diameter_quotient+
        -0.52971E+00*(diameter_quotient^2)+
        -0.31421E-03*altitude+
        -0.91133E-01*BA_quotient_Birch+
        +0.21363E+00*BA_quotient_Beech+
        -0.12131E+00*divided_plot+
        +0.64025E-01*herb+
        +0.12112E+00*moist+
        -0.65506E-01*seldom_lateral_water+
        -0.38960E-01*region5+
        +0.25636E+01
    )
  )


}


#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_Sweden_Beech <- function(
    DBH.cm,
    BA_Beech.m2_ha,
    BA_Oak.m2_ha,
    BA.m2_ha,
    SI100_Spruce,
    age,
    aspect,
    altitude,
    divided_plot=0
){
  BA_quotient_Beech <- BA_Beech.m2_ha/BA.m2_ha
  BA_quotient_Oak <- BA_Oak.m2_ha/BA.m2_ha
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)

  return(
    exp(
      -0.21212E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.73868E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.16097E+02*(1/(age+10))+
        +0.27278E+03*((1/(age+10))^2)+
        -0.30178E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.21615E-03*SI100_Spruce*10+#m to dm.
        +0.14469E-01*BA.m2_ha+
        -0.13999E-03*(BA.m2_ha^2)+
        -0.30893E-05*(altitude^2)+
        +0.17708E+00*BA_quotient_Beech+
        -0.14122E+00*BA_quotient_Oak+
        -0.13570E+00*divided_plot+
        -0.86474E-01*north+
        +0.27313E+01
    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_southern_Sweden_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    continental,
    maritime,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    age,
    aspect,
    latitude,
    altitude,
    lateral_water,
    divided_plot=0,
    county
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)

  south_eastern_county <- ifelse(county%in%c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  value_constant <- ifelse(county!="Gotland",0.27730E+01, ((0.27730E+01)-0.089))

  return(
    exp(
      -0.25255E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.52037E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.79332E+01*(1/(age+10))+
        +0.15360E+03*((1/(age+10))^2)+
        -0.25202E-02*age+
        -0.50822E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.95744E-03*spruce*SI100*10+#m to dm.
        +0.11126E-02*pine*SI100*10+ #m to dm.
        +0.86560E-02*BA.m2_ha+
        -0.68753E-04*(BA.m2_ha^2)+
        +0.52388E+00*diameter_quotient+
        -0.44192E+00*(diameter_quotient^2)+
        +0.33543E-03*altitude+
        -0.11486E-05*(altitude)^2+
        +0.14504E+00*BA_quotient_Spruce+
        +0.21586E-01*continental+
        -0.18175E-01*maritime+
        +0.16159E-01*north+
        -0.51101E-01*divided_plot+
        -0.14608E-01*seldom_lateral_water+
        +0.17760E-01*south_eastern_county+
        +value_constant
    )
  )


}


#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_southern_Sweden_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    distance_to_coast_km,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    vegetation,
    age,
    latitude,
    altitude,
    county,
    maritime,
    soil_moisture,
    divided_plot=0,
    fertilised_plot
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  empetrum_calluna <- ifelse(vegetation%in%c(15,16),1,0)
  herb <- ifelse(vegetation<7,1,0)
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)
  moist <- ifelse(soil_moisture>3,1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad","Malmöhus","Västra Götaland","Halland","Gotland"),1,0)


  return(
    exp(
      -0.24437E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.96502E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.14142E+02*(1/(age+10))+
        +0.16515E+03*((1/(age+10))^2)+
        -0.26773E-02*age+
        -0.41275E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.12858E-02*spruce*SI100*10+#m to dm.
        +0.16419E-02*pine*SI100*10+ #m to dm.
        +0.92576E-02*BA.m2_ha+
        -0.89234E-04*(BA.m2_ha^2)+
        +0.24741E+00*diameter_quotient+
        -0.22097E+00*(diameter_quotient^2)+
        -0.88419E+00*latitude+
        +0.76429E-02*(latitude^2)+
        -0.45527E-04*(altitude)+
        -0.64779E-01*divided_plot+
        -0.20583E-01*herb+
        +0.37361E-01*fertilised_plot+
        -0.21721E-01*maritime+
        -0.30357E-01*empetrum_calluna+
        -0.40200E-01*moist+
        +0.11526E+00*BA_quotient_Spruce+
        -0.28372E-01*region5+
        +0.28245E+02

    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_southern_Sweden_Broadleaves <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    age,
    latitude,
    vegetation,
    soil_moisture,
    aspect,
    divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  herb <- ifelse(vegetation<7,1,0)
  moist <- ifelse(soil_moisture>3,1,0)

  return(
    exp(
      -0.14927E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.37752E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.25858E+02*((1/(age+10))^2)+
        +0.10999E-02*spruce*SI100*10+#m to dm.
        +0.10513E-02*pine*SI100*10+ #m to dm.
        +0.11070E-01*BA.m2_ha+
        -0.10669E-03*(BA.m2_ha^2)+
        +0.28134E+00*diameter_quotient+
        -0.29273E+00*(diameter_quotient^2)+
        +0.22956E-01*latitude+
        +0.75214E-01*BA_quotient_Spruce+
        -0.72064E-01*divided_plot+
        +0.10339E+00*south+
        -0.27334E-01*herb+
        -0.44593E-01*moist+
        +0.58178E+00
    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_southern_Sweden_Birch <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    maritime,
    BA_Birch.m2_ha,
    BA_Pine.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    vegetation,
    age,
    latitude,
    altitude,
    divided_plot=0,
    fertilised_plot=0,
    county
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  herb <- ifelse(vegetation<7,1,0)
  empetrum_calluna <- ifelse(vegetation%in%c(15,16),1,0)
  dry <- ifelse(soil_moisture==1,1,0)

  region5 <- ifelse(county %in% c("Blekinge","Kristianstad","Malmöhus","Västra Götaland","Halland","Gotland"),1,0)

  value_constant <- ifelse(county!="Gotland",0.12148E+01, ((0.12148E+01)-0.067))

  return(
    exp(
      -0.14327E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.39558E+02*(1/(age+10))+
        -0.17589E-02*age+
        -0.34887E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.45798E-03*spruce*SI100*10+#m to dm.
        +0.50812E-03*pine*SI100*10+ #m to dm.
        +0.11981E-01*BA.m2_ha+
        -0.13693E-03*(BA.m2_ha^2)+
        +0.13814E+00*diameter_quotient+
        -0.25030E+00*(diameter_quotient^2)+
        +0.22195E-01*latitude+
        -0.26037E-05*latitude*altitude+
        -0.63942E-01*BA_quotient_Pine+
        +0.40774E-01*BA_quotient_Birch+
        -0.56749E-01*maritime+
        -0.73603E-01*divided_plot+
        +0.12825E-01*herb+
        -0.57647E-01*dry+
        -0.17425E+00*empetrum_calluna+
        -0.40860E-01*region5+
        +0.56214E-01*fertilised_plot+
        +value_constant
    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_northern_Sweden_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    distance_to_coast_km,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    vegetation,
    age,
    latitude,
    altitude,
    aspect,
    soil_moisture,
    divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  empetrum_calluna <- ifelse(vegetation%in%c(15,16),1,0)
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  moist <- ifelse(soil_moisture>3,1,0)


  return(
    exp(
      -0.22799E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.66896E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.11349E+02*(1/(age+10))+
        +0.17205E+03*((1/(age+10))^2)+
        -0.16672E-02*age+
        -0.58581E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.11750E-02*spruce*SI100*10+#m to dm.
        +0.11099E-02*pine*SI100*10+ #m to dm.
        +0.97338E-02*BA.m2_ha+
        -0.92807E-04*(BA.m2_ha^2)+
        -0.94134E-01*diameter_quotient+
        +0.11760E-01*(diameter_quotient^2)+
        +0.43779E+00*latitude+
        -0.34411E-02*(latitude^2)+
        -0.19820E-06*(altitude^2)+
        -0.20823E-01*close_to_coast+
        -0.44766E-01*divided_plot+
        -0.19262E-01*empetrum_calluna+
        +0.20471E-01*south+
        -0.18321E-01*moist+
        +0.88735E-01*BA_quotient_Pine+
        +0.43954E-01*BA_quotient_Spruce+
        -0.11064E+02

    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_northern_central_Sweden_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    distance_to_coast_km,
    continental,
    BA_Spruce.m2_ha,
    BA_Pine.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    vegetation,
    age,
    aspect,
    latitude,
    altitude,
    soil_moisture,
    lateral_water,
    divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  herb <- ifelse(vegetation<7,1,0)
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)
  dry <- ifelse(soil_moisture==1,1,0)
  moist <- ifelse(soil_moisture>3,1,0)
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)



  return(
    exp(
      -0.22860E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.48767E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.81995E+01*(1/(age+10))+
        +0.17613E+03*((1/(age+10))^2)+
        -0.13356E-02*age+
        -0.40996E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.73529E-03*spruce*SI100*10+#m to dm.
        +0.55197E-03*pine*SI100*10+ #m to dm.
        +0.11655E-01*BA.m2_ha+
        -0.10073E-03*(BA.m2_ha^2)+
        +0.43924E+00*diameter_quotient+
        -0.36956E+00*(diameter_quotient^2)+
        -0.10855E-01*latitude+
        +0.10401E-03*(altitude)+
        -0.63246E-06*(altitude^2)+
        +0.88430E-01*BA_quotient_Pine+
        +0.89763E-01*BA_quotient_Spruce+
        -0.46335E-01*close_to_coast+
        +0.11849E-01*continental+
        +0.20148E-01*north+
        -0.46182E-01*divided_plot+
        -0.11448E-01*herb+
        -0.32981E-01*dry+
        -0.10124E-01*moist+
        -0.11366E-01*seldom_lateral_water+
        +0.33240E+01
    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_northern_central_Sweden_Broadleaves <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    maritime,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    age,
    latitude,
    altitude,
    lateral_water,
    divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)

  return(
    exp(
      -0.75461E+02*(1/((DBH.cm*10) + 50))+ #diameter in mm
        -0.22422E+02*(1/(age+10))+
        +0.23326E+03*((1/(age+10))^2)+
        -0.34112E-02*age+
        +0.93785E-03*spruce*SI100*10+#m to dm.
        +0.12420E-02*pine*SI100*10+ #m to dm.
        +0.18241E-01*BA.m2_ha+
        -0.16138E-03*(BA.m2_ha^2)+
        +0.77488E+00*diameter_quotient+
        -0.44199E+00*(diameter_quotient^2)+
        -0.13223E-01*latitude+
        -0.21817E-03*altitude+
        +0.10235E+00*BA_quotient_Spruce+
        -0.52928E-01*divided_plot+
        +0.90171E-01*maritime+
        -0.83195E-01*seldom_lateral_water+
        +0.26801E+01
    )
  )


}

#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_northern_central_Sweden_Birch <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_Spruce.m2_ha,
    BA_Pine.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    vegetation,
    aspect,
    age,
    latitude,
    altitude,
    soil_moisture,
    lateral_water,
    fertilised_plot,
    divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  empetrum_calluna <- ifelse(vegetation%in%c(15,16),1,0)
  dry <- ifelse(soil_moisture==1,1,0)
  moist <- ifelse(soil_moisture>3,1,0)
  seldom_lateral_water <- ifelse(lateral_water%in%c(1,2),1,0)



  return(
    exp(
      -0.18780E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.41264E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        +0.49071E+01*(1/(age+10))+
        -0.38410E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.73517E-03*spruce*SI100*10+#m to dm.
        +0.58577E-03*pine*SI100*10+ #m to dm.
        +0.16827E-01*BA.m2_ha+
        -0.17158E-03*(BA.m2_ha^2)+
        -0.32079E-01*diameter_quotient+
        -0.17386E-01*latitude+
        -0.14858E-03*(altitude)+
        -0.44756E-06*(altitude^2)+
        -0.42524E-01*BA_quotient_Pine+
        -0.60774E-01*BA_quotient_Spruce+
        -0.54917E-01*divided_plot+
        +0.65036E-01*fertilised_plot+
        -0.62793E-01*dry+
        -0.24338E-01*moist+
        -0.17656E-01*seldom_lateral_water+
        -0.95882E-01*empetrum_calluna+
        +0.34916E+01
    )
  )


}


#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_diameter_quotient_Gotland_Sweden_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_Pine.m2_ha,
    BA.m2_ha,
    SI100_Spruce,
    age,
    altitude,
    divided_plot=0
){

  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha


  return(
    exp(
      -0.21146E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.71765E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.62341E+01*(1/(age+10))+
        +0.15382E+03*((1/(age+10))^2)+
        +0.39119E-03*SI100_Spruce*10+#m to dm.
        +0.14818E-01*BA.m2_ha+
        -0.12168E-03*(BA.m2_ha^2)+
        -0.19266E+00*(diameter_quotient^2)+
        +0.14934E-02*(altitude)+
        -0.13057E+00*divided_plot+
        -0.14060E+00*BA_quotient_Pine+
        +0.23998E+01

    )
  )


}


#' @rdname Soderberg1986formfactor
#' @export
Soderberg_1986_form_factor_central_Sweden_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    distance_to_coast_km,
    BA_Pine.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    age,
    latitude,
    altitude,
    soil_moisture,
    fertilised_plot,
    divided_plot=0
){
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)
  moist <- ifelse(soil_moisture>3,1,0)


  return(
    exp(
      -0.23404E+03*(1/((DBH.cm*10) + 50))+ #diameter in mm
        +0.75190E+04*((1/((DBH.cm*10) + 50))^2)+ #diameter in mm
        -0.13151E+02*(1/(age+10))+
        +0.17182E+03*((1/(age+10))^2)+
        -0.15626E-02*age+
        -0.33891E-01*(DBH.cm*10/age)+ #diameter in mm
        +0.11113E-02*spruce*SI100*10+#m to dm.
        +0.11987E-02*pine*SI100*10+ #m to dm.
        +0.10184E-01*BA.m2_ha+
        -0.90301E-04*(BA.m2_ha^2)+
        -0.18661E+00*diameter_quotient+
        +0.48146E-01*(diameter_quotient^2)+
        +0.35143E+01*latitude+
        -0.28628E-01*(latitude^2)+
        -0.33545E-06*(altitude^2)+
        -0.45949E-01*close_to_coast+
        -0.46950E-01*divided_plot+
        +0.11568E-01*fertilised_plot+
        -0.12351E-01*moist+
        -0.16899E-01*BA_quotient_Pine+
        -0.104909E+03

    )
  )


}
#' Basal Area Increment of 5 years for Individual trees in Sweden from Söderberg 1986.
#'
#' @source Söderberg, U. (1986) Funktioner för skogliga produktionsprognoser -
#' Tillväxt och formhöjd för enskilda träd av inhemska trädslag i Sverige. /
#' Functions for forecasting of timber yields - Increment and form height for
#'  individual trees of native species in Sweden. Report 14. Section of Forest
#'   Mensuration and Management. Swedish University of Agricultural Sciences.
#'    Umeå. ISBN 91-576-2634-0. ISSN 0349-2133. pp.251.
#'
#' @description
#' \strong{Applicable counties:}
#'  \tabular{cc}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#'
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been
#' distinguished by Sylvén 1917. The distribution of the different forms of
#' Scots Pine is assumed to depend on the expansion-history of the Pine,
#' according to which the Pine immigrated both from the north and the south
#' with a transitional zone in central Sweden, where both forms are present.
#' For other species, the change in genotype is more continuous over the
#' country (Kiellander 1974). For Norway Spruce, it is assumed that it has had
#' time for 30-50 generations in northern Sweden, contrasted against only 10-20
#' in southern Sweden (Kiellander 1966). Therefore the same regional division
#' is used for Norway Spruce as for Scots Pine. For the other species, the
#' regional division is limited to two regions, wherein the northern and central
#' regions have been merged.
#'
#' **NB in Söderberg (1986), no correction for logarithmic bias was introduced**
#' , as, (freely translated) p. 114: "At the presentation of the functions we
#' lightly touched on the effects of the measurement errors in the variables
#' which had been included in the regression. It was then concluded
#' that several factors contrived to that the spread about the functions is
#' overestimated. Therefore no correction for logarithmic bias was carried out,
#' primarily because the effect of the errors in the indipendent variables on
#' the spread about the function are difficult to establish. If one were to
#' assume that the spread about the function was overestimated by 10 percent,
#' the predicted growth from the functions would be increased by roughly 2
#' percent. For 20 percents overestimation, an increase of predicted growth by
#' 4.5 percent."
#'
#' @details
#' **Beech Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.87 \cr
#' Spread about the function sf \tab 0.586 \cr
#' sf/Spread about the mean \tab 0.53 \cr
#' Number of observations \tab 178 \cr
#' Sum of weights \tab 161.5 \cr
#' }
#'
#' **Norway Spruce Southern Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.85 \cr
#' Spread about the function sf \tab 0.526 \cr
#' sf/Spread about the mean \tab 0.52 \cr
#' Number of observations \tab 3564 \cr
#' Sum of weights \tab 2997.7 \cr
#' }
#'
#' **Norway Spruce Southern Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.559 \cr
#' sf/Spread about the mean \tab 0.55 \cr
#' Number of observations \tab 2651 \cr
#' Sum of weights \tab 2446.5 \cr
#' }
#'
#' **Birch Southern Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.78 \cr
#' Spread about the function sf \tab 0.658 \cr
#' sf/Spread about the mean \tab 0.63 \cr
#' Number of observations \tab 1034 \cr
#' Sum of weights \tab 892.9 \cr
#' }
#'
#' **Birch Southern Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.74 \cr
#' Spread about the function sf \tab 0.697 \cr
#' sf/Spread about the mean \tab 0.68 \cr
#' Number of observations \tab 704 \cr
#' Sum of weights \tab 648.6 \cr
#' }
#'
#' **Broadleaves Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.79 \cr
#' Spread about the function sf \tab 0.630 \cr
#' sf/Spread about the mean \tab 0.62 \cr
#' Number of observations \tab 643 \cr
#' Sum of weights \tab 541.1 \cr
#' }
#'
#' **Oak Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.90 \cr
#' Spread about the function sf \tab 0.417 \cr
#' sf/Spread about the mean \tab 0.45 \cr
#' Number of observations \tab 222 \cr
#' Sum of weights \tab 187.9 \cr
#' }
#'
#' **Scots Pine Southern Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.80 \cr
#' Spread about the function sf \tab 0.506 \cr
#' sf/Spread about the mean \tab 0.60 \cr
#' Number of observations \tab 1662 \cr
#' Sum of weights \tab 1417.6 \cr
#' }
#'
#' **Scots Pine Southern Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.72 \cr
#' Spread about the function sf \tab 0.514 \cr
#' sf/Spread about the mean \tab 0.70 \cr
#' Number of observations \tab 2373 \cr
#' Sum of weights \tab 2221.5 \cr
#' }
#'
#' **Scots Pine Central Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.476 \cr
#' sf/Spread about the mean \tab 0.55 \cr
#' Number of observations \tab 815 \cr
#' Sum of weights \tab 692.9 \cr
#' }
#'
#' **Norway Spruce Northern Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.87 \cr
#' Spread about the function sf \tab 0.470 \cr
#' sf/Spread about the mean \tab 0.49 \cr
#' Number of observations \tab 1790 \cr
#' Sum of weights \tab 1610.4 \cr
#' }
#'
#' **Norway Spruce Northern Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.526 \cr
#' sf/Spread about the mean \tab 0.55 \cr
#' Number of observations \tab 3261 \cr
#' Sum of weights \tab 3035.4 \cr
#' }
#'
#' **Norway Spruce Central Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.505 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 1565 \cr
#' Sum of weights \tab 1370.1 \cr
#' }
#'
#' **Norway Spruce Central Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.86 \cr
#' Spread about the function sf \tab 0.546 \cr
#' sf/Spread about the mean \tab 0.52 \cr
#' Number of observations \tab 1733 \cr
#' Sum of weights \tab 1606.5 \cr
#' }
#'
#' **Broadleaves Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.595 \cr
#' sf/Spread about the mean \tab 0.57 \cr
#' Number of observations \tab 246  \cr
#' Sum of weights \tab 216.0 \cr
#' }
#'
#' **Birch Northern or Central Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.76 \cr
#' Spread about the function sf \tab 0.637 \cr
#' sf/Spread about the mean \tab 0.65 \cr
#' Number of observations \tab 755 \cr
#' Sum of weights \tab 706.8 \cr
#' }
#'
#' TO DO: Same as under 65?
#' **Birch Northern or Central Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.76 \cr
#' Spread about the function sf \tab 0.637 \cr
#' sf/Spread about the mean \tab 0.65 \cr
#' Number of observations \tab 755 \cr
#' Sum of weights \tab 706.8 \cr
#' }
#'
#' **Scots Pine Northern Sweden under 65**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.419 \cr
#' sf/Spread about the mean \tab 0.48 \cr
#' Number of observations \tab 1249 \cr
#' Sum of weights \tab 1084.2 \cr
#' }
#'
#' **Scots Pine Northern Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.80 \cr
#' Spread about the function sf \tab 0.502 \cr
#' sf/Spread about the mean \tab 0.60 \cr
#' Number of observations \tab 2495 \cr
#' Sum of weights \tab 2351.3 \cr
#' }
#'
#' #' **Scots Pine Central Sweden over 45**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.81 \cr
#' Spread about the function sf \tab 0.476 \cr
#' sf/Spread about the mean \tab 0.58 \cr
#' Number of observations \tab 1440 \cr
#' Sum of weights \tab 1350.9 \cr
#' }
#'
#'
#' @param DBH.cm Diameter at breast height
#' @param DBH_largest_tree_on_plot.cm Diameter at breast height of the largest tree on the plot.
#' @param BA_tree.m2 Basal area of the tree, m^2.
#' @param BA_Pine.m2_ha Basal area Pine on the plot, m^2 / ha.
#' @param BA_Spruce.m2_ha Basal area Spruce on the plot, m^2 / ha.
#' @param BA_Birch.m2_ha Basal area Birch on the plot, m^2 / ha.
#' @param BA.m2_ha Basal area of all tree species the plot, m^2 / ha.
#' @param age Age at breast height of the tree.
#' @param thinned TRUE if the stand has been thinned, otherwise FALSE.
#' @param last_thinned Number of growing seasons since last thinning.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, meters above sea level.
#' @param SI_species Species for which SIH100 was estimated.
#' One of : 'Picea abies' or 'Pinus sylvestris'.
#' @param SI100 Site Index H100, m.
#' @param soil_moisture 1-5. e.g. [forester::Sweden_soil_types()]
#' @param peatland 1 if plot is Peatland, 0 for others (default).
#' @param divided_plot 1 for plots described in different parts,
#' which appears when the original plot consists of different land classes,
#' density classes or cutting classes or belongs to different owners.
#' 0 for full plots (default).
#' @param fertilised_plot 1 for fertilised plots, 0 for others (default).
#' @param plot_inventoried_76_77 1 for plots measured in the years 1976-77,
#' 0 for others (default).
#' @param aspect If more than 2:20 / 5 percent, one of the following. Otherwise 0.
#'
#' \tabular{cl}{
#' 1 \tab North  \cr
#' 2 \tab North-East \cr
#' 3 \tab East \cr
#' 4 \tab South-East \cr
#' 5 \tab South \cr
#' 6 \tab South-West \cr
#' 7 \tab West \cr
#' 8 \tab North-West \cr
#' }
#' @param maritime TRUE, if the plot is situated in a maritime climatic region.
#'  cf. Ångstrom 1958. e.g. [forester::Angstrom_1958_local_climate_Sweden()] .
#'  Otherwise FALSE.
#' @param divided_plot 1 for plots described in different parts,
#' which appears when the original plot consists of different land classes,
#' density classes or cutting classes or belongs to different owners. 0 for
#' full plots (default).
#' @param vegetation 1-18. See [forester::Sweden_vegetation_types()] for
#' description.
#' @param county Character string. One of:
#'  \tabular{cc}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#' @md
#'
#' @return Basal area increment during 5 years, m2.
#' @export
#' @name Soderberg1986BAI5
Soderberg_1986_BAI5_Sweden_Oak <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    aspect,
    maritime
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.58270E-01*BA.m2_ha+
                         +0.77030E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.48067E-01*BA.m2_ha+
                                +0.67014E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.35960E-01*BA.m2_ha+
                                +0.34773E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10683E+01*log(basal_area_of_tree_cm2)+
        -0.42896E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.16232E+03*(1/(age+10))+
        -0.15683E+04*((1/(age+10))^2)+
        +thinning+
        +0.29030E+00*diameter_quotient+
        +0.25426E+00*north+
        +0.22321E+00*south+
        +0.13856E+00*maritime+
        -0.37307E+01

    )/10000 #cm2 to m2
  )


}


#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_Sweden_Beech <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    SI_species,
    SI100,
    age,
    thinned,
    last_thinned,
    latitude,
    altitude,
    divided_plot=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.40446E-01*BA.m2_ha+
                         +0.52343E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.17664E-01*BA.m2_ha
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.35227E-01*BA.m2_ha+
                                +0.40499E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.15936E+01*log(basal_area_of_tree_cm2)+
        -0.51911E-03*BA_tree.m2+
        +0.90769E+02*(1/(age+10))+
        -0.62604E+03*((1/(age+10))^2)+
        +thinning+
        -0.27505E+01*diameter_quotient+
        +0.12066E+01*(diameter_quotient^2)+
        +0.10086E+01*BA_quotient_Pine+
        +0.68754E+00*BA_quotient_Birch+
        +0.15978E-02*spruce*SI100*10+#m to dm.
        +0.33566E-03*pine*SI100*10+ #m to dm.
        -0.12884E+00*altitude+
        +0.22650E-02*latitude*altitude+
        +0.35174E+00*divided_plot+
        -0.50158E+01

    )/10000 #cm2 to m2
  )


}


#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_under_65_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    aspect,
    soil_moisture,
    county,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  dry <- ifelse(soil_moisture==1,1,0)
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.61903E-01*BA.m2_ha+
                         +0.67406E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.51359E-01*BA.m2_ha+
                                +0.45834E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.60125E-01*BA.m2_ha+
                                +0.65807E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.12995E+01*log(basal_area_of_tree_cm2)+
        -0.32205E-03*basal_area_of_tree_cm2+
        -0.42745E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.13002E+03*(1/(age+10))+
        -0.88771E+03*((1/(age+10))^2)+
        +thinning+
        -0.44756E+00*diameter_quotient+
        -0.19273E+00*BA_quotient_Pine+
        -0.42807E+00*BA_quotient_Spruce+
        -0.30893E+00*BA_quotient_Birch+
        +0.21660E-03*spruce*SI100*10+#m to dm.
        +0.70422E-03*pine*SI100*10+ #m to dm.
        +0.55118E-01*peatland+
        -0.10323E+00*dry+
        +0.45389E-01*moist+
        -0.11319E+00*north+
        -0.79993E-01*south+
        +0.98700E-01*region5+
        +0.63629E-01*divided_plot+
        +0.37888E-01*fertilised_plot+
        -0.34626E+01

    )/10000 #cm2 to m2
  )


}


#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_under_65_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    soil_moisture,
    county,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  dry <- ifelse(soil_moisture==1,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.54687E-01*BA.m2_ha+
                         +0.66494E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.58141E-01*BA.m2_ha+
                                +0.87783E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.61332E-01*BA.m2_ha+
                                +0.81506E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.14888E+01*log(basal_area_of_tree_cm2)+
        -0.10040E-02*basal_area_of_tree_cm2+
        -0.99781E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.14020E+03*(1/(age+10))+
        -0.75718E+03*((1/(age+10))^2)+
        +thinning+
        -0.16288E+00*diameter_quotient+
        +0.19800E+00*BA_quotient_Pine+
        +0.94400E-03*spruce*SI100*10+#m to dm.
        +0.60341E-03*pine*SI100*10+ #m to dm.
        +0.34176E+00*peatland+
        -0.12513E+00*dry+
        +0.10167E+00*south_eastern_county+
        +0.92116E-01*divided_plot+
        +0.12979E+00*fertilised_plot+
        +0.87499E-02*plot_inventoried_76_77+
        -0.49333E+01

    )/10000 #cm2 to m2
  )


}


#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_under_65_Birch <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    soil_moisture,
    county,
    divided_plot=0,
    fertilised_plot=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.50494E-01*BA.m2_ha+
                         +0.79803E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.31238E-01*BA.m2_ha
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.49770E-01*BA.m2_ha+
                                +0.46417E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10731E+01*log(basal_area_of_tree_cm2)+
        -0.57527E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.14600E+03*(1/(age+10))+
        -0.10335E+04*((1/(age+10))^2)+
        +thinning+
        +0.15895E+01*diameter_quotient+
        -0.86151E+00*(diameter_quotient^2)+
        +0.86278E+00*BA_quotient_Pine+
        +0.28913E+00*BA_quotient_Spruce+
        +0.14656E-02*spruce*SI100*10+#m to dm.
        +0.15297E-02*pine*SI100*10+ #m to dm.
        +0.10583E+00*moist+
        -0.95624E-01*south_eastern_county+
        +0.18366E+00*divided_plot+
        +0.73270E-01*fertilised_plot+
        -0.47436E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_over_45_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    aspect,
    soil_moisture,
    county,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  dry <- ifelse(soil_moisture==1,1,0)
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.57054E-01*BA.m2_ha+
                         +0.68177E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.54388E-01*BA.m2_ha+
                                +0.68291E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.55215E-01*BA.m2_ha+
                                +0.67161E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10064E+01*log(basal_area_of_tree_cm2)+
        +0.16664E+03*(1/(age+10))+
        -0.29572E+04*((1/(age+10))^2)+
        +thinning+
        +0.13696E+01*diameter_quotient+
        -0.12015E+01*(diameter_quotient^2)+
        -0.35992E+00*BA_quotient_Pine+
        -0.55521E+00*BA_quotient_Spruce+
        -0.70004E+00*BA_quotient_Birch+
        +0.57152E-03*spruce*SI100*10+#m to dm.
        +0.66865E-03*pine*SI100*10+ #m to dm.
        +0.73311E-01*peatland+
        -0.19658E+00*dry+
        +0.69787E-01*moist+
        -0.10264E+00*north+
        -0.10337E+00*south+
        +0.54467E-01*region5+
        +0.35977E-01*divided_plot+
        +0.58978E-01*fertilised_plot+
        -0.29605E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_over_45_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    soil_moisture,
    latitude,
    altitude,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  dry <- ifelse(soil_moisture==1,1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.38635E-01*BA.m2_ha+
                         +0.42831E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.35844E-01*BA.m2_ha+
                                +0.42792E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.37468E-01*BA.m2_ha+
                                +0.36891E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.85280E+00*log(basal_area_of_tree_cm2)+
        -0.14702E-03*basal_area_of_tree_cm2+
        +0.14841E+03*(1/(age+10))+
        -0.27409E+04*((1/(age+10))^2)+
        +thinning+
        +0.19038E+01*diameter_quotient+
        -0.13080E+01*(diameter_quotient^2)+
        +0.18171E+00*BA_quotient_Pine+
        +0.87210E-03*spruce*SI100*10+#m to dm.
        +0.75936E-03*pine*SI100*10+ #m to dm.
        +0.29166E+00*peatland+
        -0.81852E-05*latitude*altitude+
        -0.11758E+00*dry+
        -0.73811E-01*south+
        +0.11152E+00*divided_plot+
        +0.10516E+00*fertilised_plot+
        +0.40410E-01*plot_inventoried_76_77+
        -0.30557E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_over_45_Birch <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    soil_moisture,
    county,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.32163E-01*BA.m2_ha+
                         +0.32234E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.35013E-01*BA.m2_ha+
                                +0.40031E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.37450E-01*BA.m2_ha+
                                +0.44980E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.86066E+00*log(basal_area_of_tree_cm2)+
        +0.76322E+02*(1/(age+10))+
        +thinning+
        +0.16702E+01*diameter_quotient+
        -0.80138E+00*(diameter_quotient^2)+
        +0.67104E+00*BA_quotient_Pine+
        +0.45025E+00*BA_quotient_Spruce+
        +0.24659E+00*BA_quotient_Birch+
        +0.17333E-02*spruce*SI100*10+#m to dm.
        +0.22591E-02*pine*SI100*10+ #m to dm.
        +0.12044E+00*moist+
        -0.11905E+00*south_eastern_county+
        +0.13061E+00*divided_plot+
        +0.19571E+00*fertilised_plot+
        +0.20698E+00*plot_inventoried_76_77+
        -0.37239E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_southern_Sweden_Broadleaves <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    aspect,
    latitude,
    divided_plot=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.57341E-01*BA.m2_ha+
                         +0.72742E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.34258E-01*BA.m2_ha+
                                +0.15713E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.74100E-01*BA.m2_ha+
                                +0.14893E-02*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.12426E+01*log(basal_area_of_tree_cm2)+
        -0.38067E-03*BA_tree.m2+
        +0.86923E+02*(1/(age+10))+
        -0.49647E+03*((1/(age+10))^2)+
        +thinning+
        -0.14061E+01*diameter_quotient+
        +0.85103E+00*(diameter_quotient^2)+
        +0.32525E+00*BA_quotient_Pine+
        +0.24568E+00*BA_quotient_Spruce+
        +0.20104E+00*BA_quotient_Birch+
        +0.90258E-03*spruce*SI100*10+#m to dm.
        +0.84649E-03*pine*SI100*10+ #m to dm.
        +0.49305E-01*latitude+
        -0.14761E+00*north+
        +0.10352E+00*divided_plot+
        -0.63258E+01

    )/10000 #cm2 to m2
  )


}


#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_Sweden_under_65_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    aspect,
    latitude,
    altitude,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.36090E-01*BA.m2_ha+
                         +0.36083E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.32110E-01*BA.m2_ha+
                                +0.51283E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.22616E-01*BA.m2_ha+
                                +0.46227E-05*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.11503E+01*log(basal_area_of_tree_cm2)+
        -0.65032E-03*basal_area_of_tree_cm2+
        +0.70512E+02*(1/(age+10))+
        -0.31435E+03*((1/(age+10))^2)+
        +thinning+
        -0.87521E+00*diameter_quotient+
        +0.49368E+00*(diameter_quotient^2)+
        -0.27401E+00*BA_quotient_Pine+
        -0.33472E+00*BA_quotient_Spruce+
        +0.11976E-02*spruce*SI100*10+#m to dm.
        +0.11935E-02*pine*SI100*10+ #m to dm.
        +0.23003E+00*peatland+
        -0.44234E-02*altitude+
        +0.76409E-04*latitude*altitude+
        +0.54128E-01*south+
        +0.48618E-01*divided_plot+
        +0.15556E+00*fertilised_plot+
        +0.84235E-01*plot_inventoried_76_77+
        -0.28683E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_Sweden_under_65_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    latitude,
    aspect,
    soil_moisture,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.66243E-01*BA.m2_ha+
                         +0.75292E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.66400E-01*BA.m2_ha+
                                +0.89623E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.54112E-01*BA.m2_ha+
                                +0.43791E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.12199E+01*log(basal_area_of_tree_cm2)+
        -0.12143E-02*basal_area_of_tree_cm2+
        -0.40367E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.77769E+02*(1/(age+10))+
        -0.19198E+03*((1/(age+10))^2)+
        thinning+
        -0.17016E+00*diameter_quotient+
        +0.31920E+00*BA_quotient_Spruce+
        +0.24518E-02*spruce*SI100*10+#m to dm.
        +0.23361E-02*pine*SI100*10+ #m to dm.
        +0.13028E+01*latitude+
        -0.99353E-02*(latitude^2)+
        -0.56156E-01*north+
        +0.12153E+00*south+
        +0.15605E+00*moist+
        +0.92899E-01*divided_plot+
        +0.99817E-01*fertilised_plot+
        +0.70382E-01*plot_inventoried_76_77+
        -0.45954E+02

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_Sweden_over_45_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    aspect,
    soil_moisture,
    latitude,
    altitude,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  dry <- ifelse(soil_moisture==1,1,0)
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.25065E-01*BA.m2_ha+
                         +0.17057E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.79174E-01*BA.m2_ha+
                                -0.18978E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.13173E-01*BA.m2_ha+
                                -0.19558E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10342E+01*log(basal_area_of_tree_cm2)+
        -0.13099E-03*basal_area_of_tree_cm2+
        +0.16924E+03*(1/(age+10))+
        -0.36209E+04*((1/(age+10))^2)+
        +thinning+
        -0.20862E+00*diameter_quotient+
        -0.40424E+00*BA_quotient_Pine+
        -0.30212E+00*BA_quotient_Spruce+
        +0.12081E-02*spruce*SI100*10+#m to dm.
        +0.15637E-02*pine*SI100*10+ #m to dm.
        +0.12818E+00*peatland+
        +0.92970E+00*latitude+
        -0.71427E-02*(latitude^2)+
        +0.11500E-03*altitude+
        -0.53920E-01*north+
        -0.14903E+00*dry+
        +0.12670E+00*divided_plot+
        +0.18797E+00*fertilised_plot+
        +0.36126E-01*plot_inventoried_76_77+
        -0.33523E+02

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_Sweden_over_45_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    latitude,
    altitude,
    aspect,
    soil_moisture,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  south <- ifelse(aspect%in%c(4,5,6,7),1,0)
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  dry <- ifelse(soil_moisture==1,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.62615E-01*BA.m2_ha+
                         +0.70095E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.48420E-01*BA.m2_ha+
                                +0.46192E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.52634E-01*BA.m2_ha+
                                +0.53348E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10633E+01*log(basal_area_of_tree_cm2)+
        -0.53052E-03*basal_area_of_tree_cm2+
        +0.15859E+03*(1/(age+10))+
        -0.36056E+04*((1/(age+10))^2)+
        thinning+
        -0.31676E+00*diameter_quotient+
        +0.28762E+00*BA_quotient_Spruce+
        +0.34134E+00*BA_quotient_Birch+
        +0.30746E-02*spruce*SI100*10+#m to dm.
        +0.27871E-02*pine*SI100*10+ #m to dm.
        +0.39932E+00*peatland+
        -0.61609E-01*latitude+
        -0.76913E-02*altitude+
        +0.11708E-03*latitude*altitude+
        -0.53450E-01*north+
        +0.53028E-01*south+
        -0.48108E-01*dry+
        +0.14839E+00*divided_plot+
        +0.12531E+00*fertilised_plot+
        +0.10107E+00*plot_inventoried_76_77+
        +0.63632E+00

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_central_Sweden_under_65_Birch <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    vegetation,
    aspect,
    altitude,
    soil_moisture,
    county,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  moist <- ifelse(soil_moisture>3,1,0)
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  herb <- ifelse(vegetation<7,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.72642E-01*BA.m2_ha+
                         +0.11651E-02*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.45533E-01*BA.m2_ha+
                                +0.42222E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.51405E-01*BA.m2_ha+
                                +0.54316E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10356E+01*log(basal_area_of_tree_cm2)+
        +0.11677E+03*(1/(age+10))+
        -0.10643E+04*((1/(age+10))^2)+
        +thinning+
        -0.23670E+00*diameter_quotient+
        -0.69019E+00*BA_quotient_Pine+
        -0.55953E+00*BA_quotient_Spruce+
        -0.71412E+00*BA_quotient_Birch+
        +0.22859E+00*peatland+
        +0.28355E-03*altitude+
        -0.16805E+00*north+
        +0.13365E+00*herb+
        +0.13661E+00*moist+
        +0.45906E-01*divided_plot+
        +0.10209E+00*fertilised_plot+
        +0.24457E+00*plot_inventoried_76_77+
        -0.25124E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_central_Sweden_over_45_Birch <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    vegetation,
    aspect,
    latitude,
    soil_moisture,
    county,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  moist <- ifelse(soil_moisture>3,1,0)
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  herb <- ifelse(vegetation<7,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.64159E-01*BA.m2_ha+
                         +0.96451E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.16802E-01*BA.m2_ha+
                                +0.34084E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.40875E-01*BA.m2_ha+
                                +0.22925E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10653E+01*log(basal_area_of_tree_cm2)+
        +0.20031E+03*(1/(age+10))+
        -0.40583E+04*((1/(age+10))^2)+
        +thinning+
        -0.14923E+01*diameter_quotient+
        +0.80965E+00*(diameter_quotient^2)+
        -0.80277E+00*BA_quotient_Pine+
        -0.66805E+00*BA_quotient_Spruce+
        -0.62539E+00*BA_quotient_Birch+
        +0.35839E+00*peatland+
        +0.10530E+01*latitude+
        -0.81380E-02*(latitude^2)+
        -0.19078E+00*north+
        +0.11808E+00*herb+
        +0.21901E+00*moist+
        +0.18760E+00*divided_plot+
        +0.10200E+00*fertilised_plot+
        +0.15176E+00*plot_inventoried_76_77+
        -0.36815E+02

    )/10000 #cm2 to m2
  )


}


#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_northern_central_Sweden_Broadleaves <- function(
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    aspect,
    latitude,
    altitude,
    divided_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  north <- ifelse(aspect%in%c(8,1,2,3),1,0)
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.2732E-01*BA.m2_ha+
                         +0.32095E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.19331E-01*BA.m2_ha
                            ),

                            0
                     )

  )



  return(
    exp(
      +0.11208E+01*log(basal_area_of_tree_cm2)+
        +0.48728E+02*(1/(age+10))+
        +thinning+
        -0.47542E+00*BA_quotient_Pine+
        +0.24701E+00*BA_quotient_Spruce+
        +0.63271E-01*latitude+
        -0.95194E-05*latitude*altitude+
        -0.27834E+00*north+
        +0.20279E+00*divided_plot+
        +0.88716E-01*plot_inventoried_76_77+
        -0.66924E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_central_Sweden_under_65_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    soil_moisture,
    latitude,
    altitude,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.49683E-01*BA.m2_ha+
                         +0.41229E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.33745E-01*BA.m2_ha+
                                +0.21671E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.38862E-01*BA.m2_ha+
                                +0.17195E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.14711E+01*log(basal_area_of_tree_cm2)+
        -0.69197E-03*basal_area_of_tree_cm2+
        -0.79926E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.12577E+03*(1/(age+10))+
        -0.53325E+03*((1/(age+10))^2)+
        +thinning+
        -0.53502E+00*diameter_quotient+
        -0.63206E+00*BA_quotient_Pine+
        -0.59632E+00*BA_quotient_Spruce+
        -0.41666E+00*BA_quotient_Birch+
        +0.17481E-02*spruce*SI100*10+#m to dm.
        +0.27543E-02*pine*SI100*10+ #m to dm.
        +0.24145E+00*peatland+
        -0.11600E+02*latitude+
        +0.97148E-01*(latitude^2)+
        +0.23556E-01*altitude+
        -0.38030E-03*latitude*altitude+
        +0.13146E+00*moist+
        +0.64887E-01*divided_plot+
        +0.99316E-01*fertilised_plot+
        +0.76914E-01*plot_inventoried_76_77+
        +0.34171E+03

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_central_Sweden_under_65_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    latitude,
    altitude,
    soil_moisture,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.47256E-01*BA.m2_ha+
                         +0.49819E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.34903E-01*BA.m2_ha+
                                +0.40225E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.30434E-01*BA.m2_ha+
                                +0.14407E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.10932E+01*log(basal_area_of_tree_cm2)+
        -0.45888E-03*basal_area_of_tree_cm2+
        -0.40552E+01*(log(basal_area_of_tree_cm2)/(age+10))+
        +0.95220E+02*(1/(age+10))+
        -0.44511E+03*((1/(age+10))^2)+
        +thinning+
        +0.23348E+01*diameter_quotient+
        -0.16233E+01*(diameter_quotient^2)+
        +0.33478E+00*BA_quotient_Spruce+
        +0.25445E-03*spruce*SI100*10+#m to dm.
        +0.80005E-03*pine*SI100*10+ #m to dm.
        +0.17215E+00*peatland+
        -0.36525E-01*altitude+
        +0.59703E-03*latitude*altitude+
        +0.13568E+00*moist+
        +0.16428E+00*divided_plot+
        +0.52583E-01*fertilised_plot+
        +0.19657E+00*plot_inventoried_76_77+
        -0.38356E+01

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_central_Sweden_over_45_Spruce <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    soil_moisture,
    latitude,
    altitude,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.40724E-01*BA.m2_ha+
                         +0.28049E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.32428E-01*BA.m2_ha+
                                +0.25540E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.33682E-01*BA.m2_ha+
                                +0.18766E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.12252E+01*log(basal_area_of_tree_cm2)+
        -0.53489E-03*basal_area_of_tree_cm2+
        +0.18789E+03*(1/(age+10))+
        +0.49425E+04*((1/(age+10))^2)+
        +thinning+
        -0.81637E+00*diameter_quotient+
        +0.40139E+00*(diameter_quotient^2)+
        -0.30428E+00*BA_quotient_Pine+
        -0.18039E+00*BA_quotient_Spruce+
        +0.22559E-02*spruce*SI100*10+#m to dm.
        +0.32384E-02*pine*SI100*10+ #m to dm.
        +0.37674E+00*peatland+
        -0.10388E+02*latitude+
        +0.87352E-01*(latitude^2)+
        +0.30944E-01*altitude+
        -0.50260E-03*latitude*altitude+
        +0.11133E+00*moist+
        +0.90603E-01*divided_plot+
        +0.32526E-01*fertilised_plot+
        +0.30440E+03

    )/10000 #cm2 to m2
  )


}

#' @rdname Soderberg1986BAI5
#' @export
Soderberg_1986_BAI5_central_Sweden_over_45_Pine <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    BA_tree.m2,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    age,
    thinned,
    last_thinned,
    SI100,
    SI_species,
    latitude,
    altitude,
    soil_moisture,
    peatland=0,
    divided_plot=0,
    fertilised_plot=0,
    plot_inventoried_76_77=0
){
  basal_area_of_tree_cm2 <- BA_tree.m2*10000
  spruce <- ifelse(SI_species=="Picea abies")
  pine <- ifelse(SI_species=="Pinus sylvestris")
  moist <- ifelse(soil_moisture>3,1,0)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha

  thinning <- ifelse(thinned==FALSE,
                     (#unthinned
                       -0.58169E-01*BA.m2_ha+
                         +0.59532E-03*(BA.m2_ha^2)

                     )

                     ,

                     ifelse(thinned==TRUE & last_thinned<5,
                            (#thinned within 5 years
                              -0.56116E-01*BA.m2_ha+
                                +0.81799E-03*(BA.m2_ha^2)
                            )


                            ,

                            (#thinned longer ago than 5 years.
                              -0.50915E-01*BA.m2_ha+
                                +0.43750E-03*(BA.m2_ha^2)

                            )


                     )

  )



  return(
    exp(
      +0.90114E+00*log(basal_area_of_tree_cm2)+
        -0.94211E-04*basal_area_of_tree_cm2+
        +0.16635E+03*(1/(age+10))+
        -0.42139E+04*((1/(age+10))^2)+
        thinning+
        +0.23240E+01*diameter_quotient+
        -0.16575E+01*(diameter_quotient^2)+
        +0.37660E+00*BA_quotient_Spruce+
        +0.17996E-02*spruce*SI100*10+#m to dm.
        +0.24084E-02*pine*SI100*10+ #m to dm.
        +0.46113E+00*peatland+
        -0.29434E-01*altitude+
        +0.47369E-03*latitude*altitude+
        +0.85870E-01*moist+
        +0.12523E+00*divided_plot+
        +0.11202E+00*fertilised_plot+
        +0.12541E+00*plot_inventoried_76_77+
        -0.33826E+01

    )/10000 #cm2 to m2
  )


}
#' Double bark function for Individual trees in southern Sweden from
#' Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd,
#' formhöjd och barktjocklek för enskilda träd. / Functions for forest
#' management: Height, form height and bark thickness of individual trees.
#' Report 52. Dept. of Forest Survey. Swedish University of Agricultural
#' Sciences. ISSN 0348-0496.
#'
#' @description
#'
#' \strong{Applicable counties:}
#'  \tabular{cc}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#'
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been
#' distinguished by Sylvén 1917. The distribution of the different forms of
#' Scots Pine is assumed to depend on the expansion-history of the Pine,
#' according to which the Pine immigrated both from the north and the south
#' with a transitional zone in central Sweden, where both forms are present.
#' For other species, the change in genotype is more continuous over the
#' country (Kiellander 1974). For Norway Spruce, it is assumed that it has had
#' time for 30-50 generations in northern Sweden, contrasted against only 10-20
#' in southern Sweden (Kiellander 1966). Therefore the same regional division
#' is used for Norway Spruce as for Scots Pine. For the other species, the
#' regional division is limited to two regions, wherein the northern and central
#' regions have been merged.
#'
#' @details
#' **Beech Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.86 \cr
#' Spread about the function sf \tab 0.255 \cr
#' sf/Spread about the mean \tab 0.52 \cr
#' Number of observations \tab 220 \cr
#' }
#'
#' **Norway Spruce Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.245 \cr
#' sf/Spread about the mean \tab 0.55 \cr
#' Number of observations \tab 7467 \cr
#' }
#'
#' **Birch Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.82 \cr
#' Spread about the function sf \tab 0.354 \cr
#' sf/Spread about the mean \tab 0.58 \cr
#' Number of observations \tab 1766 \cr
#' }
#'
#' **Broadleaves Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.298 \cr
#' sf/Spread about the mean \tab 0.55 \cr
#' Number of observations \tab 837 \cr
#' }
#'
#' **Oak Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.89 \cr
#' Spread about the function sf \tab 0.217 \cr
#' sf/Spread about the mean \tab 0.44 \cr
#' Number of observations \tab 292 \cr
#' }
#'
#' **Scots Pine Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.86 \cr
#' Spread about the function sf \tab 0.238 \cr
#' sf/Spread about the mean \tab 0.50 \cr
#' Number of observations \tab 4628 \cr
#' }
#'
#' **Norway Spruce Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.85 \cr
#' Spread about the function sf \tab 0.237 \cr
#' sf/Spread about the mean \tab 0.53 \cr
#' Number of observations \tab 7470 \cr
#' }
#'
#' **Broadleaves Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.79 \cr
#' Spread about the function sf \tab 0.388 \cr
#' sf/Spread about the mean \tab 0.62 \cr
#' Number of observations \tab 318  \cr
#' }
#'
#' **Birch Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.80 \cr
#' Spread about the function sf \tab 0.293 \cr
#' sf/Spread about the mean \tab 0.60 \cr
#' Number of observations \tab 1747 \cr
#' }
#'
#' **Scots Pine Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.232 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 3832 \cr
#' }
#'
#' **Scots Pine Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.89 \cr
#' Spread about the function sf \tab 0.229 \cr
#' sf/Spread about the mean \tab 0.46 \cr
#' Number of observations \tab 2275 \cr
#' }
#'
#'
#' @param SI100_Pine SIH 100 Pine according to
#' Hägglund 1974. e.g.
#' [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g.
#' [forester::coast_distance]
#' @param DBH.cm Diameter over bark of tree, in cm.
#' @param DBH_largest_tree_on_plot.cm Diameter over bark of the tree with
#' the greatest diameter on the plot, cm.
#' @param total_age Total age of the stand.
#' @param BA_Pine.m2_ha Basal Area m^2/ha of Scots Pine
#' @param BA_Spruce.m2_ha Basal Area m^2/ha of Norway Spruce
#' @param BA_Birch.m2_ha Basal Area m^2/ha of Birch
#' @param BA.m2_ha Basal area m^2/ha on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#' @param county County name.
#'
#' @md
#'
#' @return Double bark thickness, mm.
#' @export
#' @name Soderberg1992doublebark
Soderberg_1992_double_bark_southern_Sweden_Spruce <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2_ha,
    BA_Spruce.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.30355E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.13763E5*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.23539E-4*(total_age^2)+
        -0.13014E-2*SI100_Pine*10+ #SI should be in dm.
        -0.10863E-1*altitude+
        +0.19027E-3*latitude*altitude+
        +0.30230E0*diameter_quotient+
        +0.68055E-1*BA_quotient_Pine+
        -0.10406E0*BA_quotient_Spruce+
        +0.62182E-1*BA_quotient_Birch+
        +0.27539E-1*divided_plot+
        +0.23053E0*close_to_coast+
        +0.36138E1+
        +0.03001 #correction for logarithmic bias, appendix 5.
    )
  )
}

#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_southern_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    latitude,
    altitude,
    divided_plot=0,
    county
){

  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.38360E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.13442E5*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.20965E-2*total_age+
        -0.88795E-5*(total_age^2)+
        -0.74698E-3*SI100_Pine*10+ #SI should be in dm.
        +0.10185E-1*altitude+
        -0.17023E-3*latitude*altitude+
        -0.24281E-1*BA_quotient_Pine+
        -0.49230E-1*BA_quotient_Birch+
        +0.57067E-1*south_eastern_county+
        +0.24619E-1*divided_plot+
        +0.19141E0*close_to_coast+
        +0.47240E1+
        +0.02832 #correction for logarithmic bias, appendix 5.

    )
  )
}


#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_southern_Sweden_Oak <- function(
    DBH.cm,
    total_age,
    county,
    divided_plot
){

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.29605E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.87235E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.22680E-2*total_age+
        -0.24349E0*south_eastern_county+
        +0.44474E-1*divided_plot+
        +0.39521E1+
        +0.02354 #correction for logarithmic bias, appendix 5.
    )
  )
}


#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_southern_Sweden_Broadleaves <- function(
    SI100_Pine,
    DBH.cm,
    total_age
){

  return(
    exp(
      -0.34144E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.97900E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.31101E-2*total_age+
        -0.22562E-4*(total_age^2)+
        -0.21013E-2*SI100_Pine*10+ #SI should be in dm.
        +0.45835E1+
        +0.044402 #Baskerville 1972, logarithmic correction was not included in appendix 5.
    )
  )
}

#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_southern_Sweden_Birch <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2_ha,
    BA_Birch.m2_ha,
    BA.m2_ha,
    latitude,
    altitude,
    county
){

  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.64799E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.33167E5*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.14517E-2*total_age+
        -0.50779E-3*SI100_Pine*10+ #SI should be in dm.
        +0.54445E-2*altitude+
        -0.99383E-4*latitude*altitude+
        +0.13804E0*diameter_quotient+
        +0.88745E-1*BA_quotient_Pine+
        -0.14772E0*BA_quotient_Birch+
        -0.51335E-1*south_eastern_county+
        +0.50104E1+
        +0.04440 #correction for logarithmic bias, appendix 5.
    )
  )
}

#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_southern_Sweden_Beech <- function(
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    county
){

  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.17387E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.22597E-2*total_age+
        +0.16350E0*diameter_quotient+
        -0.26953E0*region5+
        +0.24822E1+
        +0.03251 #correction for logarithmic bias, appendix 5.
    )
  )
}


#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_northern_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2_ha,
    BA.m2_ha,
    latitude,
    altitude
){

  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.40225E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.15037E5*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.44577E-3*total_age+
        -0.15147E-3*SI100_Pine*10+ #SI should be in dm.
        -0.13581E-1*latitude+
        -0.16395E-5*latitude*altitude+
        +0.88075E-1*diameter_quotient+
        -0.11552E-1*(diameter_quotient^2)+
        -0.69739E-1*BA_quotient_Pine+
        -0.82879E-1*close_to_coast+
        +0.53324E1+
        +0.02691 #correction for logarithmic bias, appendix 5.
    )
  )
}

#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_northern_central_Sweden_Spruce <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2_ha,
    BA.m2_ha,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.23633E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.78784E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.13589E-2*total_age+
        +0.62227E-5*(total_age^2)+
        -0.15491E-2*SI100_Pine*10+ #SI should be in dm.
        +0.28379E-1*latitude+
        +0.35929E0*diameter_quotient+
        +0.57123E-1*BA_quotient_Pine+
        +0.20245E-1*divided_plot+
        -0.71409E-1*close_to_coast+
        +0.16604E1+
        +0.02808 #correction for logarithmic bias, appendix 5.
    )
  )
}

#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_northern_central_Sweden_Broadleaves <- function(
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Birch.m2_ha,
    BA.m2_ha,
    divided_plot=0
){

  BA_quotient_Birch <- BA_Birch.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)

  return(
    exp(
      -0.17562E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.49609E-2*total_age+
        +0.26968E0*diameter_quotient+
        +0.29703E0*BA_quotient_Birch+
        -0.77013E-1*divided_plot+
        +0.86920E-1*close_to_coast+
        +0.28446E1+
        +0.075272 #Baskerville 1972, funktion was not include in appendix 5.
    )
  )
}


#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_northern_central_Sweden_Birch <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2_ha,
    BA.m2_ha,
    latitude,
    altitude
){

  BA_quotient_Pine <- BA_Pine.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm

  return(
    exp(
      -0.37131E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.13012E5*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.19655E-2*total_age+
        -0.71109E-3*SI100_Pine*10+ #SI should be in dm.
        +0.86881E-2*latitude+
        +0.62991E-5*latitude*altitude+
        +0.17146E0*diameter_quotient+
        +0.18594E0*BA_quotient_pine+
        +0.31740E1+
        +0.04292 #correction for logarithmic bias, appendix 5.
    )
  )
}

#' @rdname Soderberg1992doublebark
#' @export
Soderberg_1992_double_bark_central_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Spruce.m2_ha,
    BA.m2_ha,
    latitude,
    altitude
){

  BA_quotient_Spruce <- BA_Spruce.m2_ha/BA.m2_ha
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.39422E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.14040E5*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.30388E-3*total_age+
        -0.92527E-3*SI100_Pine*10+ #SI should be in dm.
        -0.64192E-1*latitude+
        -0.31573E-3*altitude+
        +0.12632E0*diameter_quotient+
        -0.46079E-1*(diameter_quotient^2)+
        +0.58621E-1*BA_quotient_Spruce+
        -0.94391E-1*close_to_coast+
        +0.86428E1+
        +0.02622 #correction for logarithmic bias, appendix 5. OBSERVE error in appendix!!
    )
  )
}


#' Form factor function for Individual trees in southern Sweden from
#' Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd,
#' formhöjd och barktjocklek för enskilda träd. / Functions for forest
#' management: Height, form height and bark thickness of individual trees.
#'  Report 52. Dept. of Forest Survey. Swedish University of Agricultural
#'  Sciences. ISSN 0348-0496.
#'
#' @description
#' \strong{Applicable counties:}
#'  \tabular{cc}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#'
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been
#' distinguished by Sylvén 1917. The distribution of the different forms of
#' Scots Pine is assumed to depend on the expansion-history of the Pine,
#' according to which the Pine immigrated both from the north and the south
#' with a transitional zone in central Sweden, where both forms are present.
#' For other species, the change in genotype is more continuous over the
#' country (Kiellander 1974). For Norway Spruce, it is assumed that it has had
#' time for 30-50 generations in northern Sweden, contrasted against only 10-20
#' in southern Sweden (Kiellander 1966). Therefore the same regional division
#' is used for Norway Spruce as for Scots Pine. For the other species, the
#' regional division is limited to two regions, wherein the northern and central
#' regions have been merged.
#'
#' @details
#' **Beech Southern Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.181 \cr
#' sf/Spread about the mean \tab 0.48 \cr
#' Number of observations \tab 220 \cr
#' }
#'
#' **Norway Spruce Southern Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.91 \cr
#' Spread about the function sf \tab 0.152 \cr
#' sf/Spread about the mean \tab 0.41 \cr
#' Number of observations \tab 7467 \cr
#' }
#'
#' **Birch Southern Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.82 \cr
#' Spread about the function sf \tab 0.175 \cr
#' sf/Spread about the mean \tab 0.58 \cr
#' Number of observations \tab 1766 \cr
#' }
#'
#' **Broadleaves Southern Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.77 \cr
#' Spread about the function sf \tab 0.200 \cr
#' sf/Spread about the mean \tab 0.64 \cr
#' Number of observations \tab 837 \cr
#' }
#'
#' **Oak Southern Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.83 \cr
#' Spread about the function sf \tab 0.198 \cr
#' sf/Spread about the mean \tab 0.57 \cr
#' Number of observations \tab 292 \cr
#' }
#'
#' **Scots Pine Southern Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.147 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 4628 \cr
#' }
#'
#' **Norway Spruce Northern or Central Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.91 \cr
#' Spread about the function sf \tab 0.151 \cr
#' sf/Spread about the mean \tab 0.42 \cr
#' Number of observations \tab 7470 \cr
#' }
#'
#' **Broadleaves Northern or Central Sweden** ##
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.85 \cr
#' Spread about the function sf \tab 0.188 \cr
#' sf/Spread about the mean \tab 0.54 \cr
#' Number of observations \tab 318  \cr
#' }
#'
#' **Birch Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.157 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 1747 \cr
#' }
#'
#' **Scots Pine Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.91 \cr
#' Spread about the function sf \tab 0.129 \cr
#' sf/Spread about the mean \tab 0.42 \cr
#' Number of observations \tab 3832 \cr
#' }
#'
#' **Scots Pine Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.92 \cr
#' Spread about the function sf \tab 0.123 \cr
#' sf/Spread about the mean \tab 0.38 \cr
#' Number of observations \tab 2275 \cr
#' }
#'
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km, e.g. [forester::coast_distance]
#' @param DBH.cm Diameter over bark of tree, in cm.
#' @param DBH_largest_tree_on_plot.cm Diameter over bark of the tree with
#' the greatest diameter on the plot, cm.
#' @param total_age Total age of the stand.
#' @param BA_Pine.m2 Basal Area m2 of Scots Pine
#' @param BA_Spruce.m2 Basal Area m2 of Norway Spruce
#' @param BA_Birch.m2 Basal Area m2 of Birch
#' @param BA.m2 Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#' @param county County name.
#'
#' @md
#'
#' @return Form quotient of the tree.
#' @export
#' @name Soderberg1992formfactor
Soderberg_1992_form_factor_southern_Sweden_Spruce <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
    ){

      BA_quotient_Pine <- BA_Pine.m2/BA.m2
      BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
      BA_quotient_Birch <- BA_Birch.m2/BA.m2
      diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
      close_to_coast <- ifelse(distance_to_coast_km<50,1,0)


      return(
        exp(
          -0.20201E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
            +0.16550E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
            +0.39114E-2*total_age+
            -0.24311E-4*(total_age^2)+
            +0.10805E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
            +0.15779E-2*altitude+
            -0.26825E-4*latitude*altitude+
            -0.25400E0*diameter_quotient+
            +0.10089E0*BA_quotient_Pine+
            +0.27052E0*BA_quotient_Spruce+
            +0.64203E-1*BA_quotient_Birch+
            -0.53712E-1*divided_plot+
            -0.79867E-1*close_to_coast+
            +0.23991E1+
            +0.01156 #correction for logarithmic bias, appendix 5.
        )
      )


}

#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_southern_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)

  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.24722E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.96476E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.64050E-2*total_age+
        -0.33916E-4*(total_age^2)+
        +0.16113E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.57111E-2*altitude+
        +0.98668E-4*latitude*altitude+
        +0.17639E0*diameter_quotient+
        -0.30930E0*(diameter_quotient^2)+
        +0.18507E0*BA_quotient_Pine+
        +0.27249E0*BA_quotient_Spruce+
        +0.12120E0*BA_quotient_Birch+
        +0.21324E-1*south_eastern_county+
        -0.62357E-1*divided_plot+
        -0.19831E0*close_to_coast+
        +0.19624E1+
        +0.01080 #correction for logarithmic bias, appendix 5.
    )
  )


}


#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_southern_Sweden_Oak <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){


  BA_quotient_Spruce <- BA_Spruce.m2/(BA.m2)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.24454E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.77370E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.25633E-2*total_age+
        -0.16976E-4*(total_age^2)+
        +0.13153E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.56851E-5*latitude*altitude+
        -0.29397E0*diameter_quotient+
        +0.82213E-1*BA_quotient_Spruce+
        +0.26924E0*south_eastern_county+
        -0.65403E-2*region5+
        -0.77845E-1*divided_plot+
        +0.25409E1+
        +0.01960 #correction for logarithmic bias, appendix 5.

    )
  )


}


#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_southern_Sweden_Broadleaves <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){


  BA_quotient_Pine <- BA_Pine.m2/(BA.m2)
  BA_quotient_Spruce <- BA_Spruce.m2/(BA.m2)
  BA_quotient_Birch <- BA_Birch.m2/(BA.m2)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm

  return(
    exp(
      -0.15868E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.30541E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.45148E-2*total_age+
        -0.34685E-4*(total_age^2)+
        +0.83659E-3*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.11257E-1*altitude+
        +0.19625E-3*latitude*altitude+
        -0.16890E0*diameter_quotient+
        -0.18665E0*BA_quotient_Pine+
        +0.93429E-1*BA_quotient_Spruce+
        -0.74098E-1*BA_quotient_Birch+
        -0.47553E-1*divided_plot+
        +0.22560E1+
        +((0.200^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
    )
  )


}

#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_southern_Sweden_Birch <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)


  return(
    exp(
      -0.10414E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        -0.26202E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.15775E-2*total_age+
        -0.10844E-4*(total_age^2)+
        +0.94915E-3*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.10506E-1*altitude+
        +0.18430E-3*latitude*altitude+
        -0.32432E0*diameter_quotient+
        -0.49356E-1*BA_quotient_Pine+
        +0.12381E0*BA_quotient_Spruce+
        +0.11830E0*BA_quotient_Birch+
        +0.45225E-1*south_eastern_county+
        -0.68294E-1*divided_plot+
        +0.22298E1+
        +0.02000 #correction for logarithmic bias, appendix 5.
    )
  )


}

#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_southern_Sweden_Beech <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){


  BA_quotient_Spruce <- BA_Spruce.m2/(BA.m2)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.10532E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.65517E-2*total_age+
        -0.16776E-4*(total_age^2)+
        -0.52081E-3*SI100_Pine*10+ #SI 100 Pine should be in dm.
        -0.42320E-5*latitude*altitude+
        +0.14651E0*diameter_quotient+
        +0.20009E0*BA_quotient_Spruce+
        +0.19265E0*south_eastern_county+
        -0.16720E0*region5+
        -0.17627E0*divided_plot+
        +0.20763E1+
        +0.01638 #correction for logarithmic bias, appendix 5.
    )
  )


}


#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_northern_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)

  return(
    exp(
      -0.24776E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.75785E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.52773E-2*total_age+
        -0.24395E-4*(total_age^2)+
        +0.10773E-2*SI100_Pine*10+ #SI should be in dm.
        -0.15516E-1*latitude+
        -0.43763E-5*latitude*altitude+
        -0.36728E0*diameter_quotient+
        +0.56762E-1*(diameter_quotient^2)
      +0.74321E-1*BA_quotient_Pine+
        -0.52502E-1*divided_plot+
        -0.59471E-1*close_to_coast+
        +0.36491E1+
        +0.00832 #correction for logarithmic bias, appendix 5.
    )
  )


}

#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_northern_central_Sweden_Spruce <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)

  return(
    exp(
      -0.21522E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.32488E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.40044E-2*total_age+
        -0.20320E-4*(total_age^2)+
        +0.11681E-2*SI100_Pine*10+ #SI should be in dm.
        -0.11238E-1*latitude+
        +0.57508E-3*altitude+
        -0.14149E-4*latitude*altitude+
        -0.21199E0*diameter_quotient+
        +0.58171E-1*BA_quotient_Pine+
        +0.10093E0*BA_quotient_Spruce+
        -0.35409E-1*divided_plot+
        -0.66759E-1*close_to_coast+
        +0.32511E1+
        +0.01140 #correction for logarithmic bias, appendix 5.
    )
  )


}

#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_northern_central_Sweden_Broadleaves <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Birch <- BA_Birch.m2/(BA.m2)
  BA_quotient_Spruce <- BA_Spruce.m2/(BA.m2)
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)

  return(
    exp(
      -0.12623E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.42804E-2*total_age+
        -0.25316E-4*(total_age^2)+
        +0.16703E-2*SI100_Pine*10+ #SI should be in dm.
        -0.32185E-1*latitude+
        -0.27431E-4*altitude+
        -0.85686E-1*diameter_quotient+
        +0.68224E-1*BA_quotient_Spruce+
        -0.51989E-1*BA_quotient_Birch+
        -0.77704E-1*divided_plot+
        +0.13794E0*close_to_coast+
        +0.39069E1+
        +((0.188^2)/2) #Baskerville 1972, logarithmic correction was not included in appendix 5.
    )
  )


}


#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_northern_central_Sweden_Birch <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm

  return(
    exp(
      -0.23369E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.66940E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.32512E-2*total_age+
        -0.22072E-4*(total_age^2)+
        +0.86739E-3*SI100_Pine*10+ #SI should be in dm.
        -0.20822E-1*latitude+
        -0.74028E-5*latitude*altitude+
        -0.38620E0*diameter_quotient+
        +0.70742E-1*(diameter_quotient^2)+
        -0.82056E-1*BA_quotient_Pine+
        -0.21952E-1*BA_quotient_Spruce+
        -0.39112E-1*divided_plot+
        +0.41540E1+
        +0.01232 #correction for logarithmic bias, appendix 5.
    )
  )


}


#' @rdname Soderberg1992formfactor
#' @export
Soderberg_1992_form_factor_central_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast_km<50,1,0)

  return(
    exp(
      -0.25627E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.77018E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.47410E-2*total_age+
        -0.21793E-4*(total_age^2)+
        +0.13003E-2*SI100_Pine*10+ #SI 100 Pine should be in dm.
        +0.81039E-1*latitude+
        +0.75022E-2*altitude+
        -0.12694E-3*latitude*altitude+
        -0.50469E0*diameter_quotient+
        +0.10610E0*(diameter_quotient^2)+
        +0.15691E0*BA_quotient_Pine+
        +0.17465E0*BA_quotient_Spruce+
        +0.17342E0*BA_quotient_Birch+
        -0.48782E-1*divided_plot+
        -0.83240E-1*close_to_coast+
        -0.23076E1+
        +0.00756 #correction for logarithmic bias, appendix 5.
    )
  )


}
#' Height function for Individual trees in Sweden from Söderberg 1992.
#'
#' @source Söderberg, U. (1992) Funktioner för skogsindelning: Höjd,
#' formhöjd och barktjocklek för enskilda träd. / Functions for forest
#' management: Height, form height and bark thickness of individual trees.
#' Report 52. Dept. of Forest Survey. Swedish University of Agricultural
#' Sciences. ISSN 0348-0496.
#'
#' @description
#' \strong{Applicable counties:}
#'  \tabular{lr}{
#'  Northern Sweden \tab \cr
#' \tab Norrbottens lappmark \cr
#' \tab Norrbottens kustland \cr
#' \tab Västerbottens lappmark \cr
#' \tab Västerbottens kustland \cr
#' \tab Västernorrland - Ångermanlands landskap \cr
#' \tab Västernorrland - Medelpads landskap \cr
#' \tab Jämtland - Jämtlands landskap \cr
#' \tab Jämtland - Härjedalens landskap \cr
#' \tab Kopparberg - Sälen-Idre. \cr
#'
#'  Central Sweden \tab \cr
#'  \tab Kopparberg - övriga \cr
#'  \tab Gävleborg - Hälsinglands landskap \cr
#'  \tab Gävleborg - övriga \cr
#'  \tab Värmland \cr
#'
#'  Southern Sweden \tab \cr
#'  \tab Stockholm \cr
#'  \tab Uppsala \cr
#'  \tab Västmanland \cr
#'  \tab Södermanland \cr
#'  \tab Örebro \cr
#'  \tab Östergötland \cr
#'  \tab Skaraborg \cr
#'  \tab Älvsborg - Västergötlands landskap \cr
#'  \tab Älvsborg - Dalslands landskap \cr
#'  \tab Jönköping \cr
#'  \tab Kronoberg \cr
#'  \tab Kalmar \cr
#'  \tab Halland \cr
#'  \tab Kristianstad \cr
#'  \tab Malmöhus \cr
#'  \tab Blekinge \cr
#'  \tab Gotland \cr
#'  }
#'
#' \strong{Regarding the regional division of the models:}
#'
#' Freely translated from Söderberg 1986 p. 29:
#' 'The regional division is motivated in the following.
#' For Pine the country has been divided into 3 regions (see figure 4.1),
#' which are based on the different forms of Scots Pine which have been distinguished
#' by Sylvén 1917. The distribution of the different forms of Scots Pine is assumed
#' to depend on the expansion-history of the Pine, according to which the Pine
#' immigrated both from the north and the south with a transitional zone in central
#' Sweden, where both forms are present. For other species, the change in genotype
#' is more continuous over the country (Kiellander 1974). For Norway Spruce, it is
#' assumed that it has had time for 30-50 generations in northern Sweden, contrasted
#' against only 10-20 in southern Sweden (Kiellander 1966). Therefore the same regional
#' division is used for Norway Spruce as for Scots Pine. For the other species, the regional
#' division is limited to two regions, wherein the northern and central regions have been merged.
#'
#' @details
#' **Beech Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.92 \cr
#' Spread about the function sf \tab 0.161 \cr
#' sf/Spread about the mean \tab 0.41 \cr
#' Number of observations \tab 220 \cr
#' }
#'
#' **Norway Spruce Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.94 \cr
#' Spread about the function sf \tab 0.148 \cr
#' sf/Spread about the mean \tab 0.33 \cr
#' Number of observations \tab 7470 \cr
#' }
#'
#' **Norway Spruce Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.95 \cr
#' Spread about the function sf \tab 0.145 \cr
#' sf/Spread about the mean \tab 0.32 \cr
#' Number of observations \tab 7467 \cr
#' }
#'
#' **Birch Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.90 \cr
#' Spread about the function sf \tab 0.158 \cr
#' sf/Spread about the mean \tab 0.43 \cr
#' Number of observations \tab 1747 \cr
#' }
#'
#' **Birch Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.89 \cr
#' Spread about the function sf \tab 0.164 \cr
#' sf/Spread about the mean \tab 0.46 \cr
#' Number of observations \tab 1766 \cr
#' }
#'
#' **Broadleaves Northern or Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.191 \cr
#' sf/Spread about the mean \tab 0.48 \cr
#' Number of observations \tab 318 \cr
#' }
#' **Broadleaves Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.84 \cr
#' Spread about the function sf \tab 0.195 \cr
#' sf/Spread about the mean \tab 0.54 \cr
#' Number of observations \tab 837 \cr
#' }
#'
#' **Oak Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.88 \cr
#' Spread about the function sf \tab 0.178 \cr
#' sf/Spread about the mean \tab 0.47 \cr
#' Number of observations \tab 292 \cr
#' }
#'
#' **Scots Pine Northern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.92 \cr
#' Spread about the function sf \tab 0.152 \cr
#' sf/Spread about the mean \tab 0.39 \cr
#' Number of observations \tab 3832 \cr
#' }
#'
#'
#' **Scots Pine Central Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.94 \cr
#' Spread about the function sf \tab 0.137 \cr
#' sf/Spread about the mean \tab 0.34 \cr
#' Number of observations \tab 2275 \cr
#' }
#'
#' **Scots Pine Southern Sweden**
#' \tabular{ll}{
#' Multiple correlation coefficient R \tab 0.92 \cr
#' Spread about the function sf \tab 0.159 \cr
#' sf/Spread about the mean \tab 0.40 \cr
#' Number of observations \tab 4628 \cr
#' }
#'
#'
#' @param SI100_Pine SIH 100 Pine according to Hägglund 1974.
#' e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#' @param distance_to_coast_km Closest distance to coast, in km,
#' e.g. [forester::coast_distance]
#' @param DBH.cm Diameter over bark of tree, in cm.
#' @param DBH_largest_tree_on_plot.cm Diameter over bark of the tree
#' with the greatest diameter on the plot, cm.
#' @param total_age Total age of the stand.
#' @param BA_Pine.m2 Basal Area m2 of Scots Pine
#' @param BA_Spruce.m2 Basal Area m2 of Norway Spruce
#' @param BA_Birch.m2 Basal Area m2 of Birch
#' @param BA.m2 Basal area m2 on plot.
#' @param latitude Latitude, degrees.
#' @param altitude Altitude, metres.
#' @param divided_plot 0 for full plots, 1 for divided plots.
#' @param county County name.
#'
#' @md
#'
#' @return Height of tree, m.
#' @export
#' @name Soderberg1992height
Soderberg_1992_height_southern_Sweden_Beech <- function(
  DBH.cm,
  total_age,
  BA_Spruce.m2,
  BA.m2,
  latitude,
  altitude,
  divided_plot=0,
  county
){


  BA_quotient_Spruce <- BA_Spruce.m2/(BA.m2)
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.14407E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.72319E-2*total_age+
        -0.27244E-4*(total_age^2)+
        -0.57810E-5*latitude*altitude+
        +0.18040E0*BA_quotient_Spruce+
        +0.18800E0*south_eastern_county+
        -0.18416E0*region5+
        -0.17410E0*divided_plot+
        +0.52974E1+
        +0.01296 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )


}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_southern_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)


  return(
    exp(
      -0.30345E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.88427E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.68724E-2*total_age+
        -0.38585E-4*(total_age^2)+
        +0.16646E-2*SI100_Pine*10+ #SI should be in dm.
        -0.47335E-2*altitude+
        +0.82679E-4*latitude*altitude+
        +0.91429E-1*diameter_quotient+
        -0.28115E0*(diameter_quotient^2)+
        +0.20570E0*BA_quotient_Pine+
        +0.29485E0*BA_quotient_Spruce+
        +0.13909E0*BA_quotient_Birch+
        +0.36444E-1*south_eastern_county+
        -0.60312E-1*divided_plot+
        -0.19855E0*close_to_coast+
        +0.52706E1+
        +0.01264 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_southern_Sweden_Birch <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)

  return(
    exp(
      -0.22552E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.39171E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.17264E-2*total_age+
        -0.11572E-4*(total_age^2)+
        +0.89953E-3*SI100_Pine*10+ #SI should be in dm.
        -0.90184E-2*altitude+
        +0.15804E-3*latitude*altitude+
        -0.32296E0*diameter_quotient+
        -0.44799E-1*BA_quotient_Pine+
        +0.11728E0*BA_quotient_Spruce+
        +0.10104E0*BA_quotient_Birch+
        +0.42911E-1*southern_sweden+
        -0.68048E-1*divided_plot+
        +0.57820E1+
        +0.01901 #correction for logarithmic bias, appendix 5.
    )# Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_southern_Sweden_Broadleaves <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm


  return(
    exp(
      -0.22078E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.53920E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.53701E-2*total_age+
        -0.41932E-4*(total_age^2)+
        +0.53968E-3*SI100_Pine*10+ #SI should be in dm.
        -0.10758E-1*altitude+
        +0.18781E-3*latitude*altitude+
        -0.17045E0*diameter_quotient+
        -0.17291E0*BA_quotient_Pine+
        +0.10783E0*BA_quotient_Spruce+
        -0.55868E-1*BA_quotient_Birch+
        -0.51870E-1*divided_plot+
        +0.56569E1+
        +((0.195^2)/2) #Baskerville 1972, funktion was not include in appendix 5.
    )/10 # Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_southern_Sweden_Oak <- function(
    SI100_Pine,
    DBH.cm,
    total_age,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0,
    county
){


  BA_quotient_Spruce <- BA_Spruce.m2/(BA.m2)
  south_eastern_county <- ifelse(county %in% c("Stockholm","Södermanland","Uppsala","Östergötland","Kalmar","Västmanland"),1,0)
  region5 <- ifelse(county %in% c("Blekinge", "Kristianstad", "Malmöhus", "Västra Götaland", "Halland", "Gotland"),1,0)

  return(
    exp(
      -0.25811E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.63100E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.13039E-2*SI100_Pine*10+ #SI should be in dm.
        -0.41543E-5*latitude*altitude+
        -0.32505E0*diameter_quotient+
        +0.59855E-1*BA_quotient_Spruce+
        +0.17355E0*south_eastern_county+
        -0.47987E-1*region5+
        -0.69304E-1*divided_plot+
        +0.57884E1+
        +0.01584 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )


}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_southern_Sweden_Spruce <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.27421E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.38013E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.31094E-2*total_age+
        -0.20764E-4*(total_age^2)+
        +0.10161E-2*SI100_Pine*10+ #SI should be in dm.
        +0.15166E-2*altitude+
        -0.25385E-4*latitude*altitude+
        -0.23760E0*diameter_quotient+
        +0.10172E0*BA_quotient_Pine+
        +0.24012E0*BA_quotient_Spruce+
        +0.68141E-1*BA_quotient_Birch+
        -0.47848E-1*divided_plot+
        -0.69386E-1*close_to_coast+
        +0.57495E1+
        +0.01051 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_central_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA_Birch.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  BA_quotient_Birch <- BA_Birch.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.29249E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.61832E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.52675E-2*total_age+
        -0.25358E-4*(total_age^2)+
        +0.13721E-2*SI100_Pine*10+ #SI should be in dm.
        +0.69771E-1*latitude+
        +0.58106E-2*altitude+
        -0.10018E-3*latitude*altitude+
        -0.61165E0*diameter_quotient+
        +0.13132E0*(diameter_quotient^2)+
        -0.22217E0*BA_quotient_Pine+
        +0.24504E0*BA_quotient_Spruce+
        +0.23251E0*BA_quotient_Birch+
        -0.55749E-1*divided_plot+
        -0.10186E0*close_to_coast+
        +0.15712E1+
        +0.00938 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_northern_central_Sweden_Birch <- function(
    SI100_Pine,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm


  return(
    exp(
      -0.26607E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.71415E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.32789E-2*total_age+
        -0.22514E-4*(total_age^2)+
        +0.85255E-3*SI100_Pine*10+ #SI should be in dm.
        -0.18462E-1*latitude+
        -0.72180E-5*latitude*altitude+
        -0.39250E0*diameter_quotient+
        +0.76500E-1*(diameter_quotient^2)+
        -0.74398E-1*BA_quotient_Pine+
        -0.22539E-1*BA_quotient_Spruce+
        -0.35918E-1*divided_plot+
        +0.72446E1+
        +0.01248 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_northern_central_Sweden_Broadleaves <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    total_age,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    divided_plot=0
){

  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.14546E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.53659E-2*total_age+
        -0.29042E-4*(total_age^2)+
        +0.17639E-2*SI100_Pine*10+ #SI should be in dm.
        -0.34200E-1*latitude+
        +0.75841E-1*BA_quotient_Spruce+
        -0.82953E-1*divided_plot+
        +0.15566E0*close_to_coast+
        +0.70706E1+
        +((0.191^2)/2) #Baskerville 1972, logarithmic correction was not included in appendix 5.
    )
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_northern_central_Sweden_Spruce <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA_Spruce.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  BA_quotient_Spruce <- BA_Spruce.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.28663E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.47831E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.31669E-2*total_age+
        -0.16854E-4*(total_age^2)+
        +0.10855E-2*SI100_Pine*10+ #SI should be in dm.
        -0.99681E-2*latitude+
        +0.51262E-3*altitude+
        -0.12449E-4*latitude*altitude+
        -0.19831E0*diameter_quotient+
        +0.60923E-1*BA_quotient_Pine+
        +0.90784E-1*BA_quotient_Spruce+
        -0.30688E-1*divided_plot+
        -0.62548E-1*close_to_coast+
        +0.65200E1+
        +0.01095 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}

#' @export
#' @name Soderberg1992height
Soderberg_1992_height_northern_Sweden_Pine <- function(
    SI100_Pine,
    distance_to_coast_km,
    DBH.cm,
    DBH_largest_tree_on_plot.cm,
    total_age,
    BA_Pine.m2,
    BA.m2,
    latitude,
    altitude,
    divided_plot=0
){

  BA_quotient_Pine <- BA_Pine.m2/BA.m2
  diameter_quotient <- DBH.cm/DBH_largest_tree_on_plot.cm
  close_to_coast <- ifelse(distance_to_coast<50,1,0)


  return(
    exp(
      -0.28390E3*(1/((DBH.cm*10)+50))+#Diameter cm should be in mm.
        +0.64168E4*((1/((DBH.cm*10)+50))^2)+ #Diameter cm should be in mm.
        +0.63874E-2*total_age+
        -0.30707E-4*(total_age^2)+
        +0.12774E-2*SI100_Pine*10+ #SI should be in dm.
        -0.15597E-1*latitude+
        -0.48527E-5*latitude*altitude+
        -0.44962E0*diameter_quotient+
        +0.70355E-1*(diameter_quotient^2)+
        +0.87350E-1*(BA_quotient_Pine)+
        -0.56157E-1*divided_plot+
        -0.72392E-1*close_to_coast+
        +0.68125E1+
        +0.01155 #correction for logarithmic bias, appendix 5.
    )/10 # Return height in meters, not dm.
  )
}
