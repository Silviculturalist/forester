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
#' @param soil_moisture 1-5. e.g. [forester::Sweden_soil_types('moisture')]
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
