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
#' @param soil_moisture 1-5. e.g. [forester::Sweden_soil_types('moisture')]
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
#'  @param county Character string. One of:
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
