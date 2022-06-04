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


