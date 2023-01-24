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
