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
