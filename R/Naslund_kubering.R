#' Funktioner och Tabeller för kubering av stående träd.
#' @source Northern Sweden: Näslund, Manfred. 1941. Funktioner och tabeller för
#' kubering av stående träd. Tall, gran och björk i norra Sverige. Meddelanden
#' från statens skogsförsöksanstalt. Häfte 32. Nr. 4. Centraltryckeriet, Esselte,
#'  Stockholm. p. 87-142 pp. \url{https://pub.epsilon.slu.se/10185/1/medd_statens_skogsforskningsanst_032_04.pdf}
#' @source Southern Sweden and entire country: Näslund, Manfred. 1947. Funktioner
#' och tabeller för kubering av stående träd. Tall, gran och björk i södra Sverige samt i hela landet.
#' Meddelanden från statens skogsforskningsinstitut. Band 36. Nr. 3. Centraltryckeriet,
#' Esselte, Stockholm. p. 1-80. \url{https://pub.epsilon.slu.se/9900/1/medd_statens_skogsforskningsinst_036_03.pdf}
#'
#' @details If only required for more detailed function, arguments will be default=NULL.
#'
#' @param double_bark_mm Double bark thickness, mm.
#' @param diameter_cm Diameter at breast height, over or respectively under bark.
#' @param height_m Tree height, meters.
#' @param crownBaseHeight_m Height until crown base.
#' @param over_bark Boolean. If input diameter is over/under bark, and if returned value should be over or under bark.
#' @return Form factor (unspecified.) or Volume, dm^3.
#'
#'
#' @name NaslundKubering
#' @export

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_southern_Sweden_Pine <- function(height_m,diameter_cm, double_bark_mm, crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      volume = 0.1193*diameter_cm^2 + 0.02574*height*diameter_cm^2 + 0.007262*crownBaseHeight_m*diameter_cm^2 + 0.004054*diameter_cm*height_m^2 - 0.003112*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.1072*diameter_cm^2 + 0.02427*height*diameter_cm^2 + 0.007315*diameter_cm*height_m^2
    }
  } else {
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      volume = 0.07141*diameter_cm^2 + 0.02580*height*diameter_cm^2 + 0.009430*crownBaseHeight_m*diameter_cm^2 + 0.003511*diameter_cm*height_m^2 + 0.001052*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.06271*diameter_cm^2 + 0.03208*height*diameter_cm^2 + 0.005725*diameter_cm*height^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_southern_Sweden_Pine <- function(height_m,diameter_cm,double_bark_mm,crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 420.16 + 1519.24*(1/height_m) + 51.62*(height_m/diameter_cm) - 3.962*B - 0.9246*K

    } else {
      form = 308.97 + 1365.38*(1/height_m)+ 93.14*(height_m/diameter_cm)
    }

  } else {
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 448.53 + 909.21*(1/height_m)+ 44.71*(height_m/diameter_cm) + 1.339*B - 1.201*K

    } else {
      form = 408.49 + 798.46*(1/height_m) + 72.89*(height_m/diameter_cm)

    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_southern_Sweden_Spruce <- function(height_m,diameter_cm, crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m)){
      volume = 0.1059*diameter_cm^2 + 0.01968*height*diameter_cm^2 + 0.006168*crownBaseHeight_m*diameter_cm^2 + 0.01468*diameter_cm*height_m^2 - 0.04585*height_m^2
    } else {
      volume = 0.1104*diameter_cm^2 + 0.01928*height*diameter_cm^2 + 0.01815*diameter_cm*height_m^2 - 0.04936*height^2
    }
  } else {
    if(!is.null(crownBaseHeight_m)){
      volume = 0.1039*diameter_cm^2 + 0.01959*height*diameter_cm^2 + 0.005942*crownBaseHeight_m*diameter_cm^2 + 0.01417*diameter_cm*height_m^2 - 0.04332*height_m^2
    } else {
      volume = 0.1076*diameter_cm^2 + 0.01929*height*diameter_cm^2 + 0.01723*diameter_cm*height^2 - 0.04615*height_m^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_southern_Sweden_Spruce <- function(height_m,diameter_cm,crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      form = 329.09 + 1348.92*(1/height_m) + 186.94*(height_m/diameter_cm) - 583.74*height_m/diameter_cm^2 - 0.7854*K

    } else {
      form = 245.09 + 1405.66*(1/height_m)+ 231.11*(height_m/diameter_cm)-628.48*height_m/diameter_cm^2
    }

  } else {
    if(!is.null(crownBaseHeight_m)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      form = 325.04 + 1322.62*(1/height_m)+ 180.36*(height_m/diameter_cm) - 551.55*height_m/diameter_cm^2 - 0.7566*K

    } else {
      form = 245.57 + 1369.89*(1/height_m) + 219.34*(height_m/diameter_cm) - 587.65*height_m/diameter_cm^2

    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_southern_Sweden_Birch <- function(height_m,diameter_cm, double_bark_mm=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      volume = 0.09595*diameter_cm^2 + 0.02375*height*diameter_cm^2 + 0.01221*diameter_cm*height_m^2 - 0.03636*height_m^2 - 0.004605*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.1432*diameter_cm^2 + 0.008561*height*diameter_cm^2 + 0.02180*diameter_cm*height_m^2 - 0.06630*height^2
    }
  } else {
    if(!is.null(double_bark_mm)){
      volume = 0.08953*diameter_cm^2 + 0.02101*height*diameter_cm^2 + 0.01171*diameter_cm*height_m^2 - 0.03189*height_m^2 - 0.0007244*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.09944*diameter_cm^2 + 0.01862*height*diameter_cm^2 + 0.01278*diameter_cm*height^2 - 0.03544*height_m^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_southern_Sweden_Birch <- function(height_m,diameter_cm,double_bark_mm=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 302.45 + 1221.63*(1/height_m) + 155.44*(height_m/diameter_cm) - 462.95*height_m/diameter_cm^2 - 5.864*B

    } else {
      form = 109.01 + 1823.03*(1/height_m)+ 277.56*(height_m/diameter_cm)-844.17*height_m/diameter_cm^2
    }

  } else {
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 267.44 + 1139.98*(1/height_m)+ 149.04*(height_m/diameter_cm) - 406.04*height_m/diameter_cm^2 - 0.9224*B

    } else {
      form = 237.03 + 1266.05*(1/height_m) + 162.72*(height_m/diameter_cm) - 451.26*height_m/diameter_cm^2

    }
  }

  return(form)
}


#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_Sweden_Spruce <- function(height_m,diameter_cm,double_bark_mm=NULL,crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      volume = 0.1121*diameter_cm^2 + 0.02919*height*diameter_cm^2 + 0.006285*crownBaseHeight_m*diameter_cm^2 + 0.002460*diameter_cm*height_m^2 - 0.003574*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.1028*diameter_cm^2 + 0.02705*height*diameter_cm^2 + 0.005215*diameter_cm*height_m^2
    }
  } else {
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      volume = 0.06791*diameter_cm^2 + 0.02989*height*diameter_cm^2 + 0.007555*crownBaseHeight_m*diameter_cm^2 + 0.002063*diameter_cm*height_m^2 + 0.0006544*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.06045*diameter_cm^2 + 0.03413*height*diameter_cm^2 + 0.004163*diameter_cm*height^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_Sweden_Pine <- function(height_m,diameter_cm,double_bark_mm=NULL,crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 451.70 + 1426.70*(1/height_m) + 31.33*(height_m/diameter_cm) - 4.551*B - 0.8002*K

    } else {
      form = 344.40 + 1308.56*(1/height_m)+ 66.40*(height_m/diameter_cm)
    }

  } else {
    if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 476.82 + 864.68*(1/height_m)+ 26.26*(height_m/diameter_cm) + 0.8332*B - 0.9619*K

    } else {
      form = 434.54 + 769.61*(1/height_m) + 53.01*(height_m/diameter_cm)

    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_Sweden_Spruce <- function(height_m,diameter_cm,crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      form = 309.76 + 1385.38*(1/height_m)+208.29*(height_m/diameter_cm) - 640.41*(height_m/diameter_cm^2)-0.7477*K

    } else {
      form = 222.30 + 1463.59*(1/height_m) + 257.46*(height_m/diameter_cm) - 715.27*(height_m/diameter_cm^2)
    }

  } else {
    if(!is.null(crownBaseHeight_m)){
      K = ((height_m-crownBaseHeight_m)/height_m)*100
      form = 306.60 + 1363.31*(1/height_m) + 199.71*(height_m/diameter_cm) - 591.81*(height_m/diameter_cm^2) - 0.7403*K

    } else {

      form = 221.51 + 1431.21*(1/height_m)+ 244.14*height_m/diameter - 652.09*height_m/diameter^2
    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_Sweden_Spruce <- function(height_m,diameter_cm,crownBaseHeight_m=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseHeight_m)){
      volume = 0.1088*diameter_cm^2 + 0.01846*height*diameter_cm^2 + 0.005873*crownBaseHeight_m*diameter_cm^2 + 0.01636*diameter_cm*height_m^2 - 0.05030*height_m^2

    } else {
      volume = 0.1150*diameter_cm^2 + 0.01746*height_m*diameter_cm^2 + 0.02022*diameter_cm*height_m^2 - 0.05618*height_m^2
    }

  } else {
    if(!is.null(crownBaseHeight_m)){
      volume = 0.1071*diameter_cm^2 + 0.01827*height_m*diameter_cm^2 + 0.005814*crownBaseHeight_m*diameter_cm^2 + 0.01569*diameter_cm*height_m^2 - 0.04648*height_m^2
    } else {
      volume = 0.1124*diameter_cm^2 + 0.01740*height_m*diameter_cm^2 + 0.01917*diameter_cm*height_m^2 - 0.05122*height_m^2
    }
  }
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_Sweden_Birch <- function(height_m,diameter_cm,double_bark_mm=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 323.27 + 1110.48*(1/height_m) + 135.64*height_m/diameter_cm - 411.05*height_m/diameter_cm^2 - 5.533*B
    } else {
      form = 170.33 + 1661.17*(1/height_m) + 223.71*height_m/diameter_cm - 713.75*height_m/diameter_cm^2
    }

  } else {
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/diameter_cm)*100
      form = 309.17 + 922.45*(1/height_m) + 119.90*height_m/diameter_cm - 307.69*height_m/diameter_cm^2 - 1.168*B

    } else {
      form = 277.52 + 1064.41*(1/height_m) + 132.93*height_m/diameter_cm - 356.08*height_m/diameter_cm^2
    }
  }
  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_Sweden_Birch <- function(height_m,diameter_cm,double_bark_mm=NULL, over_bark=TRUE){
  if(diameter_cm<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      volume = 0.08722*diameter_cm^2 + 0.02539*height_m*diameter_cm^2 + 0.01065*diameter_cm*height_m^2 - 0.03228*height_m^2 - 0.004346*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.1305*diameter_cm^2 + 0.01338*height_m*diameter_cm^2 + 0.01757*diameter_cm*height_m^2 - 0.05606*height_m^2
    }

  } else {
    if(!is.null(double_bark_mm)){
      volume = 0.07245*diameter_cm^2 + 0.02428*height_m*diameter_cm^2 + 0.009417*diameter_cm*height_m^2 - 0.02417*height_m^2 - 0.0009177*diameter_cm*height_m*double_bark_mm
    } else {
      volume = 0.08360*diameter_cm^2 + 0.02180*height*diameter_cm^2 + 0.01044*diameter_cm*height_m^2 - 0.02797*height_m^2

    }
  }
  return(volume)
}


#' @rdname NaslundKubering
#' @export
Naslund_1940_volume_northern_Sweden_Pine<- function(height_m,diameter_cm,double_bark_mm=NULL,crownBaseHeight_m=NULL,over_bark=TRUE)
{

  if(over_bark==TRUE){
    if(diameter_cm>=5)
    {
      if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm))
      {
        volume = 0.1018*diameter_cm^2 + 0.03112*diameter_cm^2*height_m + 0.007312*diameter_cm^2*crownBaseHeight_m-0.002906*diameter_cm*height_m*double_bark_mm
      } else
      {
        volume = 0.09314*diameter_cm^2 + 0.03069*diameter_cm^2*height_m + 0.002818*diameter_cm*height_m^2
      }

    } else {
      volume = 0.22 + 0.0504*diameter_cm^2*height_m
    }
  } else {
    if(diameter_cm>=5)
    {
      if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm))
      {
        volume = 0.06059*diameter_cm^2 + 0.03153*diameter_cm^2*height_m + 0.007919*diameter_cm^2*crownBaseHeight_m+0.001773*diameter_cm*height_m*double_bark_mm
      } else {
        volume = 0.05491*diameter_cm^2 + 0.03641*diameter_cm^2*height_m + 0.002699*diameter_cm*height_m^2
      }
    } else {
      volume  = 0.15 + 0.0488*diameter_cm^2*height_m
    }
  }

  return(volume)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_form_factor_northern_Sweden_Pine<- function(height_m,diameter_cm,double_bark_mm=NULL,crownBaseHeight_m=NULL,over_bark=TRUE)
{

  if(diameter_cm>=5){
    if(over_bark==TRUE){
      if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm))
      {
        K = ((height_m-crownBaseHeight_m)/height_m)*100
        B = ((double_bark_mm/10)/diameter_cm)*100
        form = 489.35+1296.11*(1/height_m) - 3700*B - 0.9310*K
      } else
      {
        form = 390.81+1185.86*(1/height_m) + 35.88*(height_m/diameter_cm)
      }
    } else {
      if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm))
      {
        K = ((height_m-crownBaseHeight_m)/height_m)*100
        B = ((double_bark_mm/10)/diameter_cm)*100
        form = 502.22 + 771.5*(1/height_m) + 2.257*B - 1.008*K
      } else
      {
        form = 463.55 + 699.14*(1/height_m) + 34.36*(height_m/diameter_cm)
      }
    }
  } else {
    stop("diameter_cm must be >=5 cm.")
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_volume_northern_Sweden_Spruce<- function(height_m,diameter_cm,crownBaseHeight_m=NULL,over_bark=TRUE)
{

  if(over_bark==TRUE){
    if(diameter_cm>=5)
    {
      if(!is.null(crownBaseHeight_m))
      {
        volume = 0.1102*diameter_cm^2 + 0.01648*diameter_cm^2*height_m + 0.005901*diameter_cm^2*crownBaseHeight_m + 0.01929*diameter_cm*height_m^2 - 0.05565*height_m^2
      } else
      {
        volume = 0.1202*diameter_cm^2 + 0.01504*diameter_cm^2*height_m + 0.02341*diameter_cm*height_m^2 - 0.06590*height_m^2
      }

    } else {
      volume = 0.22 + 0.0849*diameter_cm^2 + 0.0311*diameter_cm^2*height_m
    }
  } else {
    if(diameter_cm>=5)
    {
      if(!is.null(crownBaseHeight_m))
      {
        volume = 0.1057*diameter_cm^2 + 0.01658*diameter_cm^2*height_m + 0.006267*diameter_cm^2*crownBaseHeight_m+ 0.01782*diameter_cm*height_m^2 - 0.04681*height_m^2
      } else {
        volume = 0.1153*diameter_cm^2 + 0.01522*diameter_cm^2*height_m + 0.02170*diameter_cm*height_m^2 - 0.05501*height_m^2
      }
    } else {
      volume  = 0.15 + 0.0832*diameter_cm^2 + 0.0312*diameter_cm^2*height_m
    }
  }

  return(volume)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_form_factor_northern_Sweden_Spruce<- function(height_m,diameter_cm,crownBaseHeight_m=NULL,over_bark=TRUE)
{

  if(diameter_cm>=5){
    if(over_bark==TRUE){
      if(!is.null(crownBaseHeight_m))
      {
        form = 0.1102*diameter_cm^2 + 0.01648*diameter_cm^2*height_m + 0.005901*diameter_cm^2*crownBaseHeight_m + 0.01929*diameter_cm*height_m^2 - 0.05565*height_m^2
      } else
      {
        form = 0.1202*diameter_cm^2 + 0.01504*diameter_cm^2*height_m + 0.02341*diameter_cm*height_m^2 - 0.06590*height_m^2
      }

    } else {
      if(!is.null(crownBaseHeight_m))
      {
        K = ((height_m-crownBaseHeight_m)/height_m)*100
        form = 290.93 + 1346.06*(1/height_m) + 226.83*(height_m/diameter_cm) - 595.98*(height_m/diameter_cm^2)-0.7980*K
      } else
      {
        form = 193.84 + 1467.46*(1/height_m)+ 276.26*(height_m/diameter_cm)-700.45*(height_m/diameter_cm^2)
      }
    }
  } else {
    stop("diameter_cm must be >= 5 cm")
  }


  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_volume_northern_Sweden_Birch<- function(height_m,diameter_cm,double_bark_mm=NULL,crownBaseHeight_m=NULL,over_bark=TRUE)
{

  if(over_bark==TRUE){
    if(diameter_cm>=5)
    {
      if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm))
      {
        volume = 0.04192*diameter_cm^2 + 0.02927*diameter_cm^2*height_m + 0.003263*diameter_cm^2*crownBaseHeight_m + 0.003719*diameter_cm*height_m^2 - 0.001692*diameter_cm*height_m*double_bark_mm
      } else
      {
        volume = 0.03715*diameter_cm^2 + 0.02892*diameter_cm^2*height_m + 0.004983*diameter_cm*height_m^2
      }

    } else {
      volume = 0.10 + 0.0613*diameter_cm^2 + 0.0315*diameter_cm^2*height_m
    }
  } else {
    if(diameter_cm>=5)
    {
      if(!is.null(crownBaseHeight_m))
      {
        volume = 0.03328*diameter_cm^2 + 0.02876*diameter_cm^2*height_m + 0.002991*diameter_cm^2*crownBaseHeight_m + 0.003695*diameter_cm*height_m^2
      } else {
        volume = 0.02703*diameter_cm^2 + 0.03023*diameter_cm^2*height_m + 0.004346*diameter_cm*height_m^2
      }
    } else {
      volume  = 0.07 + 0.0472*diameter_cm^2 + 0.0344*diameter_cm^2*height_m
    }
  }

  return(volume)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_form_factor_northern_Sweden_Birch<- function(height_m,diameter_cm,double_bark_mm=NULL,crownBaseHeight_m=NULL,over_bark=TRUE)
{

  if(diameter_cm>=5){
    if(over_bark==TRUE){
      if(!is.null(crownBaseHeight_m) & !is.null(double_bark_mm))
      {
        K = ((height_m-crownBaseHeight_m)/height_m)*100
        B = ((double_bark_mm/10)/diameter_cm)*100
        form = 414.20 + 533.74*(1/height_m) + 47.35*(height_m/diameter_cm) - 2.154*B - 0.4154*K
      } else
      {
        form = 368.17 + 473*(1/height_m) + 63.44*(height_m/diameter_cm)
      }

    } else {
      if(!is.null(crownBaseHeight_m))
      {
        K = ((height_m-crownBaseHeight_m)/height_m)*100
        form = 404.30 + 423.71*(1/height_m) + 47.05*(height_m/diameter_cm)-0.3808*K
      } else
      {
        form = 384.88 + 344.14*(1/height_m) + 55.34*(height_m/diameter_cm)
      }
    }
  } else {
    stop("diameter_cm must be >= 5 cm")
  }


  return(form)
}
