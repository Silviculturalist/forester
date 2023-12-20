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
#' @param DBH Diameter at breast height, over or respectively under bark.
#' @param height Tree height, meters.
#' @param crownBaseheight Height until crown base.
#' @param over_bark Boolean. If input diameter is over/under bark, and if returned value should be over or under bark.
#' @return Form factor (unspecified.) or Volume, dm^3.
#'
#'
#' @name NaslundKubering
#' @export

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_southern_Sweden_Pine <- function(height,DBH, double_bark_mm, crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      volume = 0.1193*DBH^2 + 0.02574*height*DBH^2 + 0.007262*crownBaseheight*DBH^2 + 0.004054*DBH*height^2 - 0.003112*DBH*height*double_bark_mm
    } else {
      volume = 0.1072*DBH^2 + 0.02427*height*DBH^2 + 0.007315*DBH*height^2
    }
  } else {
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      volume = 0.07141*DBH^2 + 0.02580*height*DBH^2 + 0.009430*crownBaseheight*DBH^2 + 0.003511*DBH*height^2 + 0.001052*DBH*height*double_bark_mm
    } else {
      volume = 0.06271*DBH^2 + 0.03208*height*DBH^2 + 0.005725*DBH*height^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_southern_Sweden_Pine <- function(height,DBH,double_bark_mm,crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      K = ((height-crownBaseheight)/height)*100
      B = ((double_bark_mm/10)/DBH)*100
      form = 420.16 + 1519.24*(1/height) + 51.62*(height/DBH) - 3.962*B - 0.9246*K

    } else {
      form = 308.97 + 1365.38*(1/height)+ 93.14*(height/DBH)
    }

  } else {
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      K = ((height-crownBaseheight)/height)*100
      B = ((double_bark_mm/10)/DBH)*100
      form = 448.53 + 909.21*(1/height)+ 44.71*(height/DBH) + 1.339*B - 1.201*K

    } else {
      form = 408.49 + 798.46*(1/height) + 72.89*(height/DBH)

    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_southern_Sweden_Spruce <- function(height,DBH, crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(crownBaseheight)){
      volume = 0.1059*DBH^2 + 0.01968*height*DBH^2 + 0.006168*crownBaseheight*DBH^2 + 0.01468*DBH*height^2 - 0.04585*height^2
    } else {
      volume = 0.1104*DBH^2 + 0.01928*height*DBH^2 + 0.01815*DBH*height^2 - 0.04936*height^2
    }
  } else {
    if(!is.null(crownBaseheight)){
      volume = 0.1039*DBH^2 + 0.01959*height*DBH^2 + 0.005942*crownBaseheight*DBH^2 + 0.01417*DBH*height^2 - 0.04332*height^2
    } else {
      volume = 0.1076*DBH^2 + 0.01929*height*DBH^2 + 0.01723*DBH*height^2 - 0.04615*height^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_southern_Sweden_Spruce <- function(height,DBH,crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseheight)){
      K = ((height-crownBaseheight)/height)*100
      form = 329.09 + 1348.92*(1/height) + 186.94*(height/DBH) - 583.74*height/DBH^2 - 0.7854*K

    } else {
      form = 245.09 + 1405.66*(1/height)+ 231.11*(height/DBH)-628.48*height/DBH^2
    }

  } else {
    if(!is.null(crownBaseheight)){
      K = ((height-crownBaseheight)/height)*100
      form = 325.04 + 1322.62*(1/height)+ 180.36*(height/DBH) - 551.55*height/DBH^2 - 0.7566*K

    } else {
      form = 245.57 + 1369.89*(1/height) + 219.34*(height/DBH) - 587.65*height/DBH^2

    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_southern_Sweden_Birch <- function(height,DBH, double_bark_mm=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      volume = 0.09595*DBH^2 + 0.02375*height*DBH^2 + 0.01221*DBH*height^2 - 0.03636*height^2 - 0.004605*DBH*height*double_bark_mm
    } else {
      volume = 0.1432*DBH^2 + 0.008561*height*DBH^2 + 0.02180*DBH*height^2 - 0.06630*height^2
    }
  } else {
    if(!is.null(double_bark_mm)){
      volume = 0.08953*DBH^2 + 0.02101*height*DBH^2 + 0.01171*DBH*height^2 - 0.03189*height^2 - 0.0007244*DBH*height*double_bark_mm
    } else {
      volume = 0.09944*DBH^2 + 0.01862*height*DBH^2 + 0.01278*DBH*height^2 - 0.03544*height^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_southern_Sweden_Birch <- function(height,DBH,double_bark_mm=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/DBH)*100
      form = 302.45 + 1221.63*(1/height) + 155.44*(height/DBH) - 462.95*height/DBH^2 - 5.864*B

    } else {
      form = 109.01 + 1823.03*(1/height)+ 277.56*(height/DBH)-844.17*height/DBH^2
    }

  } else {
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/DBH)*100
      form = 267.44 + 1139.98*(1/height)+ 149.04*(height/DBH) - 406.04*height/DBH^2 - 0.9224*B

    } else {
      form = 237.03 + 1266.05*(1/height) + 162.72*(height/DBH) - 451.26*height/DBH^2

    }
  }

  return(form)
}


#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_Sweden_Spruce <- function(height,DBH,double_bark_mm=NULL,crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')

  if(over_bark==TRUE){
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      volume = 0.1121*DBH^2 + 0.02919*height*DBH^2 + 0.006285*crownBaseheight*DBH^2 + 0.002460*DBH*height^2 - 0.003574*DBH*height*double_bark_mm
    } else {
      volume = 0.1028*DBH^2 + 0.02705*height*DBH^2 + 0.005215*DBH*height^2
    }
  } else {
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      volume = 0.06791*DBH^2 + 0.02989*height*DBH^2 + 0.007555*crownBaseheight*DBH^2 + 0.002063*DBH*height^2 + 0.0006544*DBH*height*double_bark_mm
    } else {
      volume = 0.06045*DBH^2 + 0.03413*height*DBH^2 + 0.004163*DBH*height^2
    }
  }

  return(volume)

}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_Sweden_Pine <- function(height,DBH,double_bark_mm=NULL,crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      K = ((height-crownBaseheight)/height)*100
      B = ((double_bark_mm/10)/DBH)*100
      form = 451.70 + 1426.70*(1/height) + 31.33*(height/DBH) - 4.551*B - 0.8002*K

    } else {
      form = 344.40 + 1308.56*(1/height)+ 66.40*(height/DBH)
    }

  } else {
    if(!is.null(crownBaseheight) & !is.null(double_bark_mm)){
      K = ((height-crownBaseheight)/height)*100
      B = ((double_bark_mm/10)/DBH)*100
      form = 476.82 + 864.68*(1/height)+ 26.26*(height/DBH) + 0.8332*B - 0.9619*K

    } else {
      form = 434.54 + 769.61*(1/height) + 53.01*(height/DBH)

    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_Sweden_Spruce <- function(height,DBH,crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseheight)){
      K = ((height-crownBaseheight)/height)*100
      form = 309.76 + 1385.38*(1/height)+208.29*(height/DBH) - 640.41*(height/DBH^2)-0.7477*K

    } else {
      form = 222.30 + 1463.59*(1/height) + 257.46*(height/DBH) - 715.27*(height/DBH^2)
    }

  } else {
    if(!is.null(crownBaseheight)){
      K = ((height-crownBaseheight)/height)*100
      form = 306.60 + 1363.31*(1/height) + 199.71*(height/DBH) - 591.81*(height/DBH^2) - 0.7403*K

    } else {

      form = 221.51 + 1431.21*(1/height)+ 244.14*height/diameter - 652.09*height/diameter^2
    }
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_Sweden_Spruce <- function(height,DBH,crownBaseheight=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(crownBaseheight)){
      volume = 0.1088*DBH^2 + 0.01846*height*DBH^2 + 0.005873*crownBaseheight*DBH^2 + 0.01636*DBH*height^2 - 0.05030*height^2

    } else {
      volume = 0.1150*DBH^2 + 0.01746*height*DBH^2 + 0.02022*DBH*height^2 - 0.05618*height^2
    }

  } else {
    if(!is.null(crownBaseheight)){
      volume = 0.1071*DBH^2 + 0.01827*height*DBH^2 + 0.005814*crownBaseheight*DBH^2 + 0.01569*DBH*height^2 - 0.04648*height^2
    } else {
      volume = 0.1124*DBH^2 + 0.01740*height*DBH^2 + 0.01917*DBH*height^2 - 0.05122*height^2
    }
  }
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_form_factor_Sweden_Birch <- function(height,DBH,double_bark_mm=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/DBH)*100
      form = 323.27 + 1110.48*(1/height) + 135.64*height/DBH - 411.05*height/DBH^2 - 5.533*B
    } else {
      form = 170.33 + 1661.17*(1/height) + 223.71*height/DBH - 713.75*height/DBH^2
    }

  } else {
    if(!is.null(double_bark_mm)){
      B = ((double_bark_mm/10)/DBH)*100
      form = 309.17 + 922.45*(1/height) + 119.90*height/DBH - 307.69*height/DBH^2 - 1.168*B

    } else {
      form = 277.52 + 1064.41*(1/height) + 132.93*height/DBH - 356.08*height/DBH^2
    }
  }
  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1947_volume_Sweden_Birch <- function(height,DBH,double_bark_mm=NULL, over_bark=TRUE){
  if(DBH<5) stop('Diameter must be larger than 5 cm.')
  if(over_bark==TRUE){
    if(!is.null(double_bark_mm)){
      volume = 0.08722*DBH^2 + 0.02539*height*DBH^2 + 0.01065*DBH*height^2 - 0.03228*height^2 - 0.004346*DBH*height*double_bark_mm
    } else {
      volume = 0.1305*DBH^2 + 0.01338*height*DBH^2 + 0.01757*DBH*height^2 - 0.05606*height^2
    }

  } else {
    if(!is.null(double_bark_mm)){
      volume = 0.07245*DBH^2 + 0.02428*height*DBH^2 + 0.009417*DBH*height^2 - 0.02417*height^2 - 0.0009177*DBH*height*double_bark_mm
    } else {
      volume = 0.08360*DBH^2 + 0.02180*height*DBH^2 + 0.01044*DBH*height^2 - 0.02797*height^2

    }
  }
  return(volume)
}


#' @rdname NaslundKubering
#' @export
Naslund_1940_volume_northern_Sweden_Pine<- function(height,DBH,double_bark_mm=NULL,crownBaseheight=NULL,over_bark=TRUE)
{

  if(over_bark==TRUE){
    if(DBH>=5)
    {
      if(!is.null(crownBaseheight) & !is.null(double_bark_mm))
      {
        volume = 0.1018*DBH^2 + 0.03112*DBH^2*height + 0.007312*DBH^2*crownBaseheight-0.002906*DBH*height*double_bark_mm
      } else
      {
        volume = 0.09314*DBH^2 + 0.03069*DBH^2*height + 0.002818*DBH*height^2
      }

    } else {
      volume = 0.22 + 0.0504*DBH^2*height
    }
  } else {
    if(DBH>=5)
    {
      if(!is.null(crownBaseheight) & !is.null(double_bark_mm))
      {
        volume = 0.06059*DBH^2 + 0.03153*DBH^2*height + 0.007919*DBH^2*crownBaseheight+0.001773*DBH*height*double_bark_mm
      } else {
        volume = 0.05491*DBH^2 + 0.03641*DBH^2*height + 0.002699*DBH*height^2
      }
    } else {
      volume  = 0.15 + 0.0488*DBH^2*height
    }
  }

  return(volume)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_form_factor_northern_Sweden_Pine<- function(height,DBH,double_bark_mm=NULL,crownBaseheight=NULL,over_bark=TRUE)
{

  if(DBH>=5){
    if(over_bark==TRUE){
      if(!is.null(crownBaseheight) & !is.null(double_bark_mm))
      {
        K = ((height-crownBaseheight)/height)*100
        B = ((double_bark_mm/10)/DBH)*100
        form = 489.35+1296.11*(1/height) - 3700*B - 0.9310*K
      } else
      {
        form = 390.81+1185.86*(1/height) + 35.88*(height/DBH)
      }
    } else {
      if(!is.null(crownBaseheight) & !is.null(double_bark_mm))
      {
        K = ((height-crownBaseheight)/height)*100
        B = ((double_bark_mm/10)/DBH)*100
        form = 502.22 + 771.5*(1/height) + 2.257*B - 1.008*K
      } else
      {
        form = 463.55 + 699.14*(1/height) + 34.36*(height/DBH)
      }
    }
  } else {
    stop("DBH must be >=5 cm.")
  }

  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_volume_northern_Sweden_Spruce<- function(height,DBH,crownBaseheight=NULL,over_bark=TRUE)
{

  if(over_bark==TRUE){
    if(DBH>=5)
    {
      if(!is.null(crownBaseheight))
      {
        volume = 0.1102*DBH^2 + 0.01648*DBH^2*height + 0.005901*DBH^2*crownBaseheight + 0.01929*DBH*height^2 - 0.05565*height^2
      } else
      {
        volume = 0.1202*DBH^2 + 0.01504*DBH^2*height + 0.02341*DBH*height^2 - 0.06590*height^2
      }

    } else {
      volume = 0.22 + 0.0849*DBH^2 + 0.0311*DBH^2*height
    }
  } else {
    if(DBH>=5)
    {
      if(!is.null(crownBaseheight))
      {
        volume = 0.1057*DBH^2 + 0.01658*DBH^2*height + 0.006267*DBH^2*crownBaseheight+ 0.01782*DBH*height^2 - 0.04681*height^2
      } else {
        volume = 0.1153*DBH^2 + 0.01522*DBH^2*height + 0.02170*DBH*height^2 - 0.05501*height^2
      }
    } else {
      volume  = 0.15 + 0.0832*DBH^2 + 0.0312*DBH^2*height
    }
  }

  return(volume)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_form_factor_northern_Sweden_Spruce<- function(height,DBH,crownBaseheight=NULL,over_bark=TRUE)
{

  if(DBH>=5){
    if(over_bark==TRUE){
      if(!is.null(crownBaseheight))
      {
        form = 0.1102*DBH^2 + 0.01648*DBH^2*height + 0.005901*DBH^2*crownBaseheight + 0.01929*DBH*height^2 - 0.05565*height^2
      } else
      {
        form = 0.1202*DBH^2 + 0.01504*DBH^2*height + 0.02341*DBH*height^2 - 0.06590*height^2
      }

    } else {
      if(!is.null(crownBaseheight))
      {
        K = ((height-crownBaseheight)/height)*100
        form = 290.93 + 1346.06*(1/height) + 226.83*(height/DBH) - 595.98*(height/DBH^2)-0.7980*K
      } else
      {
        form = 193.84 + 1467.46*(1/height)+ 276.26*(height/DBH)-700.45*(height/DBH^2)
      }
    }
  } else {
    stop("DBH must be >= 5 cm")
  }


  return(form)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_volume_northern_Sweden_Birch<- function(height,DBH,double_bark_mm=NULL,crownBaseheight=NULL,over_bark=TRUE)
{

  if(over_bark==TRUE){
    if(DBH>=5)
    {
      if(!is.null(crownBaseheight) & !is.null(double_bark_mm))
      {
        volume = 0.04192*DBH^2 + 0.02927*DBH^2*height + 0.003263*DBH^2*crownBaseheight + 0.003719*DBH*height^2 - 0.001692*DBH*height*double_bark_mm
      } else
      {
        volume = 0.03715*DBH^2 + 0.02892*DBH^2*height + 0.004983*DBH*height^2
      }

    } else {
      volume = 0.10 + 0.0613*DBH^2 + 0.0315*DBH^2*height
    }
  } else {
    if(DBH>=5)
    {
      if(!is.null(crownBaseheight))
      {
        volume = 0.03328*DBH^2 + 0.02876*DBH^2*height + 0.002991*DBH^2*crownBaseheight + 0.003695*DBH*height^2
      } else {
        volume = 0.02703*DBH^2 + 0.03023*DBH^2*height + 0.004346*DBH*height^2
      }
    } else {
      volume  = 0.07 + 0.0472*DBH^2 + 0.0344*DBH^2*height
    }
  }

  return(volume)
}

#' @rdname NaslundKubering
#' @export
Naslund_1940_form_factor_northern_Sweden_Birch<- function(height,DBH,double_bark_mm=NULL,crownBaseheight=NULL,over_bark=TRUE)
{

  if(DBH>=5){
    if(over_bark==TRUE){
      if(!is.null(crownBaseheight) & !is.null(double_bark_mm))
      {
        K = ((height-crownBaseheight)/height)*100
        B = ((double_bark_mm/10)/DBH)*100
        form = 414.20 + 533.74*(1/height) + 47.35*(height/DBH) - 2.154*B - 0.4154*K
      } else
      {
        form = 368.17 + 473*(1/height) + 63.44*(height/DBH)
      }

    } else {
      if(!is.null(crownBaseheight))
      {
        K = ((height-crownBaseheight)/height)*100
        form = 404.30 + 423.71*(1/height) + 47.05*(height/DBH)-0.3808*K
      } else
      {
        form = 384.88 + 344.14*(1/height) + 55.34*(height/DBH)
      }
    }
  } else {
    stop("DBH must be >= 5 cm")
  }


  return(form)
}
