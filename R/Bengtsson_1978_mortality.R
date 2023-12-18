#' Percentage of live standing volume lost to mortality per annum
#'
#' @description Natural mortality. Functions based on National Forest Inventory
#'  surveys of dead trees during 1973-1975. Dead or wind-felled trees above 5 cm
#'  diameter at breast height. Avoided harsh storm damages during 1969. Largest
#'  insecurity pertains to which season the substrate in question died.
#'
#'  A 'roof' was set at a maximum of 0.9% per annum and never let the loss be less than 0.1% per annum.
#'
#'  The mortality level during the observed period was judged to have been
#'  higher than normal so at application the mortality given by the functions
#'  was reduced with 40 % in northern Sweden and 25 % in southern Sweden.
#'
#'  \url[Available Online (Swedish)](https://lagen.nu/sou/1978:7/sid287.png)
#'
#' @param QMD Diameter of mean basal area stem. Possibly arithmetic mean diameter.
#' @param age stand age.
#' @param MAI Mean Annual Increment max. e.g. [forester::Hagglund_1981_si_to_bonitet()],[forester::Jonson_1914_to_MAImax()]
#' @param northern_Sweden Northern Sweden? North of c. 60 deg. lat?
#' @param species One of 'Pinus sylvestris','Picea abies' or 'Betula pendula'.
#' @param reduce Default=TRUE. Reduce due to rather high mortality during years of data collection? See details.
#' @param floor Minimum annual mortality, in percent of live standing volume.
#' @param cieling Maximum annual mortality, in percent of live standing volume.
#'
#' @return Annual mortality in percent of live standing volume.
#' @export

Bengtsson_1978_annual_volume_mortality_percent <- function(
    QMD, #mean dbh
    age,
    MAIMax,
    northern_Sweden=TRUE,
    species='Pinus sylvestris',
    reduce=TRUE,
    floor=0.1,
    cieling=0.9){

  if(QMD<7){
    QMD = 7
  }
  if(QMD>30){
    QMD=30
  }
  if(age<30){
    age=30
  }
  if(age>120){
    age=120
  }

  if(northern_Sweden){

    if(species=='Pinus sylvestris'){
      a = 0.437
      b1=-0.0847
      b2= 0.00214
      b3= 0.0104
      b4=-0.0000317
      b5= 0
    }
    if(species=='Picea abies'){
      a = 0.390
      b1=-0.0610
      b2= 0.00160
      b3= 0.0050
      b4= 0
      b5= 0
    }
    if(forester::tree_type(species)=='Deciduous'){
      a = 1.005
      b1=-0.0843
      b2= 0.00176
      b3=-0.000174
      b4= 0.0000591
      b5= 0
    }

  } else {
    if(species=='Pinus sylvestris'){
      a = 0.760
      b1=-0.0430
      b2= 0
      b3= 0.0047
      b4= 0
      b5= 0.048
    }
    if(species=='Picea abies'){
      a = 0.330 #0.66 in Elfving report p.64
      b1=-0.0130
      b2= 0
      b3= 0.0043
      b4= 0
      b5= 0
    }
    if(forester::tree_type(species)=='Deciduous'){
      a = 0.460
      b1=-0.0200
      b2= 0
      b3= 0.0046
      b4= 0
      b5= 0
    }

  }

  reduction=1

  if(reduce){
    reduction = ifelse(northern_Sweden,0.6,0.75) #-40% northern Sweden, -25% s. Sweden.
  }

  val = (a+b1*QMD+b2*(QMD^2)+b3*age+b4*(age^2)+b5*MAIMax)*reduction

  if(val<floor){
    val=floor
  }
  if(val>cieling){
    val=cieling
  }

  return(val)
}


#' Annual Basal Area Mortality
#'
#' @source Elfving, Björn. 2014-03-17. PM. Modellering av naturlig avgång i Heureka. (Swedish). URL:\url{https://www.heurekaslu.se/w/images/f/f4/HeurekaMortality-PM140317.pdf}
#'
#' @description Annual Mortality in terms of percent of the basal area at start of period.
#'
#' @details N.B. At application it at first glance looks as if from ProdMod2 the
#' mean diameter of the outgoing trees should be set to the QMD of the stand at
#' the start of the period.
#'
#'
#' @param northern_Sweden Logical. Site located > 60 deg. N?
#' @param species One of 'Pinus sylvestris','Picea abies','Betula pendula','Betula pubescens', or other deciduous tree species.
#' @param DAge Basal Area Weighted Mean Age, not including tree standards.
#'
#' @return Annual Mortality in Percent of Basal Area.
#' @export
Bengtson_HUGIN_annual_BA_mortality_percent <- function(
  northern_Sweden=TRUE,
  species='Pinus sylvestris',
  DAge=20){

  if(northern_Sweden){
    if(species=='Pinus sylvestris'){
      val=0.14
    }
    if(species=='Picea abies'){
      val=0.025*((DAge/10) + 1)
    }
    if(species%in%c('Betula pendula','Betula pubescens')){
      val=0.78
    }
    if(forester::tree_type(species)=='Deciduous'){
      val=0.35
    }
  }

  if(!northern_Sweden){
    if(species=='Pinus sylvestris'){
      val=0.38
    }
    if(species=='Picea abies'){
      val=0.36
    }
    if(species%in%c('Betula pendula','Betula pubescens')){
      val=0.46
    }
    if(forester::tree_type(species)=='Deciduous'){
      val=0.46
    }
  }

  return(
    val
  )

}
