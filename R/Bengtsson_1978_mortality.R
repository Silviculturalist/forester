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
#'  \href{https://lagen.nu/sou/1978:7/sid287.png}{Available Online (Swedish)}
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
    MAI,
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

  val = (a+b1*QMD+b2*(QMD^2)+b3*age+b4*(age^2)+b5*MAI)*reduction

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
#' @source Bengtsson, G. 1981-02-26. Stencil. Beräkning av den naturliga avgången ur virkesförrådet i Hugin-systemet. 8 sid. Umeå: Sveriges lantbruksuniversitet, inst. f. skogstaxering.
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
Elfving_HUGIN_annual_BA_mortality_percent <- function(
  northern_Sweden=TRUE,
  species='Pinus sylvestris',
  DAge=20){

  DAge = ifelse(DAge<100,
                ((DAge/10) + 1),
                min(((DAge-100)/20*2 + 12),17))

  if(northern_Sweden){
    if(species=='Pinus sylvestris'){
      val=0.14
    }
    if(species=='Picea abies'){
      val=-0.000236 + 0.0250275*DAge
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

  val = val/20

  if(val<0|val>1){
    warning('Mortality proportion out of bounds, clamped to extremes [0,1]')
    val = ifelse(val>1,1,0)
  }

  return(
    val
  )

}



#' Bengtsson Mortality as implemented in HUGIN 1980-06-27
#'
#' @source Bengtsson G. Handwritten PM to Anders Lundström 1980-06-27: Functions
#' for calculating natural mortality in established forests in the first version
#' of the HUGIN-system. 5 pages.
#'
#' @source Bengtsson, G. 1981-02-26. Stencil. Beräkning av den naturliga
#' avgången ur virkesförrådet i Hugin-systemet. 8 sid. Umeå: Sveriges
#' lantbruksuniversitet, inst. f. skogstaxering.
#'
#' @details Some coefficients preliminary: handwritten note may contain
#' alternatives. Functions for northern Sweden (region 1-3) based only on
#' material from Västernorrland and Jämtland county.
#'
#' @param species One of 'Pinus sylvestris','Picea abies','Betula pendula','Betula pubescens', or other deciduous tree.
#' @param BA Basal area m^2/ha (from 0 cm diameter at breast height).
#' @param stems Stems per hectare (>=5 cm diameter at breast height).
#' @param region numeric 1:5. Regional division from Swedish NFI 1973-77. Roughly 1-3 > 60N > 4-5
#' @param age age at breast height.
#' @param increment Number of years to increment. Default is 1.
#'
#' @return A list of lists. Mortality from crowding and other reasons. In
#' respective lists:
#' - Mortality in percent of BA at start of period: BA_percent
#' - Mortality in m^2/ha: BA_mortality
#' - QMD (cm) of losses: QMD_mortality.
#' - Number of stems lost based on QMD of losses and total BA: stems_mortality.
#' @export

Bengtsson_1981_mortality_BA = function(species,BA,stems,region,age,increment=1){
  if(species=='Pinus sylvestris'){
    if(region<4){
      stems=ifelse(stems>2700,2700,stems)
      crowding=
        0.3143E-01+
        -0.6877E-02*BA+
        +0.2056E-03*BA^2+
        +0.2684E-04*stems+
        -0.5092E-08*stems^2
      other=0.35
    } else {
      stems=ifelse(stems>4000,4000,stems)
      crowding=
        -0.6766E-01+
        -0.1283E-02*BA+
        +0.7748E-04*BA^2+
        +0.1441E-03*stems+
        -0.1839E-07*stems^2
      other=0.38

    }
  } else if(species=='Picea abies'){
    if(region<4){
      crowding=
        -0.2748E-02+
        +0.4493E-03*BA+
        +0.2515E-04*BA^2

      other=
        -0.3150E-03+
        +0.3337E-01*ifelse(((age%/%10)+1)<17,(age%/%10)+1,17)
    } else {
      stems=ifelse(stems>2800,2800,stems)
      crowding=
        +0.1235E-01+
        -0.2749E-02*BA+
        +0.8214E-04*BA^2+
        +0.2457E-04*stems+
        -0.4498E-08*stems^2
      other=0.36
    }

  } else if(region<4){
    if (species%in%c('Betula pubescens','Betula pendula')){
      crowding =
        -0.2513E-01+
        +0.5489E-02*BA
      other=0.78
    } else if(forester::tree_type(species)=="Deciduous"){
      crowding =
        -0.7277E-02+
        -0.2456E-02*BA+
        +0.1923E-03*BA^2
      other=0.50

    } else {
      stop("Unknown species or region.")
    }
  } else {
    if (forester::tree_type(species)=="Deciduous"){
      crowding = 0.04
      other=0.46
    } else {
      stop("Unknown species or region.")
    }

  }

  crowding = ifelse(crowding<0,0,crowding)
  other = ifelse(other<0,0,other)

  QMD = sqrt(BA/((pi/40E3)*stems))
  crowdingpercent = (crowding*increment)/100
  otherpercent = (other*increment)/100
  crowdingtotal = crowdingpercent*BA
  othertotal = otherpercent*BA
  QMDcrowding = 0.9*QMD
  QMDother = 1*QMD
  crowdingstems = crowdingtotal/((pi/4)*(QMDcrowding/100)^2)
  otherstems = crowdingstems/((pi/4)*(QMDother/100)^2)


  return(list(
    'Crowding'=
      list(
        'BA_percent'=crowdingpercent*100,
        'BA_mortality'=crowdingtotal,
        'QMD_mortality'=QMDcrowding,
        'stems_mortality'=crowdingstems),
    'Other'=
      list(
        'BA_percent'=otherpercent*100,
        'BA_mortality'=othertotal,
        'QMD_mortality'=QMDother,
        'stems_mortality'=otherstems
      )
  )
  )
}


