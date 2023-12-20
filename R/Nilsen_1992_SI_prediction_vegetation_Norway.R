#' Site index estimation from vegetation type and site properties in Norway.
#'
#' @source Nilsen, P. & Larsson, J. 1992. Bonitering av skog ved hjelp av
#' vegetasjonstype og egenskaper ved voksestedet. (Site index estimation from
#' vegetation type and site properties). Rapp. Skogforsk 22/92:1-43.
#' Available online (15/03/2022):\url{https://nibio.brage.unit.no/nibio-xmlui/handle/11250/2677459?show=full&locale-attribute=en}
#'
#' @param species One of "Picea abies" (default), or "Pinus sylvestris"
#' @param vegetation Code from [forester::Nilsen_1992_vegetation_types()]
#' @param soil_depth Integer. 1: less than 10 cm, 2: 10-30cm; 3: 30-100cm, 4: 100cm+
#' @param hillside TRUE or FALSE (default).
#' @param latitude Decimal latitude (only used for Norway Spruce).
#' @param altitude Meters above sea level.
#'
#' @details
#' \itemize{
#'    \item Spruce: R^2 = 0.62 (sd 16\%)
#'    \item Pine: R^2 = 0.56 (sd 21\%)
#'    }
#' Largest deviation on high site indices for spruce (3 m.)
#'
#' @return Estimated Site Index H40 [forester::Tveite_1976_height_trajectory_Norway_Pine()][forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]
#' @export
Nilsen_1992_SI_prediction_vegetation_Norway <- function(
  species="Picea abies",
  vegetation=1,
  soil_depth=3,
  hillside=FALSE,
  latitude,
  altitude
){
  Lichen_Pine_veg <- ifelse(vegetation==1,1,0)
  Bilberry_Pine_veg <- ifelse(vegetation==2,1,0)
  heath_vac_uliginosum <- ifelse(vegetation==3,1,0)
  spruce_bilberry_veg <- ifelse(vegetation==4,1,0)
  spruce_dryopteris <- ifelse(vegetation==5,1,0)
  spruce_low_herbs <- ifelse(vegetation==6,1,0)
  spruce_aconitum <- ifelse(vegetation==7,1,0)

  soil_depth_greater_30 <- ifelse(soil_depth==3|soil_depth==4,1,0)


  if(species=="Picea abies")
    return(exp(5.591 + -0.000000870*altitude^2 + -0.0464*latitude + -0.354*Bilberry_Pine_veg + -0.225*spruce_bilberry_veg + -0.050*spruce_dryopteris + 0.065*spruce_aconitum + 0.196*soil_depth_greater_30 + 0.055*hillside))

  if(species=="Pinus sylvestris")
    return(exp(2.158 + -0.000000710*altitude^2 + 0.196*Bilberry_Pine_veg + 0.352*spruce_bilberry_veg + 0.354*spruce_dryopteris + 0.444*spruce_low_herbs + 0.000493*spruce_aconitum + 0.259*soil_depth_greater_30 + 0.089*hillside))

}

#' Vegetation types for predicting site index for Scots Pine and Norway Spruce
#' in Norway.
#'
#' @source Nilsen, P. & Larsson, J. 1992. Bonitering av skog ved hjelp av
#' vegetasjonstype og egenskaper ved voksestedet. (Site index estimation from
#' vegetation type and site properties). Rapp. Skogforsk 22/92:1-43.
#' Available online (15/03/2022):\url{https://nibio.brage.unit.no/nibio-xmlui/handle/11250/2677459?show=full&locale-attribute=en}
#'
#' @param description Print vegetation type description? TRUE or FALSE (default)
#' @param species Print important species? TRUE or FALSE (default).
#' @param code Integer 1 - 8. Default is 1:8
#'
#' @return A tibble with english and norwegian code explanations. If species is TRUE, also a list of the most important species in latin. If description is TRUE, also a translation of the vegetation community.
#' @export
#'
#' @details N.B. Function authors norwegian is shaky.
#' @author Carl Vigren, SLU.
#'
#' @seealso [forester::Nilsen_1992_SI_prediction_vegetation_Norway()]

Nilsen_1992_vegetation_types <- function(description=FALSE,species=FALSE,code=1:8){
  cat("Description of the most important vegetation types in Norway\nTranslated 15/03/2022. My norwegian not perfect.\n")
  dplyr::tibble(
    "Code"=seq(1,8,1),
    "English"=c("Lichen", "Berry-heather","Heather-Bog bilberry", "Bilberry","Small fern","Low herbs","Large fern","Tall herbs"),
    "Norwegian"=c("Lavskog","Bærlyngskog", "Røsslyng-Blokkebærskog", "Blåbærskog","Småbregneskog","Lågurtskog","Storbregneskog","Høgstaudeskog")
  ) %>% dplyr::filter(Code==code) %>% print()

   if(description==TRUE){
     if(!(code%in%seq(1,8,1))) stop("Requires argument 'code' to be 1-8.")
     if(code==1) cat("\nDescription:\n\nThis is a poor and open forest type, with Scots Pine as dominating tree species. The ground layer is dominated by Empetrum nigrum ssp. hermaphroditum, Calluna vulgaris & lichen, with sparse amounts of Cladonia spp. The type is found on sites with poor water access, either on very shallow soil, or coarser sediment. Mostly prevalent in more arid areas of the country.")
     if(code==2) cat("\nDescription:\n\nThis is also a relatively open forest type with a dominance of Scots Pine, but also with smaller amounts of Norway Spruce or Birch. The field layer is strongly dominated by heather species. One difference from A1 (ed. note; Code 1, Lichen, Lavskog) is that mosses often dominate over lichen in the ground layer. The forest type can be present as a pure Norway Spruce forest.")
     if(code==3) cat("\nDescription:\n\nThis is the typical Scots Pine forest in the Sønnafjells (eds. note; South of Dovrefjell and east of Langfjellene). It often has sparse, uneven stand of Scots Pine and birch, and a few crooked Norway Spruces, and is often prevalent on shallow soils. Largest differences from A2 (ed. note; Code 2, Berry-heather, Bærlyngskog) by that V. uligonosum as a rule almost always is present, and that a, not thick, often soft (gyngende?) raw-humus mattress.")
     if(code==4) cat("\nDescription:\n\nThis is the most common Norway Spruce forest type on Østlandet. As a rule it is a pure spruce stand, sometimes with Scots Pine and Birch. Species poor field layer, dominated by a few herbs, blueberry shrubs and mosses.")
     if(code==5) cat("\nDescription:\n\nMesic to moist forest type, typical of east or north slopes with some warmth. The type typically has a thick, well-grown stand, usually of Norway Spruce. Field layer with small ferns and lush moss carpet, but little heather. Patches of (Spaghnum) are characteristic.")
     if(code==6) cat("\nDescription:\n\nThis is a species rich forest spread out in warmer areas. The stand is dominated by spruce or birch, ocassionally Pine or aspen. Many smaller herbs in the field layer, but the moss carpet in the ground layer is often thin and patchy.")
     if(code==7) cat("\nDescription:\n\nThis site gives a well-grown stand of spruce with some elements of birch. The type is prevalent on fresh to moist, mostly nutrient rich soils. It is associated to flat clay areas with shallow ground water or to slopes with much offrun.")
     if(code==8) cat("\nDescription:\n\nThe tall herb forest is lush and species-rich with a stand of spruce or birch, seldom Pine. Herbs, grass and ferns dominate the ground layer, but the moss carpet is very sparse. Prevalent in slopes and drains with fresh run-off, most common in higher-lying areas.")
   }

  if(species==TRUE){
    if(!(code%in%seq(1,8,1))) stop("Requires argument 'code' to be 1-8.")
    if(code==1) cat("\n\nImportant species:\n\nVaccinium vitis-idaea\nCalluna vulgaris\nEmpetrum nigrum hermaphroditum\nArcostaphylos uva-ursi\nCarex pilulifera\nDicranum drummondii\nD.spurium\nD. polysetum\nPolytrichum piliferum\nPtilidium ciliare\nCladonia arbuscula\nC. uncialis\nCornicularia aculeata\nCetraria nivalis\nC. cucullata\nStereocaulon spp.\nCladonia spp.")
    if(code==2) cat("\n\nImportant species:\n\nEmpetrum hermaphroditum\nVaccinium myrtillus\nVaccinium vitis-idaea\nCalluna vulgaris\nMelampyrum pratense\nLinnea borealis\nDeschampsia flexuosa\nHylocomium splendens\nDicranum polysetum\nPleurozium screberii\nCladonia rangiferina\nC. arbuscula")
    if(code==3) cat("\n\nImportant species:\n\nEmpetrum hermaphroditum\nVaccinium myrtillus\nVaccinium uliginosum\nVaccinium vitis-idaea\nCalluna vulgaris\nMelampyrum pratense\nHylocomium splendens\nBarbiliphozia spp.\nSphagnum nemoreum\nCetraria islandica\nCladonia rangiferina")
    if(code==4) cat("\n\nImportant species:\n\nVaccinium myrtilus\nVaccinium vitis-idaea\nTrientalis europaea\nMaianthemum bifolium\nOrthilia secunda\nSolidago virgaurea\nLinnea borealis\nLuzula pilosa\nDeschampsia flexuosa\nLycopodium annotinum\nDicranum majus\nPlagiothecium denticulatum\nSphagnum girgensohnii")
    if(code==5) cat("\n\nImportant species:\n\nAnemone nemorosa\nOxalis acetosella\nAgrostis tenuis\nCalamagrostis purpurea\nGymnocarpium dryopteris\nDryopteris expansa\nThelypteris phegopteris\nSphagnum girgensohnii")
    if(code==6) cat("\n\nImportant species:\n\nOxalis acetosella\nPolygonum viviparum\nGeranium sylvaticum\nFragaria vesca\nHepatica nobilis\nHieracium sylvatica-gr.\nPyrola rotundifolia\nMelampyrum sylvaticum\nRubus saxatilis\nViola riviniana\nVeronica officinalis\nLathyrus montanus\nCarex digitata\nMelica nutans\nPoa nemoralis\nDryopteris filix-mas\nPlagiomnium affine\nRhytidiadelphus triquetrus")
    if(code==7) cat("\n\nImportant species:\n\nRubus idaeus\nCircerbita alpina\nMyosotis decumbens\nStellaria nemorum\nCrepis paludosa\nParis quadrifolia\nRumex acetosa\nCalamagrostis purpurea\nDeschampsia caespitosa\nMilium effusum\nAthyrium filix-femina\nAthyrium distentifolium\nRhodobryum roseum\nCirriphyllum piliferum")
    if(code==8) cat("\n\nImportant species:\n\nSalix nigricans\nS. phyllicifolia\nAconitum septentrionale\nCicerbita alpina\nTrollius europaeus\nRanunculus platanifolius\nCirsium helonoides\nCrepis paludosa\nFilipendula ulmaria\nAlchemilla spp.\nGeranium sylvaticum\nGeum rivale\nHepatica nobilis\nFragaria vesca\nViola riviniana\nMelica nutans\nCalamagrostis purpurea\nMilium effusum\nDeschampsia caespitosa\nDryopteris filix-mas\nThelypteris phegopteris\nAthyrium filix-femina\nAthyrium distentifolium")
  }
}

