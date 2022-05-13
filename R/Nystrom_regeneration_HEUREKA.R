#Break out into subfunctions for different regeneration types and species for readability.


Nystrom_regeneration_HEUREKA <- function(
  plot_area,
  stems_per_species, #A named list.
  regeneration_method,
  regenerationSpecies=c("Pinus sylvestris","Picea abies","Pinus contorta","Deciduous"),
  SI100,
  meanHeightMainSpecies,
  meanHeightSecondarySpecies,
  soil_moisture,
  vegetation,
  maxNumberOfTypeTrees,
  predictionUnit,
  meanAgeTotal,
  regenerationType=c("Sown","Culture","Natural regeneration")

    #c("Synthetic","Mean heights","NFI-data")
){
  if(is.list(stems_per_species)){
    #Use coefficients of variation for height and Weibull parameters for the height distributions.


  }

  #Step 1 Total number of stems


  #sown regeneration of Pine
  step1CoeffPineSown <- list(5.48811, 0.0, 0.4274, 0.0086, 0.0, 0.1341, 0.0, 0.2865, 0.0, 0.1741, 0.0543, 0.1291, 1.063)

  #Regeneration of Pine in plantation
  step1CoeffPineplantation <- list(0.8869, -0.0252, 1.7140, 0.0070, 0.0, 0.1935, 0.2665, 0.1589, -0.0944, 0.1907, 0.0405, 0.1420, 1.103)

  #Regeneration of Spruce in plantation
  step1CoeffSpruceplantation <- list(3.9939, -0.0157, 0.9207, 0.0172, 0.0, 0.2477, 0.0, 0.1696, 0.0, 0.1944, 0.0445, 0.1455, 1.117)

  #Regeneration of Contorta in plantation
  step1CoeffContortaplantation <- list(4.5904, 0.0, 0.6144, 0.0, 0.0, 0.1687, 0.0, 0.0, -0.1350, 0.2444, 0.0205, 0.0920, 1.077)

  coefNamesStep1 <- c("Step1InterceptIndex",
                      "Step1QIndex",
                      "Step1InQIndex",
                      "Step1HIndex",
                      "Step1InHIndex",
                      "Step1QonHindIndex",
                      "Step1InSiIndex",
                      "Step1WetIndex",
                      "Step1DryIndex",
                      "Step1UdimByInQIndex",
                      "Step1SigmaSIndex",
                      "Step1SigmaPIndex",
                      "Step1BiasIndex")


  calculateTotalNumberOfStems <- function(
    meanHeightMainSpecies,
    SI,
    Deterministic,
    RegenerationType
  ){
    meanHeightMainSpecies_dm<- meanHeightMainSpecies*10

    if(RegenerationType=="Natural regeneration"){
      #natural regeneration total stems.
      step1CoeffNaturalRegeneration <- list(4.3328, -0.0076, 1.2245, 0.0, -0.2822, 0.0, 0.0, 0.1693, -0.1295, 0.1969, 0.0444, 0.1334, 1.093)
      names(step1CoeffNaturalRegeneration) <- coefNamesStep1

      temp <- step1CoeffNaturalRegeneration["Step1InQIndex"]+
              +step1CoeffNaturalRegeneration["Step1QIndex"]*q+
              +step1CoeffNaturalRegeneration["Step1InQIndex"]*lnQ+
              +step1CoeffNaturalRegeneration["Step1HIndex"]*(meanHeightMainSpecies_dm)+
              +step1CoeffNaturalRegeneration["Step1InHIndex"]*log(meanHeightMainSpecies_dm)+
              +step1CoeffNaturalRegeneration["Step1QonHindIndex"]*(q/hind)+
              +step1CoeffNaturalRegeneration["Step1InSiIndex"]*log(SI)+
              +step1CoeffNaturalRegeneration["Step1WetIndex"]*Wet+
              +step1CoeffNaturalRegeneration["Step1DryIndex"]*Dry+
              +step1CoeffNaturalRegeneration["Step1UdimByInQIndex"]*(udim*lnQ)

        ifelse(Deterministic==TRUE,
               return(exp(temp)*step1CoeffNaturalRegeneration["Step1BiasIndex"] - 1),
               return(exp(temp + runif(n=1)*sqrt(step1CoeffNaturalRegeneration["Step1SigmaPIndex"]))-1)
               )
    }

    calculateProportionOfConifer <- function(
      q,
      Qind,
      stemTotal,
      SI,
      Wet,
      Dry,
      Rich,
      Poor
    ){
      #natural regeneration
      step2CoeffNaturalRegeneration <- list(-3.5301, 0.0, 2.3451, -0.00011, -1.5062, -0.6372, 0.8493, 0.0, 0.0, 2.6902, 3.6327, 0.904 )
      #Step 2 Index names
      names(step2CoeffNaturalRegeneration) <- c("Step2InterceptIndex",
        "Step2QIndex",
        "Step2InQIndex",
        "Step2NtotIndex",
        "Step2InSiIndex",
        "Step2WetIndex",
        "Step2DryIndex",
        "Step2RichIndex",
        "Step2PoorIndex",
        "Step2SigmaSIndex",
        "Step2SigmaPIndex",
        "Step2BiasIndex")

      temp <- step2CoeffNaturalRegeneration["Step2InterceptIndex"]+
            +step2CoeffNaturalRegeneration["Step2QIndex"]*q+
            +step2CoeffNaturalRegeneration["Step2InQIndex"]*log(Qind)+
            +step2CoeffNaturalRegeneration["Step2NtotIndex"]*stemTotal+
            +step2CoeffNaturalRegeneration["Step2InSiIndex"]*log(SI)+
            +step2CoeffNaturalRegeneration["Step2WetIndex"]*Wet+
            +step2CoeffNaturalRegeneration["Step2DryIndex"]*Dry+
            +step2CoeffNaturalRegeneration["Step2RichIndex"]*Rich+
            +step2CoeffNaturalRegeneration["Step2PoorIndex"]*Poor

      randomGauss <- if(!Deterministic){
        runif(n=1)
      }

      proportionOfConifer <- ifelse(
        Deterministic,
        (exp(temp)/(exp(temp)+1))*step2CoeffNaturalRegeneration["Step2BiasIndex"],
        (exp(temp + (randomGauss*sqrt(step2CoeffNaturalRegeneration["Step2SigmaPIndex"])))/ (exp(temp + (randomGauss*sqrt(step2CoeffNaturalRegeneration["Step2SigmaPIndex"]))) + 1))
      )

      stopifnot(proportionOfConifer>0 & proportionOfConifer<1,"Proportion of Conifer must be between 0 and 1")

      return(
        proportionOfConifer
      )


    }




    StemsPerSpecies <- function(
      ProportionOfConifer,
      Qind,
      SI,
      Wet,
      Dry,
      Rich,
      Poor,
      Hwod,
      Hwd,
      Shrubs,
      Lichen,
      Deterministic
    ){
      ProportionOfDeciduous <- 1-ProportionOfConifer

      step3CoeffNaturalRegeneration <- list(-7.9523, 1.8529, 0.0, 0.0, 0.0, 1.2180, 0.0, 0.0, -1.3832, -0.4425, 1.1268, 2.1238, 7.2952, 5.6084, 0.937)
      names(step3CoeffNaturalRegeneration) <- c(
        "Step3InterceptIndex",
        "Step3InQIndex",
        "Step3PlSpruceIndex",
        "Step3InSiIndex",
        "Step3WetIndex",
        "Step3DryIndex",
        "Step3RichIndex",
        "Step3PoorIndex",
        "Step3HwodIndex",
        "Step3HwdIndex",
        "Step3ShrubsIndex",
        "Step3LichenIndex",
        "Step3SigmaSIndex",
        "Step3SigmaPIndex",
        "Step3BiasIndex"
      )



    }


    }


  }












  #Step 2 Proportion of Conifers for ..


  step2CoeffPinesown <- list(17.2044, 0.1035, -5.2596, -0.00018, 0.0, 0.0, 0.0, -0.3590, 0.6612, 1.8858, 3.7452, 0.889)

  step2Pineplantation <- list(-11.9666, 0.0, 3.2567, -0.00014, 0.0, -0.8526, 0.6199, 0.0, 0.0, 2.1187, 4.8840, 0.918)

  step2Spruceplantation <- list(-6.0583, 0.0, 1.9416, -0.00014, 0.0, 0.0, 0.0, 0.0, 0.0, 2.2588, 4.0676, 0.882)

  step2Contortaplantation <- list(-13.6254, 0.0, 3.7771, -0.00027, 0.0, 0.0, 0.4915, 0.0, 0.0, 1.8212, 4.0106, 0.892 )

  step2Deciduousplantation <- list(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 )










  #Step 3 Proportion of Conifers for..


  step3CoefPineSown <- list(-12.7319, 3.4089, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -1.8143, -0.8255, 0.5712, 0.9929, 3.9482, 9.0704, 0.828 )

  step3CoeffPineplantation <- list(-8.6426, 2.4374, -3.2733, 0.0, -0.6729, 0.8271, 0.0, 0.0, 0.0, 0.0, 0.0, 1.1508, 9.4498, 8.5676, 0.832)

  step3CoeffSpruceplantation <- list(4.8204, 1.0575, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0575, 0.3764, -2.0587, -2.0208, 6.7043, 6.2259, 0.893)

  step3CoeffContortaplantation <- list(1.8124, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.1214, 4.0225, 0.826 )

  #Step 3 Index Names







  #Coefficients for Heights variation according to table 4b. (source?)

  step4bCoeffPine <- list(0.3242, -0.12590, -0.00928, 0.2684, 0.03563, 0.01511, 0.02382, 1.02)
  step4bCoeffSpruce <- list(1.1065, -0.09513, -0.00483, 0.0000, 0.02813, 0.01557, 0.03487, 1.02)
  step4bCoeffContorta <- list(0.4902, -0.05924, -0.00113, 0.0000, 0.00000, 0.00130, 0.00250, 1.02)
  step4bCoeffDeciduous <- list(0.8290, -0.07222, -0.00249, 0.0000, 0.00000, 0.01664, 0.03348, 1.02)

  #Step 4 B Indexes
  Step4BInterceptIndex <- 1
  Step4BHIndex <- 2
  Step4BQIndex <- 3
  Step4BInQIndex <- 4
  Step4BNatFIndex <- 5
  Step4BSigmaSIndex <- 6
  Step4BSigmaPIndex <- 7
  Step4BBiasIndex <- 8




  #Coefficients for relative height, table 4a (source?)
  step4CoeffNRegenerationDeciduous <- list( 0.5086, -1.0196, 0.0, 0.0301, 0.0, -0.1075, 0.1458, 0.0822, 0.1576, 1.08 )
  step4CoeffNRegenerationSpruce <- list( 0.2296, -0.9488, 0.0, 0.0200, 0.0, 0.0, 0.0, 0.0562, 0.1257, 1.06 )
  step4CoeffPineSownDeciduous <- list( 0.9896, -1.0873, 0.0, 0.0, 0.1435, -0.1917, 0.0, 0.1470, 0.1639, 1.08 )
  step4CoeffPineSownSpruce <- list( 0.4598, -1.2752, 0.0222, 0.0, 0.0, 0.0, 0.0, 0.0707, 0.0969, 1.06 )
  step4CoeffPinePlantationDeciduous <- list( 0.9896, -1.0873, 0.0, 0.0, 0.1435, -0.1917, 0.0, 0.1470, 0.1639, 1.08 )
  step4CoeffPinePlantationSpruce <- list( 0.4598, -1.2752, 0.0222, 0.0, 0.0, 0.0, 0.0, 0.0707, 0.0969, 1.06 )
  step4CoeffSprucePlantationDeciduous <- list( 2.1372, -1.0943, 0.0, -0.2628, 0.0, 0.0, 0.0, 0.1260, 0.1261, 1.08 )
  step4CoeffSprucePlantationPine <- list( 3.8108, -1.0346, 0.0, -0.9054, 0.0, 0.0, 0.0, 0.0427, 0.0973, 1.06 )
  step4CoeffContortaPlanationDeciduous <- list( 1.8398, -1.6750, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0366, 0.1461, 1.08 )
  step4CoeffContortaPlanationPine <- list( 2.9042, -0.7510, 0.0, -0.8629, 0.0, 0.0, 0.0, 0.0623, 0.0931, 1.06 )
  step4CoeffContortaPlanationSpruce <- list( 0.6600, -1.2462, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0217, 0.1818, 1.06 )

  #Index names (note +1 compared to C##)
  Step4AInterceptIndex <- 1
  Step4AInHIndex <- 2
  Step4ASiIndex <- 3
  Step4AInSiIndex <- 4
  Step4AHerbIndex <- 5
  Step4ADryIndex <- 6
  Step4AWetIndex <- 7
  Step4ASigmaSIndex <- 8
  Step4ASigmaPIndex <- 9
  Step4ABiasIndex <- 10




  #Coefficients for Weibull-distribution (scale)
  step5CoeffPineB  <- list( 1.30058, 1.08544, 0.0, -1.55305, 0.63942, 0.0)
  step5CoeffSpruceB <- list( 1.31357, 1.07955, 0.0, -1.52749, 0.63053, 0.0)
  step5CoeffContortaB <- list( 0.41484, 1.06154, 0.0, -0.15508, 0.20504, 0.0)
  step5CoeffDeciduousB <- list( 1.12188, 1.08160, 0.0, -1.30002, 0.51977, 0.0)

  #Coefficients for Weibull-distrubtion (shape)
  step5CoeffPineC <- list( 0.32591, 0.00485, -0.02888, -0.42790, -0.95373, 0.060 )
  step5CoeffSpruceC <- list( 0.32605, 0.0, -0.02401, -0.34569, -0.98817, 0.047 )
  step5CoeffContortaC <- list( 0.34883, 0.0, 0.19180, -0.62607, -0.84180, 0.041 )
  step5CoeffDeciduousC <- list( 0.26635, 0.0, 0.01323, -0.29358, -0.96208, 0.055 )

  #Indexes for Weibulls.
  Step5InterceptIndex <- 1
  Step5SpHeightIndex <- 2
  Step5InSpHeightIndex <- 3
  Step5SpCvhIndex <- 4
  Step5InSpCvhIndex <- 5
  Step5BBiasIndex <- 6



  #Undefined tree species
  dummyCoeff <- list(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)








