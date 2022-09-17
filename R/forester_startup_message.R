.onAttach <- function(libname, pkgname) {
  packageStartupMessage("forester is being actively developed.\n
                        For News and issues, go to 'https://github.com/Silviculturalist/forester'\n
                        Breaking changes can take place at any time.\n
                        \n
                        Saturday 17 September: Regression of Swedish Forest Agency Thinning Recommendations 1984 added.\n
                        Saturday 4 June: Massive update to structure of Soderbergs Individual Tree functions.\n
                        Tuesday 14 June: New data & vignette available! ?Hoffmann_1982 \n
                        Thursday 7 July: Large errors found in SIS_est() for Hägglund, B. & Lundmark J-E ståndortsboniteringssystem. Therefore no longer exported until resolved.\n
                        Friday 8 July: Strong clarifications of Hägglund 1972 & 1973 functions, which took age at breast height and reported Site Index as total age.\n
                                       Introduced functions to calculate Time to Breast height for these functions.\n
                        Saturday 9 July: Analogues to previous change on Friday 8 July also for Hägglunds function for Scots Pine (1974)."
                        )
}
