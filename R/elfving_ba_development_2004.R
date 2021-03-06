#' Elfving Tree level Basal Area Development Functions
#'
#'@description The dependent variable is 5 yr. increment of \strong{squared} diameter at breast height, cm2.
#'  IMPORTANT: This dataset supercedes Elfving, B. (2001). Previous functions did not utilise tree age, which later was shown necessary for accurate long-term predictions.
#'  Functions for tree age were developed.
#'
#'
#' @source from the manuscript Elfving, B. (2004). "Individual-tree basal area growth functions for all Swedish forests" (Unpubl.)
#'         Readable: \url{https://www.heurekaslu.se/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'         Suitable for use together with `forester::formulator`.
#'
#'         The dependent variable is 5 yr. increment of \strong{squared} diameter at breast height, cm2.
#'
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{d}{diameter at breast height (1.3 m above ground) including bark, cm}
#'   \item{d2gt}{d^2 /1000}
#'   \item{lndp1}{ln (d+1)}
#'   \item{md}{basal area weighted mean diameter of the trees on the plot, cm   (sum d^3 / sum d^2 )}
#'   \item{md2gt}{(md)^2 /1000}
#'   \item{mdf}{1 – 1 / exp ( (14/md)^2 )}
#'   \item{ba}{basal area, m² / ha}
#'   \item{bal}{basal area of larger trees, i.e. trees on the plot with d > than that of  the target tree,  m^2 / ha}
#'   \item{balgdp1}{bal / (d + 1)}
#'   \item{invbap10}{1 / (ba + 10)}
#'   \item{propos2}{(ba proportion of other species on the plot)^2}
#'   \item{propa05}{(ba proportion of actual species)0.5}
#'   \item{gotland}{indicator for the island Gotland in the Baltic sea}
#'   \item{tsum}{temperature sum, day-degrees > 5 ºC during vegetation period = 4835 - 57.6·la t - 0.9·alt}
#'   \item{ts}{0.001·tsum}
#'   \item{ts2}{ts^2}
#'   \item{invtsm03}{1 / (ts-0.3)}
#'   \item{invdcp30}{1 / (dc + 30), where dc is distance to coastline, km}
#'   \item{lat}{latitude, degrees N}
#'   \item{alt}{altitude, m a.s.l.}
#'   \item{alt2}{alt2=alt^2}
#'   \item{peat}{indicator for plots covered by peat to more than 50 percent}
#'   \item{shallow}{takes the value 1 if soil depth is 20-70 cm or very variable and the value 2 if depth < 20  cm}
#'   \item{moist}{indicator for moist site}
#'   \item{wet}{indicator for wet site}
#'   \item{mswat}{presence of movable soil water, never=0, seldom =1, often =2}
#'   \item{expos}{exposition of slope, se-w = 1, nw-e = -1}
#'   \item{text}{soil texture, eight classes from coarse-textured (stone=1) to fine-textured (clay=8)}
#'   \item{text3gt}{text^3 /1000; for peat-land the variable text should have the value 5}
#'   \item{ditch}{indicator for plot with ditch within 25 m from plot centre}
#'   \item{thin}{indicator for plots thinned within 15 years before end of the five-year growth period}
#'   \item{fert}{Takes a value between 0-1 on fertilised plots, according to expected response, see text}
#'   \item{part}{indicator for a partitioned plot where the other part not is open land}
#'   \item{edgeff}{indicator for a partitioned plot where the other part is open land}
#'   \item{lnbarel}{ln ((ba+1) / (bar+1)) , where bar is ba in surrounding stand measured with relascope}
#'   }
#'
#' @examples
#' \dontrun{
#' elfving_gy_utveckling_coef_correction
#' }
"elfving_gy_utveckling_correction"
