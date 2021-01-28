#' SDIplot
#'
#'@param slope Slope of reference curve for the species when on a log-log scale.
#'@param start Lowest SDI to plot
#'@param stop Highest SDI to plot
#'@param interval Curve interval
#'@param maxD maximum diameter to plot to
#'@param xlabel Change the label of the x-axis
#'@param ylabel Change the label of the y-axis
#'
#'@return A plot of the functions SDI=start to SDI=stop on a log log scale after Reineke (1933)
#'@export
#'@examples
#'
#'SDI_plot(slope=-1.605)
#'

SDIplot <- function(slope,start=100,stop=1000,interval=100,maxD=40,xlabel="Average Diameter in Inches",ylabel="Trees per Acre"){
  slope %>%
    assert_is_numeric()
    if (!(slope<0)){
      abort(paste0(
        "`The slope must be negative!"
      ))
    }
  start %>%
    assert_is_numeric()
    if (!(stop>0)){
      abort(paste0(
        "`The start value must be a positive number!"
      ))
    }
  stop %>%
    assert_is_numeric() %>%
    is_greater_than(start)
  if (!(stop>0)){
    abort(paste0(
      "`The stop value must be a positive number!"
    ))
  }

  interval %>%
    assert_is_numeric()

  if (!((stop-start)%%interval==0)){
    abort(paste0(
      "`The distance stop-start must be evenly divisible by interval"
    ))
  }
  maxD %>%
    assert_is_numeric()
  if (!(maxD>0)){
    abort(paste0(
      "`The maximal average diameter to plot is not a length > 0!"
    ))
  }

maplist <- data.frame(SDI=c(seq(from=start,to = stop,by = interval)))

maplist <- maplist %>%
  mutate(func= map(SDI,forester::SDI_func,slope=slope))

maplist <- maplist %>%
  mutate(xcordlab=maxD)

maplist <- maplist %>% rowwise() %>% mutate(ycordlab= map(.x=xcordlab,.f=func)) %>% unnest(ycordlab)

maplist$SDI <- factor(maplist$SDI)
maplist$SDI <- relevel(maplist$SDI, max(as.numeric(maplist$SDI)))
maplist$SDI <- ordered(maplist$SDI)

maplist <- maplist %>% arrange(SDI)

maplist$linetype <- c(1,rep(2,nrow(maplist)-1))

maplist$linetype <- factor(maplist$linetype, levels=c(1,2))

dev.new(width=7,height=10)

ggplot()+
  scale_x_continuous(limits = c(1,maxD),breaks=c(1,2,3,4,5,6,7,8,9,c(seq(10,maxD,10))),trans=log_trans())+
  scale_y_continuous(limits=c(10,41000),breaks = c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000,30000,40000),trans=log_trans())+
  lapply(seq(1,nrow(maplist)),
         function(i) geom_function(fun = maplist$func[[i]],
                                   aes(linetype=maplist$linetype[i])))+
  lapply(seq(1,nrow(maplist)),
         function(i) annotate("text",
                              label=paste("  SDI = ",maplist$SDI[i]),
                              x=maplist$xcordlab[i],
                              y=maplist$ycordlab[i],
                              hjust=0,
                              size=3))+
  theme(legend.position = "none")+
  coord_cartesian(expand = FALSE,clip="off",xlim = c(1,maxD))+
  theme(plot.margin=unit(c(1,2,0.5,0.5),"cm"))+
  xlab(xlabel)+
  ylab(ylabel)

}
