## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, include=FALSE--------------------------------------
library(forester)
library(scales)
library(tidyverse)
library(broom)
library(modelr)
library(gridExtra)
library(grid)

## ---- figures-side, echo=FALSE, eval=TRUE, fig.asp=1.375, fig.width=7, out.width="100%", dpi=300, warning=FALSE----

a<- ggplot()+
  xlab("Average Diameter")+
  ylab("Stems per Acre")+
  xlim(1,40)+
  ylim(0,30000)+
  coord_cartesian(ylim=c(0,30000))+
  geom_function(fun=~10^(4.605)*.x^(-1.605))
b<- ggplot()+
  xlab("Average Diameter")+
  ylab("Stems per Acre")+
  scale_x_continuous(limits = c(1,40),breaks=c(1,2,3,4,5,6,7,8,9,10,20,30,40),trans=log10_trans())+
  scale_y_continuous(limits=c(10,40000),breaks=c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000, 20000, 30000, 40000),trans=log10_trans())+
  geom_function(fun=~10^(4.605)*.x^-1.605)

gridExtra::grid.arrange(a,b,ncol=2)

## ----echo=FALSE, eval=TRUE----------------------------------------------------
c <- Reineke_1933 %>% filter(SPP=="Abies magnifica") %>% head()
d <- Reineke_1933 %>% filter(SPP=="Abies concolor") %>% head()
knitr::kable(
  list(c,d),
  caption="Reineke's White and Red Fir stands",
  bookstabs=TRUE, valign='t'
)

## ----echo=TRUE, warning=FALSE-------------------------------------------------

Reineke_all <- Reineke_1933 %>% mutate(SPP="All")
Reineke_1933 <- rbind(Reineke_1933,Reineke_all)

model1<- Reineke_1933 %>%
  group_by(SPP) %>%
  nest() %>%
  mutate(Model= map(data, ~nls(NperAcre~ a * AverageDiameterInches^(b),start=list(a=-1,b=2),data=.))) %>%
# extract formula from each model, convert to one-sided form, &
# replace coefficients with fitted values, & store in dataframe
# as character string
rowwise() %>%
  mutate(func = formula(Model) %>%
           as.character() %>%
           magrittr::extract(3) %>%
           gsub("AverageDiameterInches", ".x", ., fixed = T) %>%
           gsub("a", Model$m$getPars()[1], .) %>%
           gsub("b", Model$m$getPars()[2], .) %>%
           paste("~", ., collapse = "")) %>%
  ungroup()


## ---- regression-plot, echo=FALSE, eval=TRUE, fig.asp=1.375, fig.width=7, out.width="100%", warning=FALSE, dpi=300----
ggplot(data = model1$data[[3]]) +
  geom_point(aes(x = AverageDiameterInches, y = NperAcre))+

  # add formula in each row as a separate geom_function layer
  lapply(seq(1, 2),
         function(i) geom_function(fun = rlang::as_function(formula(model1$func[i])),
                                   aes(colour = model1$SPP[i]))) +

  # change legend name (can also change palette / labels / etc.)
  scale_colour_discrete(name = "SPP")+
  scale_x_continuous(limits = c(1,45),breaks=c(1,5,10,15,20,25,30,35,40,45),trans=log10_trans())+
  scale_y_continuous(limits=c(70,10000),breaks = c(70,80,90,100,200,300,400,600,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),trans=log10_trans())
  


## -----------------------------------------------------------------------------
SDI_func <- function(SDI){
  as.formula(paste("~",(10^(1.605+log10(SDI))), "*.x^(-1.605)"))
}

SDIlist <- data.frame(SDI=seq(from=1000, to=100,by=-100))

SDIlist <- SDIlist %>% 
  mutate(func= map(SDI,SDI_func))

SDIlist <- SDIlist %>% 
  mutate(xcordlab=40)

SDIlist <- SDIlist %>% rowwise() %>% mutate(ycordlab= map(.x=xcordlab,.f=func)) %>% unnest(ycordlab)

SDIlist$linetype <- c(1,rep(2,nrow(SDIlist)-1))

SDIlist$SDI <- factor(SDIlist$SDI, levels=c("1000","900","800","700","600","500","400","300","200","100"))
SDIlist$linetype <- factor(SDIlist$linetype, levels=c(1,2))
print(SDIlist)


## ---- SDI-plot, echo=TRUE, eval=TRUE, fig.asp=1.375, fig.width=7, out.width="100%", warning=TRUE, dpi=300----

ggplot()+
  scale_x_continuous(limits = c(1,40),breaks=c(1,2,3,4,5,6,7,8,9,10,20,30,40),trans=log_trans())+
  scale_y_continuous(limits=c(10,41000),breaks = c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,20000,30000,40000),trans=log_trans())+
  lapply(seq(1,nrow(SDIlist)),
         function(i) geom_function(fun = SDIlist$func[[i]],
                                   aes(linetype=SDIlist$linetype[i])))+
  lapply(seq(1,nrow(SDIlist)),
         function(i) annotate("text",
                              label=paste("  SDI ",SDIlist$SDI[i]),
                              x=SDIlist$xcordlab[i],
                              y=SDIlist$ycordlab[i],
                              hjust=0,
                              size=3))+
  theme(legend.position = "none")+
  coord_cartesian(expand = FALSE,clip="off",xlim = c(1,40))+
  theme(plot.margin=unit(c(1,2,0.5,0.5),"cm"))+
  xlab("Average Diameter in Inches")+
  ylab("Trees per Acre")
    

