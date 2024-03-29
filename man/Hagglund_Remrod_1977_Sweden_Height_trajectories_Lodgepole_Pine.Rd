% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/Hagglund_Remrod_1977_Sweden_Height_trajectories_Lodgepole_Pine.R
\name{Hagglund_Remrod_1977_Sweden_Height_trajectories_Lodgepole_Pine}
\alias{Hagglund_Remrod_1977_Sweden_Height_trajectories_Lodgepole_Pine}
\title{Hägglund, Remröd 1977 Height growth of Lodgepole Pine Pinus contorta in Northern Sweden.}
\source{
Hägglund, Björn, Remröd, Jan (1977) Övre höjdens utveckling i bestånd med Pinus contorta.
HUGIN rapport nr. 4. Print-out. Available at Forest Library, Umeå.
}
\usage{
Hagglund_Remrod_1977_Sweden_Height_trajectories_Lodgepole_Pine(
  age_at_breast_height,
  top_height_m,
  age_2,
  output = "SIH50"
)
}
\arguments{
\item{age_at_breast_height}{Age of stand or tree at breast height 1.3 m.}

\item{top_height_m}{Top height of tree or stand in m.}

\item{age_2}{Necessary if output is "Height". The age for which height along
the same curve is to be computed.}

\item{output}{One of "SIH100", "Height", or "Equation".}
}
\value{
If output is "SIH100", the numeric value for the height (dm) a stand
will reach at age 100.

 If output is "Height", the numeric value for the height (dm) a stand will
 reach at age_2.

 If output is "Equation", a named list with 2 elements: I) "Equation": text response with the equation for that height
 curve. II) "T13": The time taken to reach breast height 1.3m.
}
\description{
Largely based on the Fortran IV script appended to the original source.
has been adapted to R such that it will return one value at a time rather than
print a matrix for height developments. OBSERVE! No function was available at the time
for time until breast height for Pinus contorta in Sweden. Studies, e.g. fig. 1 in source
shows that it seemed to follow natural regeneration of Scots Pine well, but
a connection was refrained from as new planting opportunities were being evaluated.
}
\details{
OBSERVE! Will throw warnings if outside of material, but not stop.

Based on the Chapman-Richards function.
}
\examples{
ggplot()+
geom_function(aes(linetype="Lodgepole Pine\n(Hägglund & Remröd 1977)\nh[50]\n"), fun = function(age) (13+288.435733147131*(1-exp(-age*0.0121359757121435))^1.03593431330045)/10)+
  geom_function(aes(linetype="Lodgepole Pine\n(Hägglund & Remröd 1977)\nh[50]\n"), fun= function(age) (13+328.578193602562*(1-exp(-age*0.0144478357572835))^1.0250418112026)/10)+
  geom_function(aes(linetype="Lodgepole Pine\n(Hägglund & Remröd 1977)\nh[50]\n"), fun= function(age) (13+368.699928131572*(1-exp(-age*0.0167678399608579))^1.01362095232006)/10)+
  geom_function(aes(linetype="Lodgepole Pine\n(Hägglund & Remröd 1977)\nh[50]\n"), fun= function(age) (13+405.718962027163*(1-exp(-age*0.0189158559922424))^1.00267824162738)/10)+
  geom_function(aes(linetype="Scots Pine\n(Hägglund 1974)\nh[100]\n"), fun= function (age) (13+234.725854492188*(1-exp(-age*0.0174101983378584))^1.13907963858789)/10)+
  geom_function(aes(linetype="Scots Pine\n(Hägglund 1974)\nh[100]\n"), fun= function (age) (13+269.555444335938*(1-exp(-age*0.0210388040153862))^1.11546448678253)/10)+
  geom_function(aes(linetype="Scots Pine\n(Hägglund 1974)\nh[100]\n"), fun= function (age) (13+303.224047851563*(1-exp(-age*0.0247184794701687))^1.10210374208523)/10)+
  geom_function(aes(linetype="Scots Pine\n(Hägglund 1974)\nh[100]\n"), fun= function (age) (13+334.570678710938*(1-exp(-age*0.0282856703465072))^1.0941196674377)/10)+
  scale_x_continuous(limits=c(0,60), breaks=seq(0,60,10), expand=c(0,0))+
  scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5), expand=c(0,0))+
  theme_classic()+
  labs(title="Comparison between height development for\nPinus contorta and Pinus sylvestris in Sweden",
       x="Age at breast height (years)",
       y="Dominant height (meters)")+
  theme(legend.position = c(0.70,0.2),
        plot.title = element_text(family="serif",face="italic"),
        plot.margin=unit(c(10,30,10,10),units="points"),
        legend.title = element_blank())+
  coord_cartesian(xlim = c(0,74), clip="off")+
  annotate("text",x=50, y=29.5,label=paste("h[50]"),parse=TRUE,size=3)+
  annotate("text",x=55, y=29.5,label=paste("h[100]"),parse=TRUE,size=3)+
  annotate("text",x=50,y=25,label="26",size=3)+
  annotate("text",x=55,y=25,label="32",size=3)+
  annotate("text",x=50,y=21,label="22",size=3)+
  annotate("text",x=55,y=21,label="28",size=3)+
  annotate("text",x=50,y=17,label="18",size=3)+
  annotate("text",x=55,y=17,label="24",size=3)+
  annotate("text",x=50,y=13,label="14",size=3)+
  annotate("text",x=55,y=13,label="20",size=3)
}
