list.of.packages <- c("data.table","readr","Hmisc","stringr","scales","reshape2","OneR","varhandle","WDI")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd="E:/git/gha/2019"
setwd(wd)

CPIA=WDI(country="all",indicator = c("IQ.CPA.TRAN.XQ","SP.POP.TOTL"),start=1981,end=2017,extra=T)


poverty=read.csv("E:/git/poverty_trends/data/data_warehouse/pop_in_poverty.csv")
povHC=read.csv("E:/git/poverty_trends/data/data_warehouse/poor_pop.csv")

CPIA=setnames(CPIA,"year","Year")
CPIA=setnames(CPIA,"iso2c","di_id")
CPIA=CPIA[,c("di_id","iso3c","IQ.CPA.TRAN.XQ","Year","SP.POP.TOTL")]


dat=merge(CPIA,poverty,by=c("di_id","Year"))
dat=merge(dat,povHC,by=c("di_id","Year"))
setnames(dat,"IQ.CPA.TRAN.XQ","CPIA")
dat2017=subset(dat, Year==2017)
dat2010=subset(dat, Year==2010)


ggplot(dat2017, aes(x=CPIA,y=(Poorpop.Interp/1000000)))+
  geom_bar(stat="identity")+
  labs(x="World Bank Country Policy and Institutional Assessment (CPIA)",y="People in extreme poverty\n(millions)",title="CPIA and Extreme Poverty 2015",fill="")+
  theme_classic()
ggsave("output/CPIApoorpop2015.jpeg")
ggplot(dat2010, aes(x=CPIA,y=Poorpop.Interp))+
  geom_bar(stat="identity")+
  labs(x="World Bank Country Policy and Institutional Assessment (CPIA)",y="People in extreme poverty\n(millions)",title="CPIA and Extreme Poverty 2010",fill="")+
  theme_classic()
ggsave("output/CPIApoorpop2010.jpeg")

ggplot(dat2017, aes(x=CPIA,y=ExtPovHC.Interp,fill=ExtPovHC.Interp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(x="World Bank Country Policy and Institutional Assessment (CPIA)",y="Population in extreme poverty\n(percentage)",title="CPIA and Extreme Poverty 2015",fill="")+
  theme_classic()+
  theme(legend.position="none")
ggsave("output/CPIApoorHC2015.jpeg")
ggplot(dat2010, aes(x=CPIA,y=ExtPovHC.Interp,fill=ExtPovHC.Interp))+
  geom_point()+
  geom_smooth(method=lm)+
  labs(x="World Bank Country Policy and Institutional Assessment (CPIA)",y="Population in extreme poverty\n(percentage)",title="CPIA and Extreme Poverty 2010",fill="")+
  theme_classic()+
  theme(legend.position="none")
ggsave("output/CPIApoorHC2010.jpeg")


OECDfrag=read.csv("https://raw.githubusercontent.com/githubIEP/oecd-sfr-2018/master/data_out/two-tier%20PCA%20-%20PC1%20as%20Fragility%20Score.csv")
OECDfr=OECDfrag[,c("iso3c","fragility")]
dat2017=merge(dat2017,OECDfr,by=c("iso3c"))

dat2017$fragbin=as.numeric(gsub("]","",sapply(strsplit(as.character(bin(dat2017$fragility,nbins=10)),","),`[[`,2)))

ggplot(dat2017, aes(x=fragbin,y=(Poorpop.Interp/1000000)))+
  geom_bar(stat="identity")+
  labs(x="OECD States of Fragility Index Score (2018)",y="Population in extreme poverty\n(millions in 2015)",title="OECD Fragility and and Extreme Poverty 2015",fill="")+
  theme_classic()
ggsave("output/StatFragpoorpop2015.jpeg")
ggplot(dat2017, aes(x=fragility,y=ExtPovHC.Interp,fill=ExtPovHC.Interp))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(x="OECD States of Fragility Index Score (2018)",y="Population in extreme poverty\n(percentage in 2015)",title="OECD Fragility and and Extreme Poverty 2015",fill="")+
  theme_classic()+
  theme(legend.position = "none")
ggsave("output/StatFragpoorHC2015.jpeg")

download.url="https://gain.nd.edu/assets/311783/resources_2019_19_01_21h59_1_.zip"
tmp.zip=tempfile(fileext=".zip")
download.file(download.url,tmp.zip)
zip.contents=unzip(tmp.zip,files="resources/gain/gain.csv")
gain=fread(zip.contents,header=T)
gain.m=melt(gain,id.vars = c("ISO3","Name"),variable.name="Year")
setnames(gain.m,"ISO3","iso3c")
setnames(gain.m,"value","gain")
dat2017=merge(dat2017,gain.m,by=c("iso3c","Year"))

dat20172=subset(dat2017,!is.na(gain))
dat20172$gainbin=as.numeric(gsub("]","",sapply(strsplit(as.character(bin(dat20172$gain,nbins=10)),","),`[[`,2)))

ggplot(dat20172, aes(x=gainbin,y=(Poorpop.Interp/1000000)))+
  geom_bar(stat="identity")+
  labs(x="Notre Dame Global Adaptation Initiative (ND-GAIN Index) (2018)",y="Population in extreme poverty\n(millions in 2015)",title="ND-GAIN Vulnerability to Climate Change Index and\nExtreme Poverty 2015",fill="")+
  theme_classic()
ggsave("output/NDGAINpoorpop2015.jpeg")
ggplot(dat20172, aes(x=gain,y=ExtPovHC.Interp,fill=ExtPovHC.Interp))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(x="Notre Dame Global Adaptation Initiative (ND-GAIN Index) (2018)",y="Population in extreme poverty\n(millions in 2015)",title="ND-GAIN Vulnerability to Climate Change Index and\nExtreme Poverty 2015",fill="")+
  theme_classic()+
  theme(legend.position="none")
ggsave("output/NDGAINpoorHC2015.jpeg")


