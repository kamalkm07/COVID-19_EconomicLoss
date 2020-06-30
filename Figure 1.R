
#### Figure 1 of the paper 
# Who compensates? An assessment of loss and vulnerability to household economy in India due to COVID-19 lockdown
# Bino Paul, Unmesh Patnaik, Kamal Kumar Murari, Santosh Kumar Sahu, T. Muralidharan


### Accessing data from CCSE-JHU
urlnew_rec="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
urlnew_deaths="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
urlnew_cases="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
library(tidyverse)
Covid_Recovered<- read_csv(urlnew_rec)
Covid_Deaths= read_csv(urlnew_deaths)
Covid_Cases=read_csv(urlnew_cases)
#save(Covid_Cases,file="F:/BigDataInitiatives/EPGDA/Introduction2R/COVID19/JHU-CCSS_Cases.Rdata")
#load("F:/BigDataInitiatives/EPGDA/Introduction2R/COVID19/JHU-CCSS_Cases.Rdata")
#save(Covid_Deaths,file="F:/BigDataInitiatives/EPGDA/Introduction2R/COVID19/JHU-CCSS_Deaths.Rdata")
#load("F:/BigDataInitiatives/EPGDA/Introduction2R/COVID19/JHU-CCSS_Deaths.Rdata")
#save(Covid_Recovered,file="F:/BigDataInitiatives/EPGDA/Introduction2R/COVID19/JHU-CCSS_Recovered.Rdata")
#load("F:/BigDataInitiatives/EPGDA/Introduction2R/COVID19/JHU-CCSS_Recovered.Rdata")
country="India"
Data=cbind(t(subset(Covid_Cases,Covid_Cases$`Country/Region`==country)),
           t(subset(Covid_Deaths,Covid_Deaths$`Country/Region`==country)),
           t(subset(Covid_Recovered,Covid_Recovered$`Country/Region`==country)))
DF=as.data.frame(Data[5:length(Data[,1]),])
names(DF)=c("Cases","Deaths","Recovered")
DF$Cases=as.numeric(as.character(DF$Cases))
DF$Deaths=as.numeric(as.character(DF$Deaths))
DF$Recovered=as.numeric(as.character(DF$Recovered))
DF=subset(DF,DF$Cases<195000) #### this is to end the data till May 31
x=as.Date(1:length(DF$Cases),"2020/1/21")
xstart=as.Date("2020/1/30") ## Date of first reporting of COVID-19 case in India
lock1=as.Date("2020/3/24") ## Date for lockdown 1
lock2=as.Date("2020/4/15") ## Date for lockdown 2
lock3=as.Date("2020/5/04") ## Date for lockdown 3
lock4=as.Date("2020/5/19") ## Date for lockdown 4
lockend=as.Date("2020/5/31") ## End date for the analysis period
today=Sys.Date()
GRCase=round((DF$Cases[length(DF$Cases)]^(1/121)-1)*100,2)
GRRecovery=round((DF$Recovered[length(DF$Recovered)]^(1/106)-1)*100,2)
GRDeath=round((DF$Deaths[length(DF$Deaths)]^(1/81)-1)*100,2)
DFnew=data.frame(Date=x,Legend=c(rep("Cases",length(DF$Cases)),rep("Deaths",length(DF$Deaths)),rep("Recovered",length(DF$Recovered))),Values=c(DF$Cases,DF$Deaths,DF$Recovered))
coltype=c("blue","red","green")
library(ggplot2)
p4=ggplot(DFnew,aes(x=Date,y=Values,col=Legend))+
  geom_line(size=1.5)+theme_bw()+
  geom_vline(xintercept =xstart,col="red",size=1.5)+
  annotate("rect",xmin=lock1,xmax=lock2,ymin=0,ymax=Inf,alpha=0.2,fill="green")+
  annotate("rect",xmin=lock2,xmax=lock3,ymin=0,ymax=Inf,alpha=0.2,fill="blue")+
  annotate("rect",xmin=lock3,xmax=lock4,ymin=0,ymax=Inf,alpha=0.2,fill="orange")+
  annotate("rect",xmin=lock4,xmax=lockend,ymin=0,ymax=Inf,alpha=0.2,fill="red")+
  scale_color_manual(values=coltype)+annotate("text",x=xstart-2,y=85000,label="First Case Reported on 30 Jan",angle=90)+
  annotate("text",x=lock1+10,y=200000,label="Lockdown 1")+
  annotate("text",x=lock2+8,y=200000,label="Lockdown 2")+
  annotate("text",x=lock3+7,y=200000,label="Lockdown 3")+
  annotate("text",x=lock4+6,y=200000,label="Lockdown 4")+
  ylab("Cumulative values (Cases/Recovery/Deaths)")+
  annotate("text",x=xstart+25,y=180000,label=paste("Status of COVID pregression\n in India as on",format(x[length(x)],"%d %B")),size=5,col="red")+
  annotate("rect",xmin=xstart+3,xmax=lock1-5,ymin=10000,ymax=200000,fill="yellow",alpha=0.2)+
  annotate("text",x=xstart+25,y=150000,label=paste("Confirmed Cases-",DF$Cases[length(DF$Cases)],"(",GRCase,"%",")"),col="red",size=5)+
  annotate("text",x=xstart+25,y=135000,label=paste("Total Deaths-",DF$Deaths[length(DF$Deaths)],"(",GRDeath,"%",")"),col="red",size=5)+
  annotate("text",x=xstart+25,y=120000,label=paste("Total Recovered-",DF$Recovered[length(DF$Recovered)],"(",GRRecovery,"%",")"),col="red",size=5)+
  annotate("text",x=xstart+25,y=90000,label="Estimated Economic Loss to households \n (till May 31)",col="brown",size=5)+
  annotate("text",x=xstart+25,y=55000,label=paste("Loss in Bn. USD:\n 74.62 (59.05-92.86)"),col="brown",size=5)+
  annotate("text",x=xstart+65,y=170000,label=paste("Loss:32 \n(26.2-40.3)\n (Bn. USD)"),col="brown",size=5)+
  annotate("text",x=xstart+85,y=170000,label=paste("Loss:18.8 \n(15-22.8)\n (Bn. USD)"),col="brown",size=5)+
  annotate("text",x=xstart+102,y=170000,label=paste("Loss:12.1 \n(9.2-15)\n (Bn. USD)"),col="brown",size=5)+
  annotate("text",x=xstart+116,y=170000,label=paste("Loss:12 \n(8.9-15)\n (Bn. USD)"),col="brown",size=5)
  p4=p4+xlab("")
  p4
 
### plot the covid zones in the figure 
## read shapefile 
library(sf)
India.ds=st_read("India_district_covidzones_workers.shp")
### The shapefile is made by the author to identify districts with Red, Orange and Green zones

library(ggplot2)
map=ggplot(India.ds)+
  geom_sf(aes(fill=Zone),show.legend=F)+
  scale_fill_manual(values = c("green","white","orange","red"))
map=map+theme_bw()

### working with sf dataframe 
dfdist=India.ds$distname
dfzone=India.ds$Zone
dfmarwork=India.ds$margwork_p

DF1=data.frame(District=dfdist,Zone=dfzone,MarWork=dfmarwork)

#### load census data from file "Demographics_of_India.xlsx"
library(readxl)
Demographics_of_India <- read_excel("E:/Labour/Demographics_of_India.xlsx", 
                                    sheet = "Demographics_of_India")
hdfdist=India.HH$distname
hdfhh=India.HH$no_hh
hdf_m.hh.ind.pop=India.HH$main_hh_p
hdf_tot.work.pop=India.HH$tot_work_p
DF2=data.frame(District=hdfdist,NHH=hdfhh,MHHIndPop=hdf_m.hh.ind.pop,
               TotalWorkP=hdf_tot.work.pop)

DF=merge(DF1,DF2,by=interaction("District"))
DF=DF[-153,]
### Arranging the DF
library(tidyverse)
library(dplyr)
DataSum=DF %>%
  group_by(Zone)%>%
  summarise(MarWork=sum(MarWork,na.rm=T),
            MHHIndPop=sum(MHHIndPop,na.rm=T),
            NHH=sum(NHH,na.rm=T),
            TotWorkP=sum(TotalWorkP,na.rm=T))

library(reshape)
DataSum=DataSum%>%select(-c(MHHIndPop))
DataStack <- stack(DataSum,select=-c(Zone))
DataStack$Zone=rep(c("Green","Orange","Red"),3)
Data.Frame=data.frame(Zone=DataStack$Zone,VarC=DataStack$ind,
                      Values=DataStack$values/1000000)
p1=ggplot(Data.Frame,aes(x=VarC,y=Values,fill=Zone))+
  geom_bar(stat="identity",postion="stack")
p1=p1+scale_fill_manual(values=c("green","orange","red"))
p1=p1+theme_classic()+labs(fill="COVID-19 Zones")
p1=p1+xlab("Categories:\n \n MarWork=Total marginal workforce,\t\t NHH=Total Households,\t TotWorkP=Total workforce")
p1=p1+ylab("Counts in millions")
p1


library(ggpubr)
fig=ggarrange(p4,
              ggarrange(map,p1,ncol=2,labels = c("(b)", "(c)")),
              nrow=2,labels="(a)",hjust=0.05)%>%
ggexport(filename = "Figure1.jpeg",width=4300,height=2800,
         res=300)



