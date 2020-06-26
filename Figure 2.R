#### Figure 2 of the paper 
# Who compensates? An assessment of loss and vulnerability to household economy in India due to COVID-19 lockdown
# Bino Paul, Unmesh Patnaik, Kamal Kumar Murari, Santosh Kumar Sahu, T. Muralidharan


DF1 <- read.csv("DF1Data.csv")
DF2<- read.csv("DF2Data.csv")

## Note: Data frame DF1 and DF2 are derived from the Table S3 of the manuscript

library(ggplot2)

##### plotting the main Figure 

library(ggpubr)
p1=ggplot(DF1,aes(x=Sectors,y=Total.Median,fill=Sectors))+
  geom_col(width=0.4)+geom_errorbar(aes(ymin =Total.Lower, 
                               ymax =Total.Upper), 
                           width=0.1,col="blue",lwd=1.2)+
ylab("Estimated loss in Billion USD")+theme_bw()+
  geom_point(aes(x=Sectors,y=Total.Median*WageRatio),
             size=27, col="red",shape="_")+
  theme(legend.position="none")+
  annotate("text",x=3,y=80,label="Period: March 25 to May 31")
p1
par(new=T)
plot.new()
legend("topright",legend=c("Wage loss","Uncertainty bounds")
       ,lty=c(1,1),col=c("red","blue"),lwd=c(6,3),bty="n")


p2=ggplot(DF2,aes(x=Short.name,y=WageR.Ratio.perc,fill=Category))+
  geom_col()+theme_bw()+
  theme(axis.text.x = element_text(angle =90, hjust =1))+
  ylab("Wage loss\n(% of total)")+
  xlab("")
p2

p3=ggplot(DF1,aes(x=Sectors,y=L1.Median,fill=Sectors))+
  geom_col(width=0.4)+geom_errorbar(aes(ymin =L1.Lower, 
                                        ymax =L1.Upper), 
                                    width=0.1,col="blue",lwd=1.2)+
  ylab("Estimated loss in Billion USD")+theme_bw()+
  geom_point(aes(x=Sectors,y=L1.Median*WageRatio),
             size=27, col="red",shape="_")+
  theme(legend.position="none")+
  annotate("text",x=3,y=35,label="Period: March 25 to April 14")
p3


p4=ggplot(DF1,aes(x=Sectors,y=L2.Median,fill=Sectors))+
  geom_col(width=0.4)+geom_errorbar(aes(ymin =L2.Lower, 
                                        ymax =L2.Upper), 
                                    width=0.1,col="blue",lwd=1.2)+
  ylab("Estimated loss in Billion USD")+theme_bw()+
  geom_point(aes(x=Sectors,y=L2.Median*WageRatio),
             size=27, col="red",shape="_")+
  theme(legend.position="none")+
  annotate("text",x=3,y=20,label="Period: April 15 to May 03")
p4

p5=ggplot(DF1,aes(x=Sectors,y=L3.Median,fill=Sectors))+
  geom_col(width=0.4)+geom_errorbar(aes(ymin =L3.Lower, 
                                        ymax =L3.Upper), 
                                    width=0.1,col="blue",lwd=1.2)+
  ylab("Estimated loss in Billion USD")+theme_bw()+
  geom_point(aes(x=Sectors,y=L3.Median*WageRatio),
             size=27, col="red",shape="_")+
  theme(legend.position="none")+
  annotate("text",x=3,y=13,label="Period: May 04 to May 17")
p5


p6=ggplot(DF1,aes(x=Sectors,y=L4.Median,fill=Sectors))+
  geom_col(width=0.4)+geom_errorbar(aes(ymin =L4.Lower, 
                                        ymax =L4.Upper), 
                                    width=0.1,col="blue",lwd=1.2)+
  ylab("Estimated loss in Billion USD")+theme_bw()+
  geom_point(aes(x=Sectors,y=L4.Median*WageRatio),
             size=27, col="red",shape="_")+
  theme(legend.position="none")+
  annotate("text",x=3,y=13,label="Period: May 18 to May 31")
p5
p6


fig=ggarrange(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2,
              labels = c("(a)", "(b)","(c)","(d)","(e)","(f)"),
              hjust=0.08)%>%
  ggexport(filename = "Figure2.jpeg",width=4300,height=2800,
           res=300)
