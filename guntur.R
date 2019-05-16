mydata<-read.csv(file = "~/final_csv_guntur (2).csv")
library(caTools)
library(forecast)
library(AER)
mtraining<-subset(mydata,year>=1996&year<=2001)
mtesting<-subset(mydata,year==2002&TEH_NAME!="Veldurti")
pmodel2<-lm(formula=PREMON~POMRB+TEH_NAME+year+temp_pomrb+temp_premon+rain_premon, mtraining)
pmodel1<-glm(formula=POMRB~TEH_NAME+year+temp_pomrb+rain_pomrb,family = gaussian(), mtraining)
p1<-predict(pmodel1,mtesting,type="response")
accuracy(p1,mtesting$POMRB)
p2<-predict(pmodel2,mtesting,type="response")
accuracy(p2,mtesting$PREMON)

pmodel3<-glm(formula=MONSOON~TEH_NAME+year+temp_premon+temp_monsoon+rain_premon+rain_monsoon,family = gaussian(), mtraining)

p3<-predict(pmodel3,mtesting,type="response")

accuracy(p3,mtesting$MONSOON)

pmodel4<-glm(formula=POMKH~TEH_NAME+year+temp_monsoon+temp_pomkh+rain_pomkh+rain_monsoon,family = gaussian(), mtraining)

p4<-predict(pmodel4,mtesting,type="response")

accuracy(p4,mtesting$POMKH)

img <- readJPEG("~/guntur.jpg")
df <- data.frame(mtraining$LAT,mtraining$LON)
ggplot(df, aes(mtraining$LAT,mtraining$LON)) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf,Inf) +
  stat_bin2d() +scale_fill_gradient(low="green",high="red")+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) 

