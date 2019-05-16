mydata<-read.csv(file = "~/final_csv_khammam (2).csv")
View(mydata)
library(caTools)
library(forecast)
library(AER)
msplit<-sample.split(mydata,SplitRatio = 0.8)
mydata$MONSOON=as.numeric(mydata$MONSOON)
mydata$POMRB=as.numeric(mydata$POMRB)
mydata$POMKH=as.numeric(mydata$POMKH)
mydata$PREMON=as.numeric(mydata$PREMON)
mydata$MONSOON=as.numeric(mydata$MONSOON)

mtraining<-subset(mydata,msplit=="TRUE")
mtesting<-subset(mydata,msplit=="FALSE")

pmodel2<-lm(PREMON~POMRB+year+temp_pomrb+temp_premon, mtraining)
pmodel2

pmodel1<-lm(formula=POMRB~year+temp_pomrb+rain_pomrb, mtraining)
#plot(pmodel)$

p1<-predict(pmodel1,mtesting)
p1
accuracy(p1,mtesting$POMRB)

p2<-predict(pmodel2,mtesting,type="response")

#actuals_preds2 <- data.frame(cbind(actuals=mtesting$PREMON, predicteds=p2))
#correlation_accuracy2 <- cor(actuals_preds2)
#actuals_preds2
#correlation_accuracy2[2][1]
accuracy(p2,mtesting$PREMON)

pmodel3<-glm(formula=MONSOON~year+temp_premon+temp_monsoon+rain_premon+rain_monsoon,family = gaussian(), mtraining)
#plot(pmodel)$


p3<-predict(pmodel3,mtesting,type="response")

accuracy(p3,mtesting$MONSOON)

pmodel4<-glm(formula=POMKH~year+temp_monsoon+temp_pomkh,family = gaussian(), mtraining)
#plot(pmodel)$

p4<-predict(pmodel4,mtesting,type="response")


accuracy(p4,mtesting$POMKH)

img <- readJPEG("~/khammam.jpg")

df <- data.frame(mtraining$LAT,mtraining$LON)
ggplot(df, aes(mtraining$LAT,mtraining$LON)) + 
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf,Inf) +
  stat_bin2d() +scale_fill_gradient(low="green",high="red")+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) 

