library(shiny)
library(shiny.i18n)
library(plotly)




DF<-WorldSumDiff_FP[which(WorldSumDiff_FP$Country.Region=="Iran"), ]
DF<-DF%>%
  rename("Daily confirmed"= New.Confirmed, "Daily recovereds"= New.Recoverds, "Daily deaths"= New.Deaths)%>%
mutate(Active = Confirmed-Recovered-Deaths ) %>%
  rename(Country=Country.Region)



 
DF<-subset(timeSeriesActive, timeSeriesActive$Region %in% "Iran")
 
i18n <- Translator$new(translation_json_path = "translations/translations.json")

function(cases, inWindow=10){
  nn <- length(cases)
  ss <- (nn - inWindow + 1):nn
  rate <- numeric(length(ss))
  rate[ss] <- 100 * (cases[ss] - cases[ss-1]) / cases[ss-1]
}

#######################################
pDat <- DF[,c("Country","Date","Active")]

pDat <- DF
pMat<-as.matrix(log(pDat[,c("Active")]))
colnames(pMat)<-pDat[1,1]
cfiDat<-apply(pMat, MARGIN = 2, FUN = "cfi")
cfiDat[!is.finite(cfiDat)]<-0
dateSub<-3:length(pDat$Date) # date subset
for (cc in 1:ncol(cfiDat)){
  cfiSmooth<-loess(cfiDat[,1]~as.numeric(pDat$Date[dateSub]))
  cfiDat[,cc] <- predict(cfiSmooth, newdata = as.numeric(pDat$Date[dateSub]))
}
yRange <- as.list(range(cfiDat)*1.05)
cfiDat <- data.frame(dates = pDat$Date[dateSub], cfiDat)
fig <- plot_ly(type = "scatter", mode = "none", name = "")
for (cc in 2:ncol(cfiDat)){
  fig <- fig %>% add_trace(y = cfiDat[,cc],
                           x = dates[dateSub],
                           mode = "lines",
                           name = colnames(cfiDat)[cc],
                           hoverinfo = "text+name", 
                           text = paste(format(cfiDat$dates, "%b %d"), round(cfiDat[,cc], 2)))
}
fig <- fig %>% layout(xaxis = list(title = list(text = i18n$t("Date"))),
                      yaxis = list(title = list(text = i18n$t("Curve-flattening index")),
                                   range = yRange)
) %>%
  config(displayModeBar = FALSE)
##########################################

DF<- data.frame(WorldSumAgr)%>%
  mutate(Active = Confirmed-Recovered-Deaths ) %>%
  mutate(Country = "World" ) 


pDat <- DF
gRate <- as.matrix(growthRate(pDat[,c("Active")], inWindow = nrow(pDat)-1)) #daily growth over all time
gRateMA <- apply(gRate, # get moving average (three day window)
                 MARGIN = 2, 
                 FUN = function(x, n = 3){stats::filter(x, rep(1 / n, n), sides = 1)})
gRateMA[is.infinite(gRateMA)]<-NA #remove Infs
# reorganise for plotting
gRateMA <- data.frame(dates = as.Date(pDat$Date, format = "%m.%d.%y")[-1], gRateMA)
if (nrow(gRateMA)>20) gRateMA <-tail(gRateMA, 50)
colnames(gRateMA)[-1] <- pDat[1,"Country"]
# and plot
fig <- plot_ly(gRateMA, type = "scatter", mode = "none")
for (cc in 2:ncol(gRateMA)){
  fig <- fig %>% add_trace(y = gRateMA[,cc],
                           x = ~dates,
                           mode = "lines", 
                           name = colnames(gRateMA)[cc],
                           hoverinfo = "text+name", 
                           text = paste(format(gRateMA$dates, "%b %d"), round(gRateMA[,cc], 1), "%"))
}
fig <- fig %>% layout(xaxis = list(title = list(text = i18n$t("Date"))),
                      yaxis = list(title = list(text = i18n$t("Growth rate (% per day)")))
) %>%
  config(displayModeBar = FALSE)%>%
layout(plot_bgcolor='yellow')%>%
layout(paper_bgcolor='yellow')








