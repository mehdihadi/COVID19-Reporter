#https://beta.rstudioconnect.com/content/2671/Combining-Shiny-R-Markdown.html
#How to render formula in rmakdown within shiny:  
#https://community.rstudio.com/t/r-shiny-with-a-markdown-report-in-one-of-the-tabpanel/19869
#https://shiny.rstudio.com/gallery/mathjax.html
#https://shiny.rstudio.com/gallery/including-html-text-and-markdown-files.html
#reprot
#https://shiny.rstudio.com/gallery/download-knitr-reports.html
library(webshot)  # for non-html outputs in report :https://bookdown.org/yihui/bookdown/html-widgets.html
library(knitr)    # For knitting document and include_graphics function
library(ggplot2)  # For plotting
library(png)      # For grabbing the dimensions of png files
library(selectr)
library(tinytex)
#library(shinyjs)
library(shinycssloaders)
#library("highcharter") #TEXT COLORS IN RMD
library(shiny)
#detach("package:flexdashboard", unload=TRUE)
library(shinydashboard)
library(markdown)
library(knitr)
library(plyr)
library(dplyr)
library(DT)
library(echarts4r)
#library(shinyMobile)
library(countup)
library(tidyverse)
library(ggplot2)   # to create static plot
library(gganimate)  # to add animation to static plot 
library("gapminder")
library(ggrepel)
library(tidyr)
library(shinydashboardPlus)
#library(bookdown)
library(kableExtra)
#install.packages("devtools")
#remotes::install_github("JohnCoene/waiter")
library (waiter)
#remotes::install_github("PaulC91/shinyauthr")
#remotes::install_github("jeroen/sodium")
library(shinyauthr)#for login
library(shinyjs)#for login
library(sodium) #for password hash
#install.packages("leaflet")
library(forcats)
library(leaflet)
library(shinythemes)
library(maps)
library(reshape2)
library(readxl)
library(ggridges)
library(ggpubr)
library(GGally)
library(ggplot2)   # to create static plot
library(gganimate)  # to add animation to static plot 
library("gapminder")
library(plotly) 
library(deSolve)
library(leaflet.minicharts)
library(rmarkdown)

#### TES model
library(TSrepr)
library(shinyEffects)
library(shinyTime)
library(dygraphs)
library(dendextend)
library(tableHTML)
library(smooth)
library(dtwclust)
library(data.table)

# email
library(mailR)
library(rJava)

#growth rate   #   http://covid19forecast.science.unimelb.edu.au/#about
library(shiny.i18n)

# ############### Confirmed cases
 confirmed<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
 #replache china with provinces
 confirmed[,1:2] <- data.frame(lapply(confirmed[,1:2], as.character), stringsAsFactors=FALSE)
 DF<-data.frame(table(confirmed$Country.Region))
 DF<-DF[which(DF$Freq>1), ][,1]
 DFCountry<-dput(as.character(DF))
 #confirmed[which(confirmed$Country.Region %in% DFCountry), ][,2]<-confirmed[which(confirmed$Country.Region %in% DFCountry), ][,1]
 confirmed[which(confirmed$Country.Region %in% "Israel"), ][,2]<-"Occupied Palestine"
 confirmed[which(confirmed$Country.Region %in% "Taiwan*"), ][,2]<-"Taiwan"
 
 
 confirmed <- melt(confirmed, id.vars=1:4)
 confirmed$variable <- as.Date(paste0(substring(confirmed$variable, 2),"20"), format = "%m.%d.%Y")
 colnames(confirmed)[5]<-"Date"
 colnames(confirmed)[6]<-"Confirmed"
 #convert 2 column to character
 
 
 library(dplyr)
 
 ############### Confirmed cases
 confirmed<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
 #replache china with provinces
 confirmed[,1:2] <- data.frame(lapply(confirmed[,1:2], as.character), stringsAsFactors=FALSE)
 DF<-data.frame(table(confirmed$Country.Region))
 DF<-DF[which(DF$Freq>1), ][,1]
 DFCountry<-dput(as.character(DF))
 #confirmed[which(confirmed$Country.Region %in% DFCountry), ][,2]<-confirmed[which(confirmed$Country.Region %in% DFCountry), ][,1]
 confirmed[which(confirmed$Country.Region %in% "Israel"), ][,2]<-"Occupied Palestine"
 confirmed[which(confirmed$Country.Region %in% "Taiwan*"), ][,2]<-"Taiwan"
 
 
 confirmed <- melt(confirmed, id.vars=1:4)
 confirmed$variable <- as.Date(paste0(substring(confirmed$variable, 2),"20"), format = "%m.%d.%Y")
 colnames(confirmed)[5]<-"Date"
 colnames(confirmed)[6]<-"Confirmed"
 #convert 2 column to character
 
 
 #OR
 #confirmed[,2]<-with(confirmed,ifelse(Country.Region=="Mainland China",confirmed[,1],confirmed[,2]))
 ############### dead cases
 deaths<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
 #replache china with provinces
 deaths[,1:2] <- data.frame(lapply(deaths[,1:2], as.character), stringsAsFactors=FALSE)
 DF<-data.frame(table(deaths$Country.Region))
 DF<-DF[which(DF$Freq>1), ][,1]
 DFCountry<-dput(as.character(DF))
 #deaths[which(deaths$Country.Region %in% DFCountry), ][,2]<-deaths[which(deaths$Country.Region %in% DFCountry), ][,1]
 deaths[which(deaths$Country.Region %in% "Israel"), ][,2]<-"Occupied Palestine"
 deaths[which(deaths$Country.Region %in% "Taiwan*"), ][,2]<-"Taiwan"
 
 deaths <- melt(deaths, id.vars=1:4)
 deaths$variable <- as.Date(paste0(substring(deaths$variable, 2),"20"), format = "%m.%d.%Y")
 colnames(deaths)[5]<-"Date"
 colnames(deaths)[6]<-"Deaths"
 #convert 2 column to character
 
 ############### recovered cases
 recovered<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
 recovered[,1:2] <- data.frame(lapply(recovered[,1:2], as.character), stringsAsFactors=FALSE)
 DF<-data.frame(table(recovered$Country.Region))
 DF<-DF[which(DF$Freq>1), ][,1]
 DFCountry<-dput(as.character(DF))
 #recovered[which(recovered$Country.Region %in% DFCountry), ][,2]<-recovered[which(recovered$Country.Region %in% DFCountry), ][,1]
 recovered[which(recovered$Country.Region %in% "Israel"), ][,2]<-"Occupied Palestine"
 recovered[which(recovered$Country.Region %in% "Taiwan*"), ][,2]<-"Taiwan"
 
 recovered <- melt(recovered, id.vars=1:4)
 recovered$variable <- as.Date(paste0(substring(recovered$variable, 2),"20"), format = "%m.%d.%Y")
 colnames(recovered)[5]<-"Date"
 colnames(recovered)[6]<-"Recovered"
 #convert 2 column to character
 #########################
# 
# 
# 
 WorldData<-merge(confirmed,recovered)
 WorldDataMg<-merge(WorldData,deaths)
 WorldDataMg<-WorldDataMg[,c("Country.Region", "Lat","Long","Date","Province.State","Confirmed","Recovered","Deaths")]

# ####### For some countries recovered and deaths misplaced it corrected here
 WorldDataMg<-WorldDataMg %>% 
 mutate(Deaths1 = ifelse(Recovered < Deaths, Recovered, Deaths), 
       Recovered = ifelse(Recovered < Deaths, Deaths, Recovered)) %>% 
 select(Country.Region,Lat,Long,Date,Province.State,Confirmed,Recovered, Deaths = Deaths1)


 saveRDS(WorldDataMg, "DATA.rds")



#save(Populations, file = "Populations.RData")
load("Populations.RData") # loading population

WorldData <- melt(WorldDataMg, id.vars=1:5)
WorldData$value<-as.numeric(as.character(WorldData$value))
WorldData$diff <- ave(WorldData$value, factor(c(WorldData$Country.Region)), FUN=function(x) as.numeric(as.character(c( NA ,diff(x)))))
WorldData[is.na(WorldData$diff)|WorldData$diff<0, ][,8]<-WorldData[is.na(WorldData$diff)|WorldData$diff<0, ][,7]# replace NA with firs value for series 

WorldMean<-aggregate(value~ variable+Date,WorldData,FUN = "mean" )
WorldMin<-aggregate(value~ variable+Date,WorldData,FUN = "min" )
WorldMax<-aggregate(value~ variable+Date,WorldData,FUN = "max" )
WorldSum<-aggregate(value~ variable+Date+Country.Region+Province.State+Lat+Long,WorldData,FUN = "sum" )
WorldDiff<-aggregate(diff~ variable+Date+ Country.Region+Province.State+Lat+Long,WorldData,FUN = "max" )
WorldDiffMg<-dcast(WorldDiff, Country.Region +Lat+Long +Date+Province.State ~ variable, value.var = "diff")

WorldSumMax<-aggregate(value~ variable+Country.Region+Province.State ,WorldSum,FUN = "max" )
 
WorldSum_F<- aggregate(value~ variable+Date+Country.Region,WorldSum,FUN = "sum" ) %>%
  tidyr::pivot_wider(
    c(Date,Country.Region),
    names_from = variable,
    values_from = value
  )

WorldDiff_F<- aggregate(diff~ variable+Date+Country.Region,WorldDiff,FUN = "sum" ) %>%
  tidyr::pivot_wider(
    c(Date,Country.Region),
    names_from = variable,
    values_from = diff
  )

WorldSumDiff<-merge(WorldSum_F,WorldDiff_F %>%
                      dplyr::arrange(desc(Date), desc(Confirmed))%>%
                      dplyr::rename(New.Confirmed = Confirmed,New.Recoverds = Recovered,New.Deaths = Deaths) ,
                    by=c("Country.Region","Date")) 


WorldSumDiffS<-dplyr::filter(WorldSumDiff , Date == max(WorldSumDiff$Date) ) %>% 
  dplyr::arrange(desc(Confirmed))

WorldSum_FP<- aggregate(value~ variable+Date+Country.Region+Province.State,WorldSum,FUN = "sum" ) %>%
  tidyr::pivot_wider(
    c(Date,Country.Region,Province.State),
    names_from = variable,
    values_from = value
  )

WorldDiff_FP<- aggregate(diff~ variable+Date+Country.Region+Province.State,WorldDiff,FUN = "sum" ) %>%
  tidyr::pivot_wider(
    c(Date,Country.Region,Province.State),
    names_from = variable,
    values_from = diff
  )

WorldSumDiff_FP<-merge(WorldSum_FP,WorldDiff_FP %>%
                      dplyr::arrange(desc(Date), desc(Confirmed))%>%
                      dplyr::rename(New.Confirmed = Confirmed,New.Recoverds = Recovered,New.Deaths = Deaths) ,
                    by=c("Country.Region","Province.State","Date"))

WorldSumDiff_FPS<-dplyr::filter(WorldSumDiff_FP , Date == max(WorldSumDiff$Date) ) %>% 
  dplyr::arrange(desc(Confirmed))


############## Global aggregated dataset
#### World
WorldSumAgr<-aggregate(value~ variable+Date,WorldSum,FUN = "sum" )%>%
  tidyr::pivot_wider(
    c(Date),
    names_from = variable,
    values_from = value)

WorldDiffAgr<-aggregate(diff~ variable+Date,WorldDiff,FUN = "max" )%>%
  tidyr::pivot_wider(
    c(Date),
    names_from = variable,
    values_from = diff)%>%
  rename("New.Confirmed"=Confirmed, "New.Recoverds"= Recovered, New.Deaths ="Deaths" )
WorldSumDiffAgr<-merge(WorldSumAgr,WorldDiffAgr , by=c("Date"))



################Pass base
# dataframe that holds usernames, passwords and other user data
user_base <- data.frame(
  user = c("user1", "user2"),
  password = c("pass1", "pass2"), 
  permissions = c("admin", "standard"),
  name = c("User One", "User Two"),
  stringsAsFactors = FALSE
)
################Pass base
library(sodium)

#LOAD USER BASE DATA
user_base <- readRDS("user_base.rds")

#################user online initial value
users<-reactiveValues(count = 0) 

################# Visits
# visits<-data.frame(Tvisit = 0,visit=0,Date=as.Date(as.POSIXlt(Sys.time()))) #When initial load on server run these two lines
# saveRDS(visits, "visits.rds")
visits <- readRDS("visits.rds")


############ Empty dataframe for comment gathering
#ALLCOMMENTS <- data.frame(times=character(), Name=character(),Comment=character())
#saveRDS(ALLCOMMENTS, file = 'ALLCOMMENTS.RDS')
ALLCOMMENTS <- readRDS("ALLCOMMENTS.rds")
##############To set directory for rmakdown child file
dir<-getwd()

########## growth rate

i18n <- Translator$new(translation_json_path = "translations/translations.json")

growthRate <- function(cases, inWindow=10){
  nn <- length(cases)
  ss <- (nn - inWindow + 1):nn
  rate <- numeric(length(ss))
  rate[ss] <- 100 * (cases[ss] - cases[ss-1]) / cases[ss-1]
}

# calculates the curve flatenning index.
# it is the second derivative of logA wrt t (the change in growth rate) divided by first differential (the current growth rate).
cfi <- function(active){
  lnact <-log(active)
  cfiInd <- -diff(diff(lnact))/abs(diff(lnact)[-1])
  cfiInd[abs(cfiInd)>10]<-NA # remove crazy values associated with changed test/diagnosis
  cfiInd
}


