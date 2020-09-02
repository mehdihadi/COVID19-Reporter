
library("smooth")
library(data.table)
library(dygraphs)
# Forecast Cases cumulative -----


SUM<-aggregate(value~ variable+Date+Country.Region,WorldSum[which(WorldSum$Country.Region=="Iran"), ],FUN = "sum" )%>%
  tidyr::pivot_wider(
    c(Date),
    names_from = variable,
    values_from = value)
DIFF<-aggregate(diff~ variable+Date+Country.Region,WorldDiff[which(WorldDiff$Country.Region=="Iran"), ],FUN = "max" )%>%
  tidyr::pivot_wider(
    c(Date),
    names_from = variable,
    values_from = diff)%>%
  rename("New.Confirmed"=Confirmed, "New.Recoverds"= Recovered, New.Deaths ="Deaths" )

DF<-merge(SUM,DIFF , by=c("Date"))


#################################################### 1
forec_cases_cumsum <- function(data, n_ahead,series) {
  
  series<-series
  new.series<-paste0("New.",series)
  data_ts <- ts(data[data[series] != 0, series])
  
  # If length of ts is too low, then replicate first value
  if (length(data_ts) < 6) {
    
    data_ts <- ts(c(rep(data[data[series] != 0, series][1], 6 - length(data_ts)),
                    data_ts))
    
  }
  
  pred <- es(data_ts,
             model = c("MMN", "MMdN", "MAdN", "AMdN"), # multiple choices of ETS model
             ic = "BICc", # Information Criterion
             h = n_ahead,
             loss = "MSE",
             interval = "parametric",
             level = 0.90,
             silent = "all"
  )
  
  # Handle cumulative ts, have to be increasing all the time
  if (pred$forecast[1] < data_ts[length(data_ts)]) {
    
    print("using only cases!")
    
    data_ts <- ts(data[data[new.series] != 0,  new.series])
    
    
    if (length(data_ts) < 6) {
      
      data_ts <- ts(c(rep(data[data[ new.series] != 0, new.series][1], 6 - length(data_ts)),
                      data_ts))
      
    }
    
    pred <- es(data_ts,
               model = c("MMN", "MMdN", "MAdN", "AMN", "AMdN"), # multiple choices of ETS model
               ic = "BICc",
               h = n_ahead,
               loss = "MSE",
               interval = "parametric",
               level = 0.90,
               silent = "all"
    )
    
    # Heuristic for Mean + PI forecasts
    pred$forecast <- cumsum(pred$forecast) + data[.N, Cases_cumsum]
    pred$upper <- cumsum(pred$forecast) + data[.N, Cases_cumsum]
    pred$upper <- pred$upper + pred$upper*0.1
    
  }
  
  return(pred)
  
}
#################################################1



# Forecasting Cases cumulative world -----
data_cases_cumsum_forec_world <- reactive({
  
  data_res <- copy(DF)
  
  data_forec <- forec_cases_cumsum(data_res, 5,"Deaths")
  
  data_res <- rbindlist(list(
    data_res,
    data.table(Date= seq.Date(max(data_res$Date)+1,max(data_res$Date)+5, by = 1),
               Cases_cumsum_mean = round(data_forec$forecast, digits = 0),
               Cases_cumsum_lwr = floor(data_forec$forecast),
               Cases_cumsum_upr = data_forec$upper
    )
  ), fill = TRUE, use.names = TRUE
  )
  
  data_res[, Model := data_forec$model]
  
  data_res
  
})


# Show forecasted cases of the world ----
output$dygraph_world_cases_forecast <- renderDygraph({
  
  data_res <- copy(data_res)
  
  dygraph(data_res[, .(Date, 'Cases cumulative' = Confirmed,
                       Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
          main = paste0("World",
                        ", model: ",
                        data_res[, unique(Model)])) %>%
    # dyAxis("y", label = "Cases - cumulative") %>%
    dySeries('Cases cumulative') %>%
    dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
             label = "Cases cumulative - forecast") %>%
    dyRangeSelector(dateWindow = c(data_res[, max(Date) - 5 - 5],
                                   data_res[, max(Date) + 1]),
                    fillColor = "#5bc0de", strokeColor = "#222d32") %>%
    dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
              fillGraph = TRUE, fillAlpha = 0.4,
              drawPoints = TRUE, pointSize = 3,
              pointShape = "circle",
              colors = c("#5bc0de", "#228b22")) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
    dyEvent(data_res[is.na(Cases_cumsum_mean), max(Date)],
            "Forecasting origin", labelLoc = "bottom") %>%
    dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  
})





# Forecasting Deaths cumulative for world -----
data_deaths_cumsum_forec_world <- reactive({
  
  data_res <- copy(data_world())
  
  data_forec <- forec_deaths_cumsum(data_res, 10)
  
  data_res <- rbindlist(list(
    data_res,
    data.table(DateRep = seq.Date(data_res[, max(DateRep) + 1],
                                  data_res[, max(DateRep) + 10],
                                  by = 1),
               Deaths_cumsum_mean = round(data_forec$forecast, digits = 0),
               Deaths_cumsum_lwr = floor(data_forec$forecast),
               Deaths_cumsum_upr = data_forec$upper
    )
  ), fill = TRUE, use.names = TRUE
  )
  
  data_res[, Model := data_forec$model]
  
  data_res
  
})

# Show forecasted deaths of the world ----
output$dygraph_world_deaths_forecast <- renderDygraph({
  
  data_res <- copy(data_deaths_cumsum_forec_world())
  
  dygraph(data_res[, .(DateRep, 'Deaths cumulative' = Deaths_cumsum,
                       Deaths_cumsum_mean, Deaths_cumsum_lwr, Deaths_cumsum_upr)],
          main = paste0("World",
                        ", model: ",
                        data_res[, unique(Model)])) %>%
    # dyAxis("y", label = "Deaths - cumulative") %>%
    dySeries('Deaths cumulative') %>%
    dySeries(c("Deaths_cumsum_lwr", "Deaths_cumsum_mean", "Deaths_cumsum_upr"),
             label = "Deaths cumulative - forecast") %>%
    dyRangeSelector(dateWindow = c(data_res[, max(DateRep) - 10 - 10],
                                   data_res[, max(DateRep) + 1]),
                    fillColor = "#5bc0de", strokeColor = "#222d32") %>%
    dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
              fillGraph = TRUE, fillAlpha = 0.4,
              drawPoints = TRUE, pointSize = 3,
              pointShape = "circle",
              colors = c("#5bc0de", "#228b22")) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
    dyEvent(data_res[is.na(Deaths_cumsum_mean), max(DateRep)],
            "Forecasting origin", labelLoc = "bottom") %>%
    dyLegend(width = 300, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
  
})






# Forecast Deaths cumulative -----

forec_deaths_cumsum <- function(data, n_ahead) {
  
  data_ts <- ts(data[Deaths_cumsum != 0, Deaths_cumsum])
  
  if (length(data_ts) < 6) {
    
    data_ts <- ts(c(rep(data[Deaths_cumsum != 0, Deaths_cumsum][1], 6 - length(data_ts)),
                    data_ts))
    
  }
  
  pred <- es(data_ts,
             model = c("MMN", "MMdN"),
             ic = "AICc",
             h = n_ahead,
             loss = "MSE",
             interval = "parametric",
             level = 0.90,
             silent = "all")
  
  return(pred)
  
}

#########################################################################


