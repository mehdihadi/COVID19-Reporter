

server <- function(input, output, session) {
  
  ##########################################Pass server
  
  # call the logout module with reactive trigger to hide/show
#off#  logout_init <- callModule(shinyauthr::logout, 
#off#                            id = "logout", 
#off#                            active = reactive(credentials()$user_auth))
  # call login module supplying data frame, user and password cols
  # and reactive trigger
#off#  credentials <- callModule(shinyauthr::login, 
#off#                            id = "login", 
#off#                            data = user_base,
#off#                            user_col = user,
#off#                            pwd_col = password,
#off#                            log_out = reactive(logout_init()))
 
#off#   user_info <- reactive({credentials()$info})
  
  ##########################################Pass server  
  # give time for wait screen to show
  #---------1
  # Sys.sleep(3) 
  # waiter_hide()
  # 
  # observeEvent(input$show, {
  #   show_waiter(spinner)
  #   Sys.sleep(3) # give time for wait screen to show
  #   waiter_hide()
  # })
  # 
  
  #-------- waiter2
  # Sys.sleep(2)
  # update_waiter(html = tagList(spin_orbiter(),span("Grabbing a cup of coffee...", style="color:white;")))
  # 
  #------ waiter3
  # call the waitress
  # waitress <- Waitress$
  #   new(theme = "overlay-percent")$
  #   start() # start
  # 
  # for(i in 1:10){
  #   waitress$inc(10) # increase by 10%
  #   Sys.sleep(.3)
  # }
  # # hide when it's done
  # waitress$close() 
  #--------waiter4
  #   Sys.sleep(3) # do something that takes time
  #   waiter_hide()

  ################################ Hide modeling tab   
     
#off#   hideTab(inputId = "TABS", target = "Modeling")   
  
   ################################ Hide modeling tab  
  
   ####################### count connected users: some part of code in Global
   onSessionStart = isolate({
     users$count <<- users$count + 1
     visit<-data.frame(Tvisit = last(visits$Tvisit)+1,visit=1,Date=as.Date(as.POSIXlt(Sys.time())))
     visits <<- rbind(visits,visit)
     saveRDS(visits, "visits.rds")
   })
   

   onSessionEnded(function() {
     isolate({
       users$count <<-users$count - 1
       
     })
   })
   
   ############################
   
   
   
    
  output$Version <- renderUI({  
    markdown::markdownToHTML(knit( 'Update.Rmd', quiet = TRUE),fragment.only=TRUE) 
    withMathJax(includeMarkdown( 'Update.md' ))
  })
  #we can't (shouldn't) have two elements with the same ID in an HTML document (whether using Shiny or not). thus:

   #https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html
 
  # output$Introduction <- renderUI({
  #   tags$iframe(src='Introduction.html',width="100%",frameBorder="0",height="1000px")
  # })
  #  output$ESIR <- renderUI({
  #   tags$iframe(src='ESIR.html',width="100%",frameBorder="0",height="500px")
  #  })
   
   # getPage<-function() {
   #   return(includeHTML("www//ESIR.html"))
   # }
   # output$ESIR<-renderUI({getPage()})  #it can be used with htmloutput()

   
   
   
  #trend tab
   observe({
     
     updateSelectInput(session, 
                      "province",
                      choices = dput(as.character(as.character(c("Select all",unique(WorldData[WorldData$Country.Region %in%
                                                                                                 input$country, "Province.State"]))))),
                      selected = "Select all"
                      )
  })
  
  #SERI tab
  observe({
    
       updateSelectInput(session, 
                      "province1",
                      choices = dput(as.character(as.character(c("Select all",unique(WorldData[WorldData$Country.Region %in%
                                                                                                 input$country1, "Province.State"]))))),
                      selected = "Select all"
    )
  })
  
  
############ 
      # observeEvent(input$country1, {    updateTextInput(session,
      #                                                   "N_value",
      #                                                  "Population(N):",unique(Populations[Populations$Country %in%
      #                                                                                         input$country1, "Pop2020"]*1000)
      # )
      # })
################   
  
  
 
  
   CountSum <- reactive({
      if(input$country=="Select all" & input$province=="Select all"){
        aggregate(value~ variable,WorldSumMax,FUN = "sum" ) 
      }else if (input$country!="Select all" & input$province=="Select all") {
        DF<-WorldSumMax[which(WorldSumMax$Country.Region==input$country), ] 
        aggregate(value~ variable,DF,FUN = "sum" ) 
      }else{
        DF<-WorldSumMax[which(WorldSumMax$Country.Region==input$country & WorldSumMax$Province.State==input$province), ] 
        aggregate(value~ variable,DF,FUN = "sum" ) 
      }
    }) 
    
   
    output$DataText <- renderUI({
            markdown::markdownToHTML(knit('Table.Rmd', quiet = TRUE), fragment.only=TRUE)
            withMathJax(includeMarkdown('Table.md'))
        # 
    })
    
  
    
    
    
    
    output$RawText <- renderUI({
            markdown::markdownToHTML(knit( 'Raw.Rmd' , quiet = TRUE),fragment.only=TRUE) 
            withMathJax(includeMarkdown( 'Raw.md' ))
    })
    
    
    
    LatestDataTable<-reactive(
      WorldSumDiffS %>%
        rename( "Country"= Country.Region,"New cases"= New.Confirmed, "New recovereds"= New.Recoverds, "New deaths"= New.Deaths)
    )
    
   
    # https://bookdown.org/yihui/rmarkdown-cookbook/child-document.html 
    CountryDataTable<-reactive({ 
      
        if (input$country!="Select all" & input$province=="Select all") {
          DF<-WorldSumDiff_FP[which(WorldSumDiff_FP$Country.Region==input$country), ]
          DF%>%
            rename("Daily confirmed"= New.Confirmed, "Daily recovereds"= New.Recoverds, "Daily deaths"= New.Deaths)
          
        }else{
          DF<-WorldSumDiff_FP[which(WorldSumDiff_FP$Country.Region==input$country & WorldSumDiff_FP$Province.State==input$province), ]
          DF%>%
            rename("Daily confirmed"= New.Confirmed, "Daily recovereds"= New.Recoverds, "Daily deaths"= New.Deaths)
          
        }
      }) 
      
    output$datatable = renderDataTable({
      #req(!is.null(results$dataframeTotal))
      x = LatestDataTable() %>%
        mutate(Active = Confirmed-Recovered-Deaths ) %>%
        mutate(totalActivePer = (Active)/Confirmed) %>%
        mutate(totalRecoveredPer = Recovered/Confirmed) %>%
        mutate(totalDeathPer = Deaths/Confirmed) %>%
        select(Country = Country, Confirmed = Confirmed, Recovered=Recovered,Deaths=Deaths, Active = Active,"New cases" ,"New recovereds", "New deaths","Active (%)" = totalActivePer,"Recovered (%)" = totalRecoveredPer,"Deaths (%)" = totalDeathPer)
     
      datatable(x,
               # extensions = 'Buttons',
                rownames = FALSE,
                filter = 'top',
                options = list(
                  searchHighlight = TRUE,
                  pageLength = 30,
                  scrollX = TRUE,
                  dom = 'Bfrtip'
                  # ,
                  # buttons =
                  #   list(
                  #     list(
                  #       extend = 'collection',
                  #       buttons = c('csv', 'pdf'),
                  #       text = 'Download'
                  #     )
                  #   )
                  
                )
                
      ) %>%
        formatPercentage('Active (%)',2) %>%
        formatPercentage('Recovered (%)',2) %>%
        formatPercentage('Deaths (%)',2) %>%
        formatStyle(
          'Active (%)',
          background = styleColorBar(x$'Active (%)', '#31bed4'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Recovered (%)',
          background = styleColorBar(x$'Recovered (%)', '#8bd431'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'Deaths (%)',
          background = styleColorBar(x$'Deaths (%)', '#ff5757'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
    
##### Growth- rate #####   #   http://covid19forecast.science.unimelb.edu.au/#about
    output$growthRate <- renderPlotly({
      
      if (input$country=="Select all" & input$province=="Select all") {
        pDat <- data.frame(WorldSumAgr)%>%
          mutate(Active = Confirmed-Recovered-Deaths ) %>%
          mutate(Country = "World" ) 
           
      }else{
        pDat <- CountryDataTable()%>%
          mutate(Active = Confirmed-Recovered-Deaths ) %>%
          rename(Country=Country.Region)
        }
      
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
        layout(paper_bgcolor='yellow')%>%
        layout( xaxis = list(
          showline = TRUE,
          mirror = "ticks",
          linecolor = toRGB("black"),
          linewidth = 1
        ), yaxis = list(
          showline = TRUE,
          mirror = "ticks",
          linecolor = toRGB("black"),
          linewidth = 1
        ))
      
      
     
    })
    
 ##### Curve-flattening #####    
    output$cfi <- renderPlotly({
     
      if (input$country=="Select all" & input$province=="Select all") {
        pDat <- data.frame(WorldSumAgr)%>%
          mutate(Active = Confirmed-Recovered-Deaths ) %>%
          mutate(Country = "World" ) 
        
      }else{
      
       pDat <- CountryDataTable()%>%
        mutate(Active = Confirmed-Recovered-Deaths ) %>%
        rename(Country=Country.Region)
      }
      
      pMat<-as.matrix(log(pDat[,c("Active")]))
      colnames(pMat)<-pDat[1,"Country"]
      cfiDat<-apply(pMat, MARGIN = 2, FUN = "cfi")
      cfiDat[!is.finite(cfiDat)]<-0
      dateSub<-3:length(pDat$Date) # date subset
      for (cc in 1:ncol(cfiDat)){
        cfiSmooth<-loess(cfiDat[,1]~as.numeric(pDat$Date[dateSub]))
        cfiDat[,cc] <- predict(cfiSmooth, newdata = as.numeric(pDat$Date[dateSub]))
      }
      yRange <- as.list(range(cfiDat)*1.05)
      cfiDat <- data.frame(dates = pDat$Date[dateSub], cfiDat)
      fig <- plot_ly(cfiDat,type = "scatter", mode = "none", name = "")
      
      for (cc in 2:ncol(cfiDat)){
        fig <- fig %>% add_trace(y = cfiDat[,cc],
                                 x = ~dates,
                                 mode = "lines",
                                 name = colnames(cfiDat)[cc],
                                 hoverinfo = "text+name", 
                                 text = paste(format(cfiDat$dates, "%b %d"), round(cfiDat[,cc], 2)))
      }
      fig <- fig %>% layout(xaxis = list(title = list(text = i18n$t("Date"))),
                            yaxis = list(title = list(text = i18n$t("Curve-flattening index")),
                                         range = yRange)
      ) %>%
        config(displayModeBar = FALSE)%>%
        layout(plot_bgcolor='yellow')%>%
        layout(paper_bgcolor='yellow')%>%
        layout( xaxis = list(
          showline = TRUE,
          mirror = "ticks",
          linecolor = toRGB("black"),
          linewidth = 1
        ), yaxis = list(
          showline = TRUE,
          mirror = "ticks",
          linecolor = toRGB("black"),
          linewidth = 1
        ))
      
    
      
      
    })
    
    
    # output$datatable <- DT::renderDataTable( 
    #   
    #    
    #    DT::datatable(LatestDataTable(),  
    #              extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
    #              options = list(
    #                #  dom = 't',
    #                # deferRender = TRUE,
    #                searching = TRUE,
    #                #autoWidth = F,
    #                # scrollCollapse = TRUE,
    #                rownames = FALSE,
    #                scroller = TRUE,
    #                scrollX = TRUE,
    #                scrollY = "500px",
    #                #fixedHeader = TRUE,
    #                class = 'row-border' 
    #                # fixedColumns = list(
    #                #   leftColumns = 3,
    #                #   heightMatch = 'none'
    #                # )
    #              )
    #    )
    #    
    #    
    #   )
     
     
     output$downloadCumtable <- downloadHandler(
       
       filename = function() {
         paste('Cumulative data', '.csv', sep='')
       },
       content = function(file) {
         write.csv(LatestDataTable(), file)
       }
     ) 
     
     RawDataTable<-reactive(
       WorldDataMg %>%
         rename( "Country"= Country.Region,"Province"= Province.State)
     )
     
     
     output$rawdata <- DT::renderDataTable( 
       
       
       DT::datatable(RawDataTable(),  
                     extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                     options = list(
                       #  dom = 't',
                       # deferRender = TRUE,
                       searching = TRUE,
                       #autoWidth = F,
                       # scrollCollapse = TRUE,
                       rownames = FALSE,
                       scroller = TRUE,
                       scrollX = TRUE,
                       scrollY = "500px",
                       #fixedHeader = TRUE,
                       class = 'row-border' 
                       # fixedColumns = list(
                       #   leftColumns = 3,
                       #   heightMatch = 'none'
                       # )
                     )
       )
       
       
     )
     
     output$downloadRaw <- downloadHandler(
       
       filename = function() {
         paste('Raw data', '.csv', sep='')
       },
       content = function(file) {
         write.csv(RawDataTable(), file)
       }
     ) 
     
     
     # withSpinner(DT::dataTableOutput("rawdata"),type=1),
     # downloadButton("downloadRaw", "Download the table")
     
     
     
    
     output$Con <- renderCountup({
       countup(CountSum()[1,2])  
     })
     
     output$Rec <- renderCountup({
       countup(CountSum()[2,2]) 
     })
     
     output$Dea <- renderCountup({
       countup(CountSum()[3,2])
     })
     
   #################################  slope 5
    output$ConS5 <- renderCountup({
      countup( round(Slopediff()[1,1],0))  # confirmed
    })
    
    output$RecS5 <- renderCountup({ # Recovered
      countup(round(Slopediff()[1,2],0))
    })
    
    output$DeaS5 <- renderCountup({ # Deaths
      countup(round(Slopediff()[1,3],0))
    })
    #################################  slope 10
    output$ConS10 <- renderCountup({
      countup( round(Slopediff()[2,1],0))  # confirmed
    })
    
    output$RecS10 <- renderCountup({ # Recovered
      countup(round(Slopediff()[2,2],0))
    })
    
    output$DeaS10 <- renderCountup({ # Deaths
      countup(round(Slopediff()[2,3],0))
    })
    
    #################################  slope 20
    output$ConS20 <- renderCountup({
      countup( round(Slopediff()[3,1],0))  # confirmed
    })
    
    output$RecS20 <- renderCountup({ # Recovered
      countup(round(Slopediff()[3,2],0))
    })
    
    output$DeaS20 <- renderCountup({ # Deaths
      countup(round(Slopediff()[3,3],0))
    })
 
    PlotCum <- reactive({
      if(input$country=="Select all" & input$province=="Select all"){
        aggregate(value~ variable+Date,WorldSum,FUN = "sum" )
      }else if (input$country!="Select all" & input$province=="Select all") {
        DF<-WorldSum[which(WorldSum$Country.Region==input$country), ] 
        aggregate(value~ variable+Date+Country.Region,DF,FUN = "sum" ) 
      }else{
        DF<-WorldSum[which(WorldSum$Country.Region==input$country & WorldSum$Province.State==input$province), ] 
        aggregate(value~ variable+Date+Province.State,DF,FUN = "sum" ) 
        
      }
    }) 
    
    
    

    Plotdiff <- reactive({
      if(input$country=="Select all" & input$province=="Select all"){
        aggregate(diff~ variable+Date,WorldDiff,FUN = "max" )
      }else if (input$country!="Select all" & input$province=="Select all") {
        DF<-WorldDiff[which(WorldDiff$Country.Region==input$country), ] 
        aggregate(diff~ variable+Date+Country.Region,DF,FUN = "max" ) 
      }else{
        DF<-WorldDiff[which(WorldDiff$Country.Region==input$country & WorldDiff$Province.State==input$province), ] 
        aggregate(diff~ variable+Date+Province.State,DF,FUN = "max" ) 
        
      }
    }) 
    
    Slopediff <- reactive({
      if(input$country=="Select all" & input$province=="Select all"){
        DF<-aggregate(diff~ variable+Date,WorldDiff,FUN = "max" )%>%
          tidyr::pivot_wider(c(Date),
                             names_from = variable,
                             values_from = diff)
        DF5<-tail(DF,5)
        DF10<-tail(DF,10)
        DF20<-tail(DF,20)
        ConfS5<-lm( Confirmed~ Date,DF5)
        RecS5<-lm( Recovered~ Date,DF5)
        DeathS5<-lm( Deaths~ Date,DF5)
        ConfS10<-lm( Confirmed~ Date,DF10)
        RecS10<-lm( Recovered~ Date,DF10)
        DeathS10<-lm( Deaths~ Date,DF10)
        ConfS20<-lm( Confirmed~ Date,DF20)
        RecS20<-lm( Recovered~ Date,DF20)
        DeathS20<-lm( Deaths~ Date,DF20)
        
        d5<-data.frame(C=coef(ConfS5)[[2]],R=coef(RecS5)[[2]],D=coef(DeathS5)[[2]])
        d10<-data.frame(C=coef(ConfS10)[[2]],R=coef(RecS10)[[2]],D=coef(DeathS10)[[2]])
        d20<-data.frame(C=coef(ConfS20)[[2]],R=coef(RecS20)[[2]],D=coef(DeathS20)[[2]])
        rbind(d5,d10,d20)
        
        
      }else if (input$country!="Select all" & input$province=="Select all") {
        DF<-WorldDiff[which(WorldDiff$Country.Region==input$country), ] 
        DF<-aggregate(diff~ variable+Date+Country.Region,DF,FUN = "max" )%>%
          tidyr::pivot_wider(c(Date),
                             names_from = variable,
                             values_from = diff)
        DF5<-tail(DF,5)
        DF10<-tail(DF,10)
        DF20<-tail(DF,20)
        ConfS5<-lm( Confirmed~ Date,DF5)
        RecS5<-lm( Recovered~ Date,DF5)
        DeathS5<-lm( Deaths~ Date,DF5)
        ConfS10<-lm( Confirmed~ Date,DF10)
        RecS10<-lm( Recovered~ Date,DF10)
        DeathS10<-lm( Deaths~ Date,DF10)
        ConfS20<-lm( Confirmed~ Date,DF20)
        RecS20<-lm( Recovered~ Date,DF20)
        DeathS20<-lm( Deaths~ Date,DF20)
        
        d5<-data.frame(C=coef(ConfS5)[[2]],R=coef(RecS5)[[2]],D=coef(DeathS5)[[2]])
        d10<-data.frame(C=coef(ConfS10)[[2]],R=coef(RecS10)[[2]],D=coef(DeathS10)[[2]])
        d20<-data.frame(C=coef(ConfS20)[[2]],R=coef(RecS20)[[2]],D=coef(DeathS20)[[2]])
        rbind(d5,d10,d20)
        
      }else{
        DF<-WorldDiff[which(WorldDiff$Country.Region==input$country & WorldDiff$Province.State==input$province), ] 
        DF<-aggregate(diff~ variable+Date+Province.State,DF,FUN = "max" )%>%
          tidyr::pivot_wider(c(Date),
                             names_from = variable,
                             values_from = diff)
        DF5<-tail(DF,5)
        DF10<-tail(DF,10)
        DF20<-tail(DF,20)
        ConfS5<-lm( Confirmed~ Date,DF5)
        RecS5<-lm( Recovered~ Date,DF5)
        DeathS5<-lm( Deaths~ Date,DF5)
        ConfS10<-lm( Confirmed~ Date,DF10)
        RecS10<-lm( Recovered~ Date,DF10)
        DeathS10<-lm( Deaths~ Date,DF10)
        ConfS20<-lm( Confirmed~ Date,DF20)
        RecS20<-lm( Recovered~ Date,DF20)
        DeathS20<-lm( Deaths~ Date,DF20)
        
        d5<-data.frame(C=coef(ConfS5)[[2]],R=coef(RecS5)[[2]],D=coef(DeathS5)[[2]])
        d10<-data.frame(C=coef(ConfS10)[[2]],R=coef(RecS10)[[2]],D=coef(DeathS10)[[2]])
        d20<-data.frame(C=coef(ConfS20)[[2]],R=coef(RecS20)[[2]],D=coef(DeathS20)[[2]])
        rbind(d5,d10,d20)
        
      }
    }) 
    
    
    
    
    
    output$downloadCountrytable <- downloadHandler(
      
      filename = function() {
        paste('Country data', '.csv', sep='')
      },
      content = function(file) {
        
        
        if (input$Modification=="Cumulative cases"){ 
          write.csv(tidyr::pivot_wider(PlotCum(),
                                       c(Date),
                                       names_from = variable,
                                       values_from = value), file)
        }else{
          write.csv(tidyr::pivot_wider(Plotdiff(),
                                       c(Date),
                                       names_from = variable,
                                       values_from = diff), file) 
        }
      }
    ) 
    
    
    
    
    
    
    
    
    
    
    SEIRCum <- reactive({
      if(input$country1=="Select all" & input$province1=="Select all"){
        DF<-aggregate(value~ variable+Date,WorldSum,FUN = "sum" )
         dcast(DF,Date ~ variable, value.var = "value")
      }else if (input$country1!="Select all" & input$province1=="Select all") {
        DF<-WorldSum[which(WorldSum$Country.Region==input$country1), ] 
        DF<-aggregate(value~ variable+Date+Country.Region,DF,FUN = "sum" )
        dcast(DF,Date ~ variable, value.var = "value") 
      }else{
        DF<-WorldSum[which(WorldSum$Country.Region==input$country1 & WorldSum$Province.State==input$province1), ] 
        DF<-aggregate(value~ variable+Date+Province.State,DF,FUN = "sum" ) 
        dcast(DF,Date ~ variable, value.var = "value")
        
        
      }
    })
    
SEIRDiff<- reactive({
      
      if(input$country1=="Select all" & input$province1=="Select all"){
        DF<-aggregate(diff~ variable+Date,WorldDiff,FUN = "max" )
        dcast(DF,Date ~ variable, value.var = "diff")
      }else if (input$country1!="Select all" & input$province1=="Select all") {
        DF<-WorldDiff[which(WorldDiff$Country.Region==input$country1), ] 
        DF<-aggregate(diff~ variable+Date+Country.Region,DF,FUN = "max" ) 
        dcast(DF,Date ~ variable, value.var = "diff") 
      }else{
        DF<-WorldDiff[which(WorldDiff$Country.Region==input$country1 & WorldDiff$Province.State==input$province1), ] 
        DF<-aggregate(diff~ variable+Date+Province.State,DF,FUN = "max" )
        dcast(DF,Date ~ variable, value.var = "diff")
        
      }
    }) 
    
    
    SEIRCalc <-  reactive({
      
      #https://blog.ephorie.de/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov
      # https://triplebyte.com/blog/modeling-infectious-diseases#fn1
      #file:///L:/Airquality%20shiny/20-253807.pdf
      
      #https://wiki.eclipse.org/Epidemiological_Parameters 
      #For world
      #https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology#The_SEIR_model
      
      #https://stats.stackexchange.com/questions/446712/fitting-sir-model-with-2019-ncov-data-doesnt-conververge
      #https://www.lewuathe.com/covid-19-dynamics-with-sir-model.html
      #https://triplebyte.com/blog/modeling-infectious-diseases
      #https://towardsdatascience.com/social-distancing-to-slow-the-coronavirus-768292f04296
        DF<- SEIRCum() # or SEIRDiff()
        Infected <- DF[,2]
        DATE <-  DF[,1]
        N <- as.numeric(input$N_value) 
        N.<<-N
        C <- as.numeric(input$C_value)/100  # C is percent of Remaining social contact 
        F_Value<-as.numeric(input$F_value)/100  # F is percent of population involvolved to remove social contact 
        
        # old <- par(mfrow = c(1, 2))
        # plot(DATE, Infected, type ="b")
        # plot(DATE, Infected, log = "y")
        # abline(lm(log10(Infected) ~ DATE))
        # title("Confirmed Cases 2019-nCoV world", outer = TRUE, line = -2)
        
        SEIR <- function(time, state, parameters) {
          par <- as.list(c(state, parameters))
          with(par, {
            dS <-  -beta*(1-(1-C^2)*F_Value)/N * I * S 
            dE<- (beta*(1-(1-C^2)*F_Value)/N * I * S)-siggma*E
            dI <- siggma*E- gamma * I
            dR <- gamma * I
             N<-S+E+I+R
            list(c(dS,dE, dI, dR))
          })
        }
        
        
        init <- c(S = input$S_value*N , E=5.1*10^-6*N,I = 2.5*10^-7*N, R = 0)#world
        #init <- c(S = 0.9*N , E=20,I = 2, R = 0)#Iran
        
        RSS <- function(parameters) {
          names(parameters) <- c("beta", "gamma","siggma","C","F_Value")
          
          total_days<- difftime(DATE,DF[min(which(DF$Confirmed> 0)),1 ] ,units="days")
          out <- ode(y = init, times = as.vector(total_days), func = SEIR, parms = parameters)
          fit <- out[ , 4]
          sum((Infected - fit)^2)
        }
        
        # initPar <- c(0.5, 0.05,0.2)
        initPar <- c(input$beta_value,input$gamma_value,input$sigma_value,0,0)
        # parameters<-c(0.5, 0.05,0.2)
        #lower<-c(0,0,0)
        #upper<-c(Inf,Inf,Inf)
      
        Opt <<- reactive(optim(initPar, RSS, method = "Nelder-Mead" )) # optimize with some sensible conditions
        
        #out <- reactive(optim(c(input$range[1], input$range[2]), fr))
        # output$optim_out <- renderPrint(out())
        output$beta  <- renderText( Opt()$par[1]*(1-(1-C^2)*F_Value) )
        output$sigma  <- renderText( Opt()$par[3] )
        output$gamma  <- renderText( Opt()$par[2] )
        R_zero<<-Opt()$par[1]/Opt()$par[2]  #use << to be available in global environment ans pass to RMD
        output$R_0  <- renderText(R_zero) 
        output$R_t  <- renderText((1-(1-C^2)*F_Value)*R_zero) # Return Rt. when F and C is 0 Rt=R0 
       
        
        # output$R_t <-Opt()$par[1]/Opt()$par[2]  
        # Opt$message
        ## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
        
        Opt_par <- setNames(Opt()$par, c("beta", "gamma","siggma","C","F_Value"))
        C.<<-C
        F._Value<<-F_Value
        output$c_value <- renderText( (1-Opt()$par[4])*100-((1-C)*100) )
        output$f_value<- renderText( (Opt()$par[5])*100+((F_Value)*100) )
        ######R2 of model confirmity
        #DF[min(which(DF$Confirmed> 0)),1 ]
        t <- seq(DATE[1], tail(DATE, n=1), "day")
        total_days2<- difftime(t,t[1] ,units="days")
        fit1 <- data.frame(ode(y = init, times = as.vector(total_days2), func = SEIR, parms = Opt_par))
        fit1DF1<-cbind(fit1,Date=t)
        fit1DF1<-merge(fit1DF1,DF,by="Date",all = TRUE)
        fit1DF1<-na.omit( fit1DF1[,2:7])
        R2<-round(cor(fit1DF1$I,fit1DF1$Confirmed )^2,4)
       
        # observe({
        #   #       updateSelectInput(session, "var.corr", selected = input$var.corr)
        #   updateSliderInput(session,"C_value","Social contacting (%)",(1-Opt()$par[4])*100-((1-C)*100) , min = 0, max = 100)
        #   updateSliderInput(session,"F_value", "Population engages in decrease social contacts (%)",(Opt()$par[5])*100+((F_Value)*100), min = 0, max = 100)
        #     })
        # 
         
        ########### Forecasting step
        # DF[min(which(DF$Confirmed> 0)),1 ]
        t <- seq(DATE[1], tail(DATE, n=1) + as.numeric(input$For_day), "day")
        total_days2<- difftime(t,t[1] ,units="days")
        
        fitBase <- data.frame(ode(y = init, times = as.vector(total_days2), func = SEIR, parms = Opt_par))
        fitBase<-fitBase%>% 
          dplyr::rename(S.b = S,I.b = I,R.b = R,E.b = E)
        ########### Intervention by social distancing
        fitInterv <- data.frame(ode(y = init, times = as.vector(total_days2), func = SEIR, parms = replace(Opt_par, c("C","F_Value"), c( C,
                                                                                                                                         F_Value)
        )))
        fitInterv<-fitInterv%>% 
          dplyr::rename(S.i = S,I.i = I,R.i = R, E.i = E)
        
        FitDF1<-cbind(fitBase,fitInterv[-1])
        col <- 1:8 # colour
        FitDF1<-cbind(FitDF1,Date=t)
        FitDF<-merge(FitDF1,DF,by="Date",all = TRUE)
        FitDF <- melt(FitDF[,-2], id.vars=1)
        
        Imax<<-FitDF1[FitDF1$I.b == max(FitDF1$I.b), "I.b", drop = FALSE][1,1]
        DateMax<<-FitDF1[FitDF1$I.b  == max(FitDF1$I.b), "Date", drop = FALSE][1,1]
        
        #ImaxInterv<-FitDF1[FitDF1$I.i== max(FitDF1$I.i), "I.i", drop = FALSE][1,1]
        ItimemaxInterv<<-FitDF1[ FitDF1$I.i== max(FitDF1$I.i) , "time", drop = FALSE][1,1]
        ImaxInterv<<-FitDF1$I.i[ItimemaxInterv+1]
        INextmaxInterv<<-FitDF1$I.i[ItimemaxInterv+2]
        
        DateMaxInterv<<-FitDF1[FitDF1$I.i== max(FitDF1$I.i), "Date", drop = FALSE][1,1]
        
        
        output$R2Coef <- renderText(paste("R<sup>2</sup> for 'confirmed' cases and 'I.b':",R2))
         
        # output$ESIRTable <- DT::renderDataTable(datatable(FitDF, 
        #                                                 options = list(pageLength = 6,lengthChange=FALSE)) %>%
        #                                         formatRound(c(3:9), 2) %>% 
        #                                         formatStyle(columns = c(3:9), 'text-align' = 'center'))  
        # 
        # 
     
        output$SEIR_Curent <- renderUI({
          if (min(FitDF1$I.b) < 0 |min(FitDF1$R.b)  < 0|min(FitDF1$S.b)  < 0 |min(FitDF1$E.b) < 0){
            withMathJax(helpText("SEIR model could not converged well, try again if this message persists by changing initial parameters." ))
          }else{
            
            markdown::markdownToHTML(knit("SEIRcurent.Rmd", quiet = TRUE),fragment.only=TRUE)
            withMathJax(includeMarkdown('SEIRcurent.md'))
          }
          
         
           # withMathJax(includeMarkdown("www//Test.Rmd"))
          #includeMarkdown(rmarkdown::render("www//Test.Rmd"))
        })
        
       
        
        output$SEIR_Interv <- renderUI({
          if ( as.numeric(input$F_value) > 0 ){
            markdown::markdownToHTML(knit("SEIRinterv.Rmd", quiet = TRUE),fragment.only=TRUE)
            withMathJax(includeMarkdown('SEIRinterv.md'))
          }
          
          # withMathJax(includeMarkdown("www//Test.Rmd"))
          #includeMarkdown(rmarkdown::render("www//Test.Rmd"))
        })
        
        
        FitDF  
       
      })

    
    SEIRPLOT<-reactive(
      SEIRCalc()[which(SEIRCalc()$variable %in% input$SeriesCurve), ]%>%
        dplyr::group_by(variable)%>% 
        echarts4r::e_charts(Date) %>% 
        echarts4r::e_line(value) %>% 
       # echarts4r::e_color(color = NULL, background = NULL),
        # echarts4r::e_line(model, name = "Fit", lineStyle = ls) %>% 
        echarts4r::e_tooltip(trigger = "axis")%>% 
        e_grid(containLabel=T,left = "2%",right = "2%")   #show axes title completely
      
    )
    
    output$SERTPlot <-  renderEcharts4r({ 
      
      SEIRPLOT()
    })
    

    
    output$downloadSEIRtable <- downloadHandler(
      
      filename = function() {
        paste('SEIR table', '.csv', sep='')
      },
      content = function(file) {
        write.csv(SEIRDataTable(), file)
      }
    )
    
   
    
    
    SEIRDataTable<-reactive(
      
      tidyr::pivot_wider(SEIRCalc()[which(SEIRCalc()$variable %in% input$SeriesCurve), ],
                         c(Date),
                         names_from = variable,
                         values_from = value)      
      
    )
    
    
    output$SEIRTable <- DT::renderDataTable(   DT::datatable(SEIRDataTable(),
                                                        extensions = c("FixedColumns", "FixedHeader", "Scroller"),
                                                        options = list(pageLength = 5,lengthChange=FALSE,
                                                                       #  dom = 't',
                                                                       # deferRender = TRUE,
                                                                       searching = F,
                                                                       #autoWidth = F,
                                                                       # scrollCollapse = TRUE,
                                                                       rownames = FALSE,
                                                                       scroller = TRUE,
                                                                       scrollX = TRUE,
                                                                       scrollY = "200px",
                                                                       #fixedHeader = TRUE,
                                                                       class = 'row-border' 
                                                                       # fixedColumns = list(
                                                                       #   leftColumns = 3,
                                                                       #   heightMatch = 'none'
                                                                       # )
                                                        )))  
    
    #%>% formatRound(columns = c( ) , 0) 
 
    
    
    # options = list(pageLength = 6,lengthChange=FALSE)) %>%
    #   formatRound(c(3:9), 2) %>% 
    #   formatStyle(columns = c(3:9), 'text-align' = 'center')
    # 
    output$downloadSEIR <- renderUI({ 
      req(SEIRDataTable())
      downloadButton("downloadSEIRtable", "Download the table")
    } )
    
    TREND<-reactive(
      if (input$Modification=="Cumulative cases"){ 
        
        DF<- PlotCum()
        # DF<-DF[which(DF[,"variable"]==input$series), ]
        
        
        if(input$Log==TRUE){
          DF[,"value"]<-log(DF[,"value"])
        }else{
          DF<-DF
        }
        
        DF%>%
          dplyr::group_by(variable)%>% 
          echarts4r::e_charts(Date) %>% 
          echarts4r::e_title(subtext = ETSData()[[2]])%>%
          echarts4r::e_line(value) %>%
          echarts4r::e_color( c("brown", "green","red") )%>%  #,background = "cyan" 
          # echarts4r::e_line(model, name = "Fit", lineStyle = ls) %>% 
          echarts4r::e_tooltip(trigger = "axis")%>%  
          e_grid(containLabel=T,left = "5%",right = "5%")%>%   #show axes title completely
          e_theme("shine") # https://echarts4r.john-coene.com/articles/themes.html
        
      } else if  (input$Modification=="Daily cases") {
        
        DF<- Plotdiff()
        #  DF<-DF[which(DF[,"variable"]==input$series), ]
        
        if(input$Log==TRUE){
          DF[,"diff"]<-log(DF[,"diff"])
        }else{
          DF<-DF
        }
        DF%>%
          dplyr::group_by(variable)%>% 
          echarts4r::e_charts(Date) %>% 
          echarts4r::e_title(subtext = ETSData()[[2]])%>%
          echarts4r::e_line(diff) %>% 
          echarts4r::e_color( c("brown", "green","red") )%>% #,background = "cyan"
          # echarts4r::e_line(model, name = "Fit", lineStyle = ls) %>% 
          echarts4r::e_tooltip(trigger = "axis")%>%
          e_grid(containLabel=T,left = "5%",right = "5%")%>%   #show axes title completely
          e_theme("shine") # https://echarts4r.john-coene.com/articles/themes.html
        
        
      }
    )
    
    
    output$TrendPlot <-  renderEcharts4r({
            TREND()
    })
 

   
    output$mymap<-renderLeaflet ({
      
      if (input$MapRadio=="Cumulative"){ 
        
        DF<- WorldDataMg
        
      }else{
        DF<- WorldDiffMg
      }
      
      colnames(DF)[2]<-"lat"
      colnames(DF)[3]<-"lon" 
      
      leaflet::leaflet(data = DF) %>% # create leaflet object
        leaflet::addTiles() %>% # add basemap
        #leaflet::addCircleMarkers( clusterOptions = markerClusterOptions())#,color = ~pal(masraf),popup= ~paste0(name.bakhsh, "<br/>Application: ", masraf) ,label=~as.character(noe.sazand),labelOptions = labelOptions(noHide = F, direction = 'top'))# add data layer - markers
        #leaflet::addMarkers(clusterOptions = markerClusterOptions(), popup= ~paste0(lon,lat, "<br/>Number: ", value,sep=",") ,label=~as.character(Country.Region),labelOptions = labelOptions(noHide = F, direction = 'top'))
        
        leaflet.minicharts::addMinicharts(
          DF$lon, DF$lat, 
          chartdata = log10(DF[,c("Confirmed", "Recovered", "Deaths")]),
          time = DF$Date,
          type="bar",
          colorPalette = c("orange", "green", "red"),
          popup=leaflet.minicharts::popupArgs(showTitle = TRUE,showValues = F,
                                              labels = NULL,supValues = DF[,c("Country.Region","Province.State","Date","Confirmed", "Recovered", "Deaths")]),
          width =20, height = 40
        )   
    })
    
    

    DEATHRATE<-reactive(
      {
        
        form <- htmlwidgets::JS("function(value){
    return(value + '%')
  }")
        
        if (input$Modification=="Cumulative cases"){ 
          DF<- PlotCum()
          
          # DF<-DF[which(DF$variable==input$series), ]
          
          
          if(input$Log==TRUE){
            DF[,"value"]<-log(DF[,"value"])
          }else{
            DF<-DF
          }
          
          DF %>% 
            dplyr::group_by(variable) %>% 
            dplyr::ungroup() %>%
            tidyr::pivot_wider(
              id_cols = Date,
              names_from = variable,
              values_from = value
              
            ) %>%
            dplyr::mutate(
              #https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30195-X/fulltext
              #https://www.worldometers.info/coronavirus/coronavirus-death-rate/
              rate = Deaths / (lag(Confirmed,14)),
              rate = round(rate * 100, 3)
            ) %>% 
            mutate(rate=replace(rate, rate>100, 100)) %>%
            echarts4r::e_charts(Date) %>% 
            echarts4r::e_area(rate, name = "Death rate") %>% 
            echarts4r::e_tooltip(trigger = "axis") %>% 
            echarts4r::e_legend(FALSE) %>% 
            echarts4r::e_y_axis(formatter = form) %>%
            echarts4r::e_title(subtext = ETSData()[[2]])%>%
          #  echarts4r::e_color(background = "cyan" )%>%
            #echarts4r::e_visual_map(
            #   rate,
            #  show = FALSE,
            #  inRange = list(
            #    color = deaths_pal
            #  )
            # ) %>% 
            echarts4r::e_mark_point(
              data = list(type = "max"), 
              itemStyle = list(color = "skyblue"),
              label = list(color = "#000"),
              title = "Max"
            ) %>% 
            echarts4r::e_mark_line(
              data = list(type = "average"),
              itemStyle = list(color = "blue"),
              title = "Average"
            ) %>%
            e_grid(containLabel=T,right = "20%",left = "5%")%>%   #show axes title completely
            e_theme("shine") # https://echarts4r.john-coene.com/articles/themes.html
          
          
        } else if  (input$Modification=="Daily cases") {
          
          DF<- Plotdiff()
          
          
          #DF<-DF[which(DF$variable==input$series), ]
          if(input$Log==TRUE){
            DF[,"diff"]<-log(DF[,"diff"])
          }else{
            DF<-DF
          }
          DF %>% 
            dplyr::group_by(variable) %>% 
            dplyr::ungroup() %>%
            tidyr::pivot_wider(
              id_cols = Date,
              names_from = variable,
              values_from = diff
              
            ) %>%
            dplyr::mutate(
              #https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30195-X/fulltext
              #https://www.worldometers.info/coronavirus/coronavirus-death-rate/
              rate = Deaths / (lag(Confirmed,14)),
              rate = round(rate * 100, 3)
            ) %>% 
            mutate(rate=replace(rate, rate>100, 100)) %>%
            echarts4r::e_charts(Date) %>% 
            echarts4r::e_area(rate, name = "Death rate") %>% 
            echarts4r::e_tooltip(trigger = "axis") %>% 
            echarts4r::e_legend(FALSE) %>% 
            echarts4r::e_y_axis(formatter = form) %>%
            echarts4r::e_title(subtext = ETSData()[[2]])%>%
           # echarts4r::e_color(background = "cyan" )%>%
            #echarts4r::e_visual_map(
            #   rate,
            #  show = FALSE,
            #  inRange = list(
            #    color = deaths_pal
            #  )
            # ) %>% 
            echarts4r::e_mark_point(
              data = list(type = "max"), 
              itemStyle = list(color = "skyblue"),
              label = list(color = "#000"),
              title = "Max"
            ) %>% 
            echarts4r::e_mark_line(
              data = list(type = "average"),
              itemStyle = list(color = "blue"),
              title = "Average"
            )%>%
            e_grid(containLabel=T,right = "20%",left = "5%")%>%   #show axes title completely
            e_theme("shine") # https://echarts4r.john-coene.com/articles/themes.html
          
          
        }
      }
    )
    
    
    output$RatePlot <-  renderEcharts4r(
      DEATHRATE() 
    )
 
 
    
    BarData<-reactive({
      if(input$BarRadio=="Cumulative"){
        DF<-WorldSum_F
      }else{
        DF<-WorldDiff_F
      }
      
      #########################population adjustment
      if(input$Popul==TRUE){
        DF$Confirmed<-(DF$Confirmed / Populations$Pop2020[match(DF$Country.Region, Populations$Country)]) *1000 # unit of popolation was *1000 thus i maultipled by 1000 to be per milion
        DF$Recovered<-(DF$Recovered / Populations$Pop2020[match(DF$Country.Region, Populations$Country)]) *1000
        DF$Deaths<-(DF$Deaths / Populations$Pop2020[match(DF$Country.Region, Populations$Country)])*1000 
      }
     
      #########################population adjustment
      
        if(input$SeriesBar=="Confirmed"){
          dd <- filter(DF[,-c(4:5)], Date == max(DF$Date) ) %>% 
            arrange(desc(Confirmed))%>%
            dplyr::rename(Var = Confirmed)
        }else if (input$SeriesBar=="Recovered") {
          dd <- filter(DF[,-c(3,5)], Date == max(DF$Date) ) %>% 
            arrange(desc(Recovered)) %>%
            rename(Var = Recovered)
        }else{
          dd <- filter(DF[,-c(3,4)], Date == max(DF$Date) ) %>% 
            arrange(desc(Deaths)) %>%
            rename(Var = Deaths)
          
          
        }
      
      dd
      
      })
      
    
    output$downloadBar <- downloadHandler(
      
      filename = function() {
        paste('Barplot data', '.csv', sep='')
      },
      
       content = function(file) {
        
          write.csv(if(input$SeriesBar=="Confirmed"){
            BarData() %>% 
              dplyr::rename(Confirmed = Var)
          }else if (input$SeriesBar=="Recovered") {
            BarData() %>% 
              dplyr::rename(Recovered = Var)
          }else{
            BarData() %>% 
              dplyr::rename(Deaths  = Var)
          }
                
                    
                    
                    , file)
        
      }
    ) 
    

    output$BarPlot <-  renderEcharts4r({
      #par(mfrow = c(1, 1), mar = c(0,8,0,8), oma = c(4,8,4,8))
    
      dd <- BarData() [1:as.integer(nrow(BarData())*input$Country_percent) , ]
      bs <- list(
        shadowColor = "rgba(0, 0, 0, 0.8)",
        shadowBlur = 5,
        shadowOffsetX = 3
      )
      dd %>% 
        dplyr::arrange(Var) %>% #desc(Date)  
       # dplyr::group_by(Date) %>% 
        echarts4r::e_charts(Country.Region ) %>% 
        echarts4r::e_bar( Var, name = input$SeriesBar , itemStyle = bs ) %>% 
        #echarts4r::e_bar(Recovered, name = "Recovered" ) %>% 
        #echarts4r::e_bar(Deaths,name = "Deaths") %>%  
        # echarts4r::e_labels(position = "right") %>% 
        # echarts4r::e_tooltip() %>% 
        echarts4r::e_flip_coords() %>%
        echarts4r::e_legend(
          orient = "vertical",
          right = 150,
          top = 350
        ) %>% 
        echarts4r::e_tooltip(
          trigger = "axis",
          axisPointer = list(
            type = "shadow"
          )
        )%>%
        
         e_axis_labels( x = ifelse(input$Popul==TRUE,"Cases/1000000","Number of Cases"), y = "Countries") %>%  
      
        
      e_grid(containLabel=T,right = "20%")%>%   #show axes title completely
        e_theme("shine") # https://echarts4r.john-coene.com/articles/themes.html
      
    })
    
    
     
    
    reactivehour <- reactive({
        
       # return(subset(BDcompleta, Hour == input$hour))
        
    })
    
    
# Plot
    
    output$deathTrend <- renderPlotly({
        
        df<-WorldSum_F%>%
        dplyr::arrange(desc(Date), desc(Confirmed))%>%
        rename( "Country"= Country.Region)
      gg<-ggplot(df,aes_string("Confirmed" , input$series, color = input$series )) +
        geom_point(aes_string(size = input$series,label="Country", frame = as.character("Date"), ids = "Country")) +
        # geom_text(aes(label=Country.Region, frame = as.character(Date)))+
        scale_x_log10()
     
       
      
      if(input$series=="Deaths"){
            gg<-gg+scale_color_gradient2(midpoint=mean(df$Deaths), low="yellow", mid="orange",
                                   high="red", space ="Lab" )
      }else{
        gg<-gg+scale_color_gradient2(midpoint=mean(df$Recovered), low="yellow", mid="blue",
                                     high="green", space ="Lab" ) 
       }


      ggplotly(gg,tooltip = c("size","ids","frame"))
        
    })
    
    
 
# 
    # output$downloadReport <- downloadHandler(
    #   filename = function() {
    #     paste('Report', sep = '.', 'pdf')
    #   },
    # 
    #   content = function(file) {
    #     src <- normalizePath('Report.Rmd')
    # 
    #     # temporarily switch to the temp dir, in case you do not have write
    #     # permission to the current working directory
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     file.copy(src, 'Report.Rmd', overwrite = TRUE)
    # 
    #     library(rmarkdown)
    #     out <- render('Report.Rmd', pdf_document())
    #     file.rename(out, file)
    #   }
    # )

    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.',"html" 
              
        #       switch(
        #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        # )
        
        )
      },
      content = function(file) {
        src <- normalizePath("Report.Rmd" )

        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "Report.Rmd", overwrite = TRUE)

        
        out <- rmarkdown::render("Report.Rmd",rmarkdown::html_document()
        #                          switch(
        #   input$format,
        #   PDF = rmarkdown::pdf_document(), HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
        # ) 
        )
        file.rename(out, file)
      }
    )
    
    ################################################## Country report
    output$downloadCountryReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.',"html" 
              
              #       switch(
              #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
              # )
              
        )
      },
      content = function(file) {
        src <- normalizePath("CountryReport.Rmd" )
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, "CountryReport.Rmd", overwrite = TRUE)
        
        
        out <- rmarkdown::render("CountryReport.Rmd",rmarkdown::html_document()
                                 #                          switch(
                                 #   input$format,
                                 #   PDF = rmarkdown::pdf_document(), HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
                                 # ) 
        )
        file.rename(out, file)
      }
    )
    
    
    
    # 
    # output$downloadWord <- downloadHandler(
    #   filename = function() {
    #     paste('word-report', sep = '.',"docx" 
    #           
    #           #       switch(
    #           #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    #           # )
    #           
    #     )
    #   },
    #   content = function(file) {
    #     src <- normalizePath("SEIRinterv.Rmd" )
    #     
    #     # temporarily switch to the temp dir, in case you do not have write
    #     # permission to the current working directory
    #     owd <- setwd(tempdir())
    #     on.exit(setwd(owd))
    #     file.copy(src, "SEIRinterv.Rmd", overwrite = TRUE)
    #     
    #     library(rmarkdown)
    #     out <- rmarkdown::render("SEIRinterv.Rmd",rmarkdown::word_document()
    #                              #                          switch(
    #                              #   input$format,
    #                              #   PDF = rmarkdown::pdf_document(), HTML = rmarkdown::html_document(), Word = rmarkdown::word_document()
    #                              # ) 
    #     )
    #     file.rename(out, file)
    #   }
    # )
    # 
    # 
    
    
    

# waiter2-------------------------------------------------------------
    # Sys.sleep(2)
    # update_waiter(html = tagList(spin_orbiter(),span("Getting back to work...", style="color:white;")))
    # Sys.sleep(2)
    # hide_waiter()
    ######################################################
    #################### Population involvement
    
    observeEvent(input$country1,{
       
        
      if(input$country1=="Select all"){
          updateTextInput(session,
                          "N_value",
                          "Population(N):", 7777504748
          )
        }else{
          updateTextInput(session,
                          "N_value",
                          "Population(N):",unique(Populations[Populations$Country %in%
                                                                input$country1, "Pop2020"]*1000)
          )
        }
       
    })
    
    #################### Population involvement
    

    
    output$SEIRCreditPage <- renderUI({
      
#off#      req(credentials()$user_auth) # From Ui (SEIR analysis)
     
      ######################################################## The fuidepage only rendered for valid credentials
       fluidPage(  
        fluidRow( 
          br(),br(),
          valueBox( textOutput("beta") ,HTML(paste("Infection rate","(",HTML("&beta;"),")", sep = "")) ,  color =  "green", width = 3),
          valueBox( textOutput("sigma"),HTML(paste("Incubation rate","(",HTML("&sigma;"),")", sep = "")), color = "green", width = 3),
          valueBox( textOutput("gamma") ,HTML(paste("Recovery rate","(",HTML("&gamma;"),")", sep = "")), color = "green", width = 3),
          valueBox( textOutput("R_0") ,HTML(paste("Reproduction number","(",HTML(paste("R", tags$sub(0), sep = "")),")", sep = "")),  color = "green", width = 3),
          valueBox( textOutput("R_t") ,HTML(paste("Effective reproduction number","(",HTML(paste("R", tags$sub("t"), sep = "")),")", sep = "")),  color = "green", width = 3),
          valueBox( textOutput("c_value") ,HTML(paste("Social contact(%)","(",HTML(paste("c" , sep = "")),")", sep = "")),  color = "green", width = 3),
          valueBox( textOutput("f_value") ,HTML(paste("Population involved(%)","(",HTML(paste("f" , sep = "")),")", sep = "")),  color = "green", width = 3)
          
          
        ), 
        br(),          
        
############################################################## Statdard user 
#off#        if (user_info()$permissions == "standard") { 
#off#       column(width = 2,
               
#off#               selectInput(inputId ="country1",
#off#                          label = "Country:",
#off#                           choices = c("Select all","Iran"),
#off#                           selected = "Select all"), 
               
#off#               selectInput(inputId = "province1", #name of input
#off#                           label = "Province:", #label displayed in ui
#off#                           choices = dput(as.character(as.character(c("Select all",unique(WorldData$Province.State))))),
#off#                           selected = "Select all"),#calls list of available counties
#off#               br(), 
#off#               box(title = tags$b("SEIR model parameters"), width = 13, status = "warning", solidHeader = TRUE,
#off#                   textInput("N_value", "Population(N):" ,7777504748),
#off#                   textInput("For_day", "Forecast step (days):",200),
#off#                   sliderInput("C_value", "Social contacting (c)", 100, min = 0, max = 100),
#off#                   sliderInput("F_value", "Population engages in decrease social contacts (f)", 0, min = 0, max = 100 ),
#off#                   sliderInput("S_value", "S initial fraction", 0.9, min = 0, max = 1),
                   
                   # sliderInput("I_value", "I initial fraction", 2.5*10^-7, min = 0, max = 1,step=0.0000001),
#off#                   hr(),
#off#                   sliderInput("beta_value", HTML("&beta;:"), 0.5, min = 0, max = 1,step=0.01),
#off#                   sliderInput("sigma_value", HTML("&sigma;:"), 0.2, min = 0, max = 1,step=0.01),
#off#                   sliderInput("gamma_value", HTML("&gamma;:"), 0.04, min = 0, max = 1,step=0.01)
                   #actionButton("Apply", "Apply")
                   
#off#               ), 
#off#               br(),
#off#               uiOutput("downloadSEIR"),
#off#               br(),
#off#               downloadButton( "downloadReport" ,  "Make a report")
               
               
#off#        )
        
#off#          } else if (user_info()$permissions == "admin") {  ################################ dmin user 
          column(width = 2,
                 
                 selectInput(inputId ="country1",
                             label = "Country:",
                             choices = dput(as.character(as.character(c("Select all",unique(WorldData$Country.Region))))),
                             selected = "Select all"), 
                 
                 selectInput(inputId = "province1", #name of input
                             label = "Province:", #label displayed in ui
                             choices = dput(as.character(as.character(c("Select all",unique(WorldData$Province.State))))),
                             selected = "Select all"),#calls list of available counties
                 br(), 
                 box(title = tags$b("SEIR model parameters"), width = 13, status = "warning", solidHeader = TRUE,
                     textInput("N_value", "Population(N):" ,7777504748),
                     textInput("For_day", "Forecast step (days):",200),
                     sliderInput("C_value", "Social contacting (c)", 100, min = 0, max = 100),
                     sliderInput("F_value", "Population engages in decrease social contacts (f)", 0, min = 0, max = 100 ),
                     sliderInput("S_value", "S initial fraction", 0.9, min = 0, max = 1),
                     
                     # sliderInput("I_value", "I initial fraction", 2.5*10^-7, min = 0, max = 1,step=0.0000001),
                     hr(),
                     sliderInput("beta_value", HTML("&beta;:"), 0.5, min = 0, max = 1,step=0.01),
                     sliderInput("sigma_value", HTML("&sigma;:"), 0.2, min = 0, max = 1,step=0.01),
                     sliderInput("gamma_value", HTML("&gamma;:"), 0.04, min = 0, max = 1,step=0.01)
                     #actionButton("Apply", "Apply")
                     
                 ), 
                 br(),
                 uiOutput("downloadSEIR"),
                 br(), 
                 
                 ################################ Show report download for admin users
                # if (user_info()$permissions == "admin") {
                   
                   downloadButton( "downloadReport" ,  "Make a report")
                   
                # } #else if (user_info()$permissions == "standard") {
                 #   dplyr::storms[,1:11]
                 # }
                 #####################################
          )
#off#        }
,
        
        
        column(width = 9,
               fluidRow(box(title = tags$b("SEIR model"),style = "height:500px; background-color: yellow;", width = 12, status = "success", solidHeader = TRUE, 
                            withSpinner(echarts4rOutput("SERTPlot",width = "100%", height = "500px"),type=1),htmlOutput("R2Coef")
               ) 
               ),
               br(),
               uiOutput('SEIR_Curent'),
               uiOutput('SEIR_Interv'),
               br(),
               
               withSpinner(DT::dataTableOutput("SEIRTable"),type=1)   #uiOutput('ESIR')
               
        ),
        
        column(width = 1,
               checkboxGroupInput("SeriesCurve", label = "Curves:", 
                                  choices = c("Confirmed","Recovered","Deaths","S.b","E.b","I.b","R.b","S.i","E.i","I.i","R.i"), selected = c("Confirmed","Recovered","Deaths","I.b","I.i")) 
               
               
        )
        
      )
      
    })
    
   
    
    
    
     ############ Send comment
    observeEvent(input$send, {
      
      ms <- 'alert("Thank you for the advice!");'
      
      COMMENT <-   data.frame(times=Sys.time(), Name=input$name,Comment=input$commenttext)
      ALLCOMMENTS <<- rbind(ALLCOMMENTS,COMMENT)
      ### save localy
      saveRDS(ALLCOMMENTS, file = 'ALLCOMMENTS.rds') # On local server
      
      ### Email
      #java issue: https://stackoverflow.com/questions/50016207/unable-to-send-email-using-mailr-package
      from <- "m.hadi.shiny@gmail.com"
      to <- "m.hadi1981@gmail.com"
      subject <- "mailSubject"
    ###### When you have attach file
      #FileName <- paste0("Mes", as.integer(Sys.time()),".csv")
      #filePath <- file.path(tempdir(), FileName)
      #write.csv(COMMENT, filePath, row.names = FALSE, quote = TRUE)
    ###
      body<-paste(input$name,':',input$commenttext)
      mailR::send.mail(from ,to ,subject,body=body,smtp = list(host.name = "smtp.gmail.com", port = 465,
       user.name=from, passwd="", ssl=TRUE),authenticate = TRUE,send = TRUE) #attach.files=c(filePath)  
      ####
      
      session$sendCustomMessage(type='jsCode', list(value = ms))
      
      observe({
        updateTextInput(session,"name", value="") 
        updateTextAreaInput (session,"commenttext", value="") 
        
      })
      
    })
    
    
    
   
    
    
    ########################### Smothing modeling
    
    ETSData <- reactive({
      
      if(input$country=="Select all" & input$province=="Select all"){
        Lbl<-"World"
        list(WorldSumDiffAgr,Lbl)
      }else if (input$country!="Select all" & input$province=="Select all") {
        Lbl<-input$country
        SUM<-aggregate(value~ variable+Date+Country.Region,WorldSum[which(WorldSum$Country.Region==input$country), ],FUN = "sum" )%>%
          tidyr::pivot_wider(
            c(Date),
            names_from = variable,
            values_from = value)
        DIFF<-aggregate(diff~ variable+Date+Country.Region,WorldDiff[which(WorldDiff$Country.Region==input$country), ],FUN = "max" )%>%
          tidyr::pivot_wider(
            c(Date),
            names_from = variable,
            values_from = diff)%>%
          rename("New.Confirmed"=Confirmed, "New.Recoverds"= Recovered, New.Deaths ="Deaths" ) 
        
        list(merge(SUM,DIFF , by=c("Date")),Lbl)
        
      }else{
        Lbl<-input$province
        SUM<-aggregate(value~ variable+Date+Province.State,WorldSum[which(WorldSum$Country.Region==input$country & WorldSum$Province.State==input$province), ],FUN = "sum" )%>%
          tidyr::pivot_wider(
            c(Date),
            names_from = variable,
            values_from = value) 
        DIFF<-aggregate(diff~ variable+Date+Province.State,WorldDiff[which(WorldDiff$Country.Region==input$country & WorldDiff$Province.State==input$province), ],FUN = "max" )%>%
          tidyr::pivot_wider(
            c(Date),
            names_from = variable,
            values_from = diff)%>%
          rename("New.Confirmed"=Confirmed, "New.Recoverds"= Recovered, New.Deaths ="Deaths" ) 
        list(merge(SUM,DIFF , by=c("Date")),Lbl)
      }
    }) 
    
    
    
    ########### ETS function
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
   ETSDataModel <- reactive({
      
         data_res <- ETSData()[[1]]
      
         
      data_forec <- forec_cases_cumsum(data_res, input$Horiz,input$ForcastSeries )
      
      data_res <- rbindlist(list(
        data_res,
        data.table(Date= seq.Date(max(data_res$Date)+1,max(data_res$Date)+input$Horiz, by = 1),
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
   output$ETS_plot <- renderDygraph({
      
      data_res <- ETSDataModel()
      
      dygraph(data_res[, .(Date, 'Cases' = get(input$ForcastSeries),
                           Cases_cumsum_mean, Cases_cumsum_lwr, Cases_cumsum_upr)],
              main = paste0(ETSData()[[2]],
                            ", model: ",
                            data_res[, unique(Model)])) %>%
        # dyAxis("y", label = "Cases - cumulative") %>%
        dySeries('Cases') %>%
        dySeries(c("Cases_cumsum_lwr", "Cases_cumsum_mean", "Cases_cumsum_upr"),
                 label = "Forecast") %>%
        dyRangeSelector(dateWindow = c(data_res[, max(Date) - input$Horiz - input$Horiz],
                                       data_res[, max(Date) + 1]),
                        fillColor = "#1a95ed", strokeColor = "#222d32") %>%
        dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                  fillGraph = TRUE, fillAlpha = 0.4,
                  drawPoints = TRUE, pointSize = 3,
                  pointShape = "circle",
                  colors = c("#1a95ed", "#f7a10c")) %>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
        dyEvent(data_res[is.na(Cases_cumsum_mean), max(Date)],
                "Forecasting base", labelLoc = "bottom") %>%
        dyLegend(width = 150, show = "auto", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)
      
    })
    
    
    
    
    
    
    
    
    
}

