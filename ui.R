
# Header ------------------------------------------------------------------
 


header <- dashboardHeader(title = 'COVID19 Reporter', titleWidth = 400,
                          tags$li(a(href = 'http://ier.tums.ac.ir/',
                                    img(src = 'IER.png', 
                                        title = "Click to visit IER",
                                        height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"
                          ),
                          class = "dropdown"),
                          
                          tags$li(a(href = 'http://en.tums.ac.ir/en',
                                    img(src = 'TUMS.jpg', 
                                        title = "Click to visit TUMS",
                                        height = "30px"),
                                    style = "padding-top:10px; padding-bottom:10px;"
                          ),
                          class = "dropdown")
                          
)




# Sidebar Menu ------------------------------------------------------------
sidebar <- dashboardSidebar(width = 220,
                             
                            sidebarMenu(
                                menuItem("Introduction", tabName = "intro", icon = icon("align-justify")),
                                menuItem("Tables", tabName = "data", icon = icon("table")),
                                menuItem("Data analysis", tabName = "descriptive", icon = icon("chart-bar")),
                                menuItem("About", tabName = "about", icon = icon("info-circle")),
                                hr(),
                                
                                # sidebarUserPanel() was changed by inserting "style="background-color:transparent;" as follows because leaflet package overwritten background of user pannel 
                                HTML('<div class="user-panel">
  <div class="pull-left image" >
  <img src="https://www.tums.ac.ir/1395/10/07/6705-2016-12-27-07-41.jpg" class="img-circle" alt="User Image"/>
  </div>
  <div class="pull-left info" style="background-color:transparent;right: 4px">
  <p>
  <a target="_blank_" href="https://www.tums.ac.ir/faculties/m-hadi">Mahdi Hadi</a>
  </p>
  Assistant professor<br/>at IER
</div>
</div>'),
                                hr(),
                                # menuItem("Source code", icon = icon("file-code-o"), 
                                #          href = " ")
                                #img(src = "Corona.png", height = 100, width = 100)
                                
                                tags$head(tags$style("#Update{color: yellow;
                                 font-size: 16px;
                                 font-style: bold;
                                 }"
                                )
                                ),
                              
                               uiOutput('Version'),
                               
                                fluidRow(box(title = "Send your comment!", width=11 ,style = "background-color: black",status = "danger", solidHeader = TRUE, 
                                             textInput("name", label = "Your name:"),
                                             textAreaInput("commenttext", label = "Write your comment:", height="150px"),
                                             actionButton("send", "send"),
))
                            )                           
)
 
# Options for Spinner
options(spinner.color="red", spinner.color.background="yellow", spinner.size=2)
# Body section ------------------------------------------------------------

#------- waiter1 
# spinner <- tagList(
#   spin_chasing_dots(),
#   span("Loading stuff...", style="color:white;")
# )

#-waiter-------4 
#Sys.sleep(3)
body <- dashboardBody(

 ############ Include css for rmd column formatting
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "rmdCol.css")
  ),
 ############### 
  ##################### To display icons
  #https://www.angularjswiki.com/angular/font-awesome-icons-list-usage-css-content-values/
   tags$head(
  # tags$link( rel="stylesheet" ,href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.9.0/css/all.css") ), #When server is online
   tags$link( rel="stylesheet" ,type = "text/css",href="fontawesome/css/all.css") ), #When server is offline

  #Download fontaowsome package : https://fontawesome.com/how-to-use/on-the-web/setup/hosting-font-awesome-yourself
  
  
  
  
  ##################### Countries flags
  
  tags$head(
   #https://github.com/lipis/flag-icon-css
    tags$link( rel="stylesheet" ,type = "text/css",href="flags/css/flag-icon.css") ), #When server is offline
  
  ##################### Countries flags
  
#-------waiter1 
  # use_waiter(),
  # waiter_show_on_load(spinner), # will show on load
  # 
#-------waiter2  
  # use_waiter(include_js = T), # do not include js
  # show_waiter_on_load(html = tagList(spin_orbiter(),span("Loading Dash...", style="color:white;")), color = "#3A3F44"), # place at the bottom
#--------waiter3
#use_waitress(),
  #--------waiter4
 #use_waiter(include_js = T),
# waiter_show_on_load(spin_fading_circles()), # place at the bottom


tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
         
      }
    '))),
  
  
  tags$head(tags$style(HTML(".grViz { width:100%!important;}"))), #For plot aligning in box but not sure to work!
  
  
  tags$head(tags$style(HTML('
      .main-sidebar {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 13px;
        background-color: red;

      }
    '))),
  
  tags$head(tags$style(HTML('
.box {margin: 5px;}'
  ))),
  
  # tags$head(tags$style(HTML("
  #                            body {
  #                               width: 100% !important;
  #                               max-width: 100% !important;
  #                            }
  # 
  #                            "))), #to prevent rmarkdown shink page of dashboard 
  
  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
  
  #set color of boxplot control box in data analysis tab  
  #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: yellow; border-top: 2px yellow; border-bottom: 2px yellow;border-left: 2px yellow}")),
  
  #tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:red}")),
  
  #change dashboard body and sliderbar background
  tags$head(tags$style(HTML('
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: black;
                                }
  

                                '))),


  
  
    tabItems(
        
            tabItem("intro", includeMarkdown("Introduction.Rmd"),
       
                includeMarkdown("Contact.Rmd")), 
        
        tabItem("data",
                uiOutput("DataText"),
                withSpinner(DT::dataTableOutput("datatable"),type=1),
                tags$style(type="text/css", "#downloadCumtable {background-color:orange;color: black;  height: 30px; width: 160px;font-size: 14px}"),
                downloadButton("downloadCumtable", "Download the table"),
                uiOutput("RawText"), 
                withSpinner(DT::dataTableOutput("rawdata"),type=1),
                tags$style(type="text/css", "#downloadRaw {background-color:orange;color: black;  height: 30px; width: 160px;font-size: 14px}"),
                downloadButton("downloadRaw", "Download the table"),
                includeMarkdown("Contact.Rmd")
               
        ), 
        tabItem("descriptive", 
                tabsetPanel(id="TABS",
                    tabPanel("Trend", icon = icon("chart-line"), #https://fontawesome.com/icons/globe?style=solid
                                
                         
                             fluidRow(
                              #https://www.angularjswiki.com/angular/font-awesome-icons-list-usage-css-content-values/
                              # valueBox( countupOutput("Con") ,"Confirmed", icon=tags$i(class="fas fa-temperature-high" ), color =  "yellow", width = 4), #icon = icon("temperature-high"),
                               valueBox(countupOutput("Con"), fluidRow(
                                 column(HTML("Confirmed <br> Latest 5 days slope: <br> Latest 10 days slope: <br> Latest 20 days slope:"), status = "primary", title = "",  width = 6 , height = 120),
                                 column("",br(),countupOutput("ConS5") ,br(), countupOutput("ConS10") ,br(), countupOutput("ConS20"), status = "primary", title = "Daily confirmed cases change (People)/day",  width = 6 , height = 120)
                               ),icon=tags$i(class="fas fa-temperature-high" ), color =  "orange", width = 4,), 
                               
                               #valueBox( countupOutput("Rec")  ,"Recovered",icon=tags$i(class="fas fa-hand-holding-heart" ), color = "green", width = 4), #icon = icon('hand-holding-heart'),
                               #withSpinner(countupOutput("Rec"), custom.css = T, type = 1)
                               valueBox(countupOutput("Rec"), fluidRow(
                                 column(HTML("Recovered <br> Latest 5 days slope: <br> Latest 10 days slope: <br> Latest 20 days slope:"), status = "primary", title = "",  width = 6 , height = 120),
                                 column("",br(),countupOutput("RecS5") ,br(), countupOutput("RecS10"),br(), countupOutput("RecS20"), status = "primary", title = "Daily recovered cases change (People)/day",  width = 6 , height = 120)
                               ),icon=tags$i(class="fas fa-hand-holding-heart" ), color =  "green", width = 4,),
                               
                               
                               #valueBox( countupOutput("Dea") ,"Deaths", icon=tags$i(class="fas fa-cross" ),color = "red", width = 4), #icon = icon('cross'),
                               
                               valueBox(countupOutput("Dea"), fluidRow(
                                 column(HTML("Deaths <br> Latest 5 days slope: <br> Latest 10 days slope: <br> Latest 20 days slope:"), status = "primary", title = "",  width = 6 , height = 120),
                                 column("",br(),countupOutput("DeaS5") ,br(), countupOutput("DeaS10"),br(), countupOutput("DeaS20"), status = "primary", title = "Daily dead cases change (People)/day",  width = 6 , height = 120)
                               ),icon=tags$i(class="fas fa-cross" ), color =  "red", width = 4,)
                               
 
                               
                                
                             ),
                             
                             
                             
                             #WorldSumMax
                             
                             
                             fluidRow(
                                 column(width = 10,
                                        fluidRow(box( title = tags$b("Trend and death rate "),style = "background-color: yellow;", width = 12, status = "warning", solidHeader = TRUE,
                                                     checkboxInput("Log", "Log scale", value = FALSE),         
                                                     
                                                     fluidRow(width = 12, 
                                                              column(h4("Ourbreak trend"),width = 7, align="center",div(style = "margin:0px 0px 0px 0px; padding: 0px  0px 0px  0px",withSpinner(echarts4rOutput(width = "100%","TrendPlot",height="400px"), type = 1))),
                                                              column(h4("Death rate"),width =5,align="center", div(style = "margin:0px 0px 0px 0px; padding: 0px 0px 0px 0px",withSpinner(echarts4rOutput(width = "100%","RatePlot",height="400px"), type = 1)))
                                                              )),
                                                 box( title = tags$b("Growth rate and curve flattening index"),style = "background-color: yellow;", width = 12, status = "warning", solidHeader = TRUE,
                                                      #checkboxInput("Log", "Log scale", value = FALSE),         
                                                      
                                                      fluidRow(width = 12, 
                                                               column(h4("Growth rate"),width = 7, align="center",div(style = "margin:0px 0px 0px 0px; padding: 0px  0px 0px  0px",withSpinner(plotlyOutput(width = "100%","growthRate",height="400px"), type = 1))),
                                                               column(h4("Curve flattening index"),width =5,align="center", div(style = "margin:0px 0px 0px 0px; padding: 0px 0px 0px 0px",withSpinner(plotlyOutput(width = "100%","cfi",height="400px"), type = 1)))
                                                      )))
                                         
                                      
                                       # column(width = 7,withSpinner(echarts4rOutput(width = "100%","TrendPlot",height="400px"), type = 1)),
                                       #column(width = 5,withSpinner(echarts4rOutput(width = "100%","RatePlot",height="400px"), type = 1)) 
                                 ),
                                 
                                 
                                 
                               #  style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:5px',
                                 column(width = 2,
                                        
                                        selectInput(inputId ="country",
                                                    label = "Country:",
                                                    choices = dput(as.character(as.character(c("Select all",unique(WorldData$Country.Region))))),
                                                    selected = "Select all"),
                                        
                                        selectInput(inputId = "province", #name of input
                                                    label = "Province:", #label displayed in ui
                                                    choices = dput(as.character(as.character(c("Select all",unique(WorldData$Province.State))))),
                                                    selected = "Select all"),#calls list of available counties
                                                    
                                        
                                        
                                                    # br(), checkboxGroupInput("series", label = "Select the type of data searies:", 
                                                                    #          choices = c(levels(WorldSum$variable)), selected = c("Confirmed", "Recovered", "Deaths")),
                                                     br(), 
                                        radioButtons("Modification", label = "Apply modification to series:", 
                                                                             choices = c("Cumulative cases","Daily cases"), selected = c("Cumulative cases")),
                                        tags$style(type="text/css", "#downloadCountrytable {background-color:orange;color: black;  height: 30px; width: 100%;font-size: 14px}"),
                                        downloadButton("downloadCountrytable", "Download data"),
                                        br(),br(),
                                        tags$style(type="text/css", "#downloadCountryReport {background-color:orange;color: black;  height: 30px; width: 100%;font-size: 14px}"),
                                        downloadButton( "downloadCountryReport" , "Make a report ")
                                        
                                        )
                                        
                               
                                
                                 ),
                              
                             fluidRow(
                               column(width = 10,
                                      fluidRow(box(title = tags$b("Forecasting using ETS (Exponential smoothing)"), style = "background-color: yellow;", width = 12,height="564px", status = "warning", solidHeader = TRUE, withSpinner(dygraphOutput(width = "100%","ETS_plot",height="500px"), type = 1)  )
                                      )
                               ),
                               
                               column(width = 2,
                                      sliderInput(
                                        inputId = "Horiz", 
                                        label = "Set days ahead to forecast:",
                                        min = 1,
                                        max = 30,
                                        value = 10
                                      ),
                                      radioButtons("ForcastSeries", label = "Select variable:", 
                                                   choices = c("Confirmed","Recovered","Deaths"), selected = c("Confirmed"))
                                      
                               )
                             ),
                             
                             
                             fluidRow(
                                 column(width = 10,
                                        fluidRow(box(title = tags$b("Deaths and recovered cases over time"), style = "background-color: white;", width = 12,height="600px", status = "warning", solidHeader = TRUE, withSpinner(plotlyOutput("deathTrend",height="530px"),type=1))
                                        )
                                 ),
                                 
                                 column(width = 2,
                                        radioButtons("series", label = "Select variable:", 
                                                                    choices = c("Recovered","Deaths"), selected = c("Deaths"))
                                        
                                 )
                             ),
                             includeMarkdown("Contact.Rmd")
                           
                    ),
                    tabPanel("Map and Barplot", icon = icon("globe"), 
                             
                             # h2(tags$b("Factors that increase pollution in the Aburra Valley:")),
                             
                             # fluidRow(valueBox(h3("Topography"), "A narrow and semi-closed valley.", width = 4, color = "red", icon = icon("image")),
                             #          valueBox(h3("Meteorology"), "Atmospheric stability and low ventilation.", width = 4, color = "red", icon = icon("cloudversify")),
                             #          valueBox(h3("Emissions"), "Pollution in a densely populated region.", width = 4, color = "red", icon = icon("bus"))
                             # ),
                             # 
                             # h2(tags$b("Bar charts for epidemic parameters over time for countries")),
                             
                             #fluidRow(
                             #    valueBox(round(mean(BDcompleta$Pm25),3), "Average PM 2.5 (μg/m^3)",icon = icon("info"), color = "green", width = 4),
                             #    valueBox(round(mean(BDcompleta$Precipitation),3),"Average Precipitation (mm)",icon = icon("info"), color = "green", width = 4),
                             #    valueBox(round(mean(BDcompleta$Wind_direction),3),"Average Wind direction (°)",icon = icon("info"), color = "green", width = 4)
                            # ),
                             
                            # fluidRow(
                          #       valueBox(round(mean(BDcompleta$Wind_speed),3), "Average Wind speed (m/s)",icon = icon("info"), color = "yellow", width = 4),
                           #      valueBox(round(mean(BDcompleta$Air_temperature),3),"Average Air temperature (°C)",icon = icon("info"), color = "yellow", width = 4),
                           #      valueBox(round(mean(BDcompleta$Air_humedity),3),"Average Air humedity (%)",icon = icon("info"), color = "yellow", width = 4)
                           #  ),
                             
                          fluidRow(
                            column(width = 12,
                                   fluidRow(box(height="820px",title = fluidRow(   
                                      column(12,  radioButtons("MapRadio", label = "Epicenters over time",inline=T,choices = c("Cumulative","Daily"), selected = c("Cumulative")))
                                   ) , width = 12, status = "warning" , solidHeader = TRUE,
                                   withSpinner(leafletOutput("mymap",height="700px"),type=1))
                                   )
                            )
                                
                              ), 
                             
                             fluidRow(
                                 column(width = 9,
                                        fluidRow(box(title = tags$b("Countries bar plot"),style = 'display:block; height:100%;overflow-y: scroll;background-color: yellow;', width = 12,  status = "warning", align = "center", 
                                                     solidHeader = TRUE,collapsible = F, div(style = "margin:0px 0px 0px 0px; padding: 0px 0px 0px 0px",withSpinner(echarts4rOutput(width = "100%","BarPlot",height="500px"), type = 1)))
                                        )  
                                 ),
                                   
                                 column(width = 2,
                                      #  fluidRow(box(title = tags$b("Control box for the correlation graph"), width = 12, status = "success", solidHeader = FALSE, selectInput("var.corr", "select the variables to be displayed:",
                                      #                                                                                                                                         choices = names(cuantis),
                                      #                                                                                                                                         selected = names(cuantis), multiple = TRUE))
                                       # ),
                                        fluidRow(radioButtons("SeriesBar", label = "Select cases:", 
                                                              choices = c("Confirmed","Recovered","Deaths"), selected = c("Confirmed")),
                                                 radioButtons("BarRadio", label = "Series", choices = c("Cumulative","Latest date"), selected = c("Cumulative")),
                                                 sliderInput("Country_percent", "Include countries in bar cahrt", 0.1, min = 0, max = 1),
                                                 checkboxInput("Popul", "Population adjustment", value = FALSE),
                                                 tags$style(type="text/css", "#downloadBar {background-color:orange;color: black;  height: 30px; width: 160px;font-size: 14px}"),
                                                 downloadButton("downloadBar", "Download data")
                                        )
                                 )
                             ),
                           includeMarkdown("Contact.Rmd")
                    ),
                    
                   
                     tabPanel("Modeling", icon = icon("desktop"), 
                             
                             
                             fluidPage( 
                             fluidRow(
                               tabsetPanel(
                                tabPanel("SEIR model", icon = icon("info-circle"),
                                         fluidPage( 
                                         fluidRow(fluidRow(column(12,
                                                                  withMathJax(),
                                                                  h2("Model Description"),
                                                                  
                                                                  includeMarkdown("SEIR.Rmd")
                                                                 
                                                               
                                         )))
                                        ),  
                                           
                                         # markdown::markdownToHTML(knit( 'Contact.Rmd', quiet = TRUE),fragment.only=TRUE), 
                                         # withMathJax(includeMarkdown( 'Contact.md' ))
                                         
                                         
                                ),
                                tabPanel("SEIR analysis and report", icon = icon("calculator"),
                                          #h2(tags$b("Factors that increase pollution in the Aburra Valley:")),
                                          
                                            
                                            # must turn shinyjs on--------------------------------Pass------
                                #off#       shinyjs::useShinyjs(),
                                            # add logout button UI 
                                #off#       div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                                            # add login panel UI function
                                #off#       shinyauthr::loginUI(id = "login"),#---------------------Pass--------
                                           
                                                    
                                withSpinner(uiOutput("SEIRCreditPage"),type=1) ,        
                                includeMarkdown("Contact.Rmd")
                               
                               )
                               )
                               
                               
                               #  style='padding-left:0px; padding-right:1px; padding-top:5px; padding-bottom:5px',
                              
                               
                             ))
                             

                    )
                )
        ),
        tabItem("about", includeMarkdown("About.Rmd"))
    )

)

 
# Completing the ui part with dashboardPage -------------------------------



#ui <- dashboardPage(title = 'Coronavirus', header, sidebar, body, skin='blue')

ui <- dashboardPagePlus(
    title = "Coronavirus",
    enable_preloader = TRUE,
    loading_duration = 2.1,
    header = header,
    sidebar = sidebar,
    sidebar_fullCollapse=T,
    body = body
     # -light
  )
 





