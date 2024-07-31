
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(data.table)
library(DT)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggpattern)
library(ggiraph)
library(COINr)
library(lubridate)
library(markdown)

source("R/read.R")
source("R/functions.R")

# Define UI for application that draws a histogram
bootstrapPage(
  tags$head(
    tags$style(HTML("
  #loadmessage {
  position:fixed; z-index:8; top:50%; left:50%; padding:10px;
  text-align:center; font-weight:bold; color:#000000; background-color:#CCFF66;
  }
  
  .loader {
  position:fixed; z-index:8; border:16px solid #999999;
  border-top: 16px solid #2e3436; border-radius: 50%;
  width: 80px; height: 80px; top:45%; left:45%;
  animation: spin 2s linear infinite;
  }

  .prevent_click{
  position:fixed; 
  z-index:9;
  width:100%;
  height:100vh;
  background-color: transparent;   
  }

  @keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
  }"))
  ),
  
  conditionalPanel(
    condition = "$(\'html\').hasClass(\'shiny-busy\')",
    tags$div(class = "loader"),
    tags$div(class = "prevent_click")
  ),
  # "#000000","#333333","#666666","#999999","#CCCCCC"
navbarPage(theme = shinytheme("spacelab"), "Draft Equity & Opportunity Indexes", id = "maintab",
           tags$head(tags$style(HTML('* {font-family: "Arial"};')),
                     tags$style(HTML('.pickerStyle .btn-default {background: #fff; color: #333; border-color: #ccc; text-shadow: none;}
                                                   .btn-default:focus, .btn-default.focus {color: #333; background-color: #fff; border-color: #ccc;}
                                                   .btn-default:hover {color: #333; background-color: #fff; border-color: #ccc;}
                                                   .btn-default:active, .btn-default.active, .open>.dropdown-toggle.btn-default {color: #333; background-color: #fff; border-color: #ccc;}
                                                   .btn-default:active:hover, .btn-default.active:hover, .open>.dropdown-toggle.btn-default:hover,
                                                   .btn-default:active:focus, .btn-default.active:focus, .open>.dropdown-toggle.btn-default:focus,
                                                   .btn-default:active.focus, .btn-default.active.focus, .open>.dropdown-toggle.btn-default.focus {color: #333; background-color: #fff; border-color: #ccc;}
                                                   .bootstrap-select .dropdown-toggle:focus, .bootstrap-select>select.mobile-device:focus+.dropdown-toggle {outline: 5px auto #fff!important; outline-offset: -2px}'))),
                     # tags$style(".pickerStyle .btn-default {background: #fff; color: #333;}
                     #                          .btn-default:active, .btn-default.active, .open>.dropdown-toggle.btn-default {background: #fff; color: #333;}")),
           
           #indexYearTitle,
           tabPanel("Welcome", value = "tab1",
                     
                   tags$style("#indexSchoolTitle, #indicatorSchoolTitle, #measureSchoolTitle, 
                              #indicatorYearTitle, #measureYearTitle,
                              #indicatorIndTitle, #measureIndTitle{
                              color: #656565;font-size: 28px; font-style: bold;
                              text-align: center}"),
                   tags$style("#measureSchoolTitle1, #measureSchoolTitle2, #measureCompYearTitle1, 
                              #measureCompYearTitle2,#measureCompIndTitle1, #measureCompIndTitle2{
                              color: #656565; font-size: 18px; font-style: italic;
                              text-align: center}"),
                   tags$style("#indexNarrative, #indicatorNarrative{
                              font-size: 17px; text-align: left; margin-bottom: 15px;}"),
                   tags$style("#measureNarrative{
                              font-size: 16px; text-align: justify; margin-bottom: 15px;}"),
                   tags$style("#indexNarrativeNote, #indicatorNarrativeNote{
                              font-size: 15px; text-align: left; font-style: italic;}"),
                   # tags$style(HTML(".tooltip > .tooltip-inner {color: black;background-color: #DBDBDB;}")),
                   tags$style(HTML(".tooltip {font-size: 1.4rem;}
                                   .tooltip-inner {color: black;background-color: #DBDBDB;}")),

                   mainPanel(
                     tags$h2("Welcome to the Draft Equity & Opportunity Indexes Dashboard!", align = 'center'),
                     br(),
                     includeMarkdown("www/aboutpage.Rmd"),
                     # br(),
                     # div(actionButton("enterbutton", "Enter Draft Dashboard", width = 200,style="background-image: none; background-color: #7b7b7b; border-color: #c9c9c9; color: white;"),align = 'center'),
                     br(),
                     # br(),
                     tags$h4("First, select a school:", align = 'center'),                     
                     # br(),
                     # br(),
                     # div(selectInput("initschoolcodeselect", "",
                     #             choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)), 
                     #             selected = NULL),align = 'center'),
                     tags$div(pickerInput("initschoolcodeselect", "",
                                          choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)), 
                                          selected = NULL,
                                          options = list(`live-search` = TRUE)), 
                              class = "pickerStyle",align = 'center'),
                     br(),
                     tags$h4("Then click on an index to begin exploring:",align='center'),
                     br(),
                     div(actionButton("cond",
                                      # HTML("Conditions:<br/>measure<br/>schools'<br/>baseline"),
                                      # style = "background-image: none; height: 220px; width: 220px; border-radius: 50%; 
                                      # background-color: #3ed1df; border-color: #3ed1df; color: white; font-size: 18px; font-weight: bold;}"
                                      HTML("Conditions"),
                                      style = "background-image: none; height: 180px; width: 180px; border-radius: 50%; 
                                      background-color: #3ed1df; border-color: #7b7b7b; color: white; font-size: 24px; font-weight: bold;}"),
                         bsTooltip(id="cond",title="<b>About this index:</b><br/>Awareness of the differential access to the community, environmental health, and education conditions from kindergarten through grade 12 can reduce group disparities."),
                         actionButton("res",
                                      # HTML("Resources:<br/>measure<br/>schools'<br/>investment"),
                                      # style = "background-image: none; height: 220px; width: 220px; border-radius: 50%; 
                                      # background-color: #fa8a11; border-color: #fa8a11; color: white; font-size: 18px; font-weight: bold;}"),
                                      HTML("Resources"),
                                      style = "background-image: none; height: 180px; width: 180px; border-radius: 50%; 
                                      background-color: #fa8a11; border-color: #7b7b7b; color: white; font-size: 24px; font-weight: bold;}"),
                         bsTooltip(id="res",title="<b>About this index:</b><br/>This description requires updating."),
                         actionButton("opp",
                                      # HTML("Experiences:<br/>measure<br/>schools'<br/>experiences"),
                                      # style = "background-image: none; height: 220px; width: 220px; border-radius: 50%; 
                                      # background-color: #0eb393; border-color: #0eb393; color: white; font-size: 18px; font-weight: bold;}"),
                                      HTML("Experiences"),
                                      style = "background-image: none; height: 180px; width: 180px; border-radius: 50%; 
                                      background-color: #0eb393; border-color: #7b7b7b; color: white; font-size: 24px; font-weight: bold;}"),
                         bsTooltip(id="opp",title="<b>About this index:</b><br/>Schools are a vital source of opportunities that can redue inequalities by monitoring engagement, curricular breadth, and school climate."),
                         actionButton("out",
                                      # HTML("Outcomes:<br/>measure<br/>schools'<br/>achievements"),
                                      # style = "opacity: .80; background-image: none; height: 220px; width: 220px; border-radius: 50%; 
                                      # background-color: #541690; border-color: #541690; color: white; font-size: 18px; font-weight: bold;}"
                                      HTML("Outcomes"),
                                      style = "opacity: .80; background-image: none; height: 180px; width: 180px; border-radius: 50%; 
                                      background-color: #541690; border-color: #7b7b7b; color: white; font-size: 24px; font-weight: bold;}"),
                         bsTooltip(id="out",title="<b>About this index:</b><br/>This description requires updating.")
                           , align = 'center'),
                     br(),
                     br(),
                     # div(actionButton("cond2",
                     #                  HTML(paste0(strwrap('Awareness of the differential access to the community, 
                     #                                      environmental health, and education conditions from kindergarten 
                     #                                      through grade 12 can reduce group disparities',width=25), collapse="</br>")),
                     #                  style = "background-image: none; height: 220px; width: 180px; border-radius: 1%;
                     #                  background-color: #D8F6F9; border-color: #3ed1df; color: grey; font-size: 14px;white-space:normal;}"
                     #                  ),
                     #     actionButton("res2",
                     #                  HTML("measure<br/>schools'<br/>investment"),
                     #                  style = "background-image: none; height: 220px; width: 180px; border-radius: 1%;
                     #                  background-color: #fcdcb8; border-color: #fa8a11; color: grey; font-size: 14px;}"
                     #                  ),
                     #     
                     #     actionButton("opp2",
                     #                  HTML(paste0(strwrap('Schools are a vital source of opportunities that can redue 
                     #                                      inequalities by monitoring engagement, curricular breadth, 
                     #                                      and school climate',width=26), collapse="</br>")),
                     #                  style = "background-image: none; height: 220px; width: 180px; border-radius: 1%;
                     #                  background-color: #9FE1D4; border-color: #0eb393; color: grey; font-size: 14px;}"
                     #                  ),
                     #     actionButton("out2",
                     #                  HTML("measure<br/>schools'<br/>achievements"),
                     #                  style = "opacity: .80; background-image: none; height: 220px; width: 180px; border-radius: 1%;
                     #                  background-color: #BBA2D3; border-color: #541690; color: grey; font-size: 14px;}"
                     #                  )
                     #     , align = 'center'),
                     # fluidRow(
                     #   column(width=4),
                     #   column(
                     #     tableOutput("indextable"), width = 3)),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     div(tags$a(img(src="HorizontalLogo_1_Gradient.png",width = "270px"), href="https://www.rilds.org"),
                         tags$a(img(src="ridelogo.png",width = "270px"), href="https://ride.ri.gov"), align = 'center'),
                     width = 12
                     )),
           # .btn-myColor {color: #666;background-color: #fff;border-color: #ccc;active-bg: #fa8a11;}
           #     .btn-myColor,.btn-myColor.focus {color: #ffffff;background-color: #541690;border-color: #18490d;}    
          tabPanel("Dashboard", value = "tab2",
                   sidebarPanel(
                     
                       conditionalPanel(condition = "input.tabid == 1||input.tabid == 2||input.tabid == 3||input.tabid == 4||input.tabid == 5",
                                                 tags$style('.btn-myColor {color: #ccc;background-color: #f9f9f9;border-color: #ccc}
                                                             .btn-myColor:active,.btn-myColor.active {color: #f9f9f9;background-color: #eaeaea;border-color: #ccc}
                                                             .shiny-input-checkboxgroup label~.shiny-options-group,.shiny-input-radiogroup label~.shiny-options-group {margin-left: 15px}
                                                             .btn-group-vertical {margin-left: 10px}'
                                                            ),
                                                 radioGroupButtons("indexselect", "Select an index:",
                                                              # choices = c("Conditions" = "conditions","Experiences" = "experiences",
                                                              #             "Resources" = "resources","Outcomes" = "outcomes"),
                                                              choiceNames = list(HTML("<b style='color:#3ed1df;font-size:14px;text-shadow: none;'>Conditions</b>"),
                                                                                 HTML("<b style='color:#fa8a11;font-size:14px;text-shadow: none;'>Resources</b>"),
                                                                                 HTML("<b style='color:#0eb393;font-size:14px;text-shadow: none;'>Experiences</b>"),
                                                                                 HTML("<b style='color:#541690;font-size:14px;text-shadow: none;'>Outcomes</b>")),
                                                              choiceValues = c("conditions","resources","experiences","outcomes"),
                                                              selected = "experiences",
                                                              direction="vertical",
                                                              # size="lg",
                                                              status = "myColor"
                                                              )
                                        ),
                       conditionalPanel(condition = "input.tabid == 1||input.tabid == 2||input.tabid == 3||input.tabid == 4||input.tabid == 5||input.tabid == 6||input.tabid == 8",
                                        # radioButtons("schoolyearselect", "Select a school year:",
                                        #              choices = c("2017-18" = 1, "2018-19" = 2, "2019-20" = 3, "2020-21" = 4),
                                        #              selected = 4)
                                        radioGroupButtons("schoolyearselect", "Select a school year:",
                                                          # choices = c("Conditions" = "conditions","Experiences" = "experiences",
                                                          #             "Resources" = "resources","Outcomes" = "outcomes"),
                                                          choiceNames = list(HTML("<b style='color:#656565;font-size:14px;text-shadow: none;'>&ensp;&ensp;2017-18&ensp;&ensp;</b>"),
                                                                             HTML("<b style='color:#656565;font-size:14px;text-shadow: none;'>&ensp;&ensp;2018-19&ensp;&ensp;</b>"),
                                                                             HTML("<b style='color:#656565;font-size:14px;text-shadow: none;'>&ensp;&ensp;2019-20&ensp;&ensp;</b>"),
                                                                             HTML("<b style='color:#656565;font-size:14px;text-shadow: none;'>&ensp;&ensp;2020-21&ensp;&ensp;</b>")),
                                                          choiceValues = c(1,2,3,4),
                                                          selected = 4,
                                                          direction="vertical",
                                                          # size="lg",
                                                          status = "myColor"
                                                          )
                                        ),
                                                 # radioButtons("indexselect", "Select an index:",
                                                 #              # choices = c("Conditions" = "conditions","Experiences" = "experiences",
                                                 #              #             "Resources" = "resources","Outcomes" = "outcomes"),
                                                 #              choiceNames = list(HTML("<b style='color:#3ed1df;font-size:16px;'>Conditions</b>"),HTML("<b style='color:#0eb393;font-size:16px;'>Experiences</b>"),
                                                 #                              HTML("<b style='color:#fa8a11;font-size:16px;'>Resources</b>"),HTML("<b style='color:#541690;font-size:16px;'>Outcomes</b>")),
                                                 #              choiceValues = c("conditions","experiences","resources","outcomes"),
                                                 #              selected = "experiences"),
                       conditionalPanel(condition = "input.tabid == 1||input.tabid == 2||input.tabid == 3||input.tabid == 4||input.tabid == 5||input.tabid == 6||input.tabid == 7||input.tabid == 8",
                                        # checkboxInput("charterselect", "Include charters", value = T),
                                        radioButtons("charterselect2", "Select a school type:",choices = c("All Schools"="all","Charters"="charters","Non-Charters"="noncharters"),selected = "all")),
                       conditionalPanel(condition = "input.tabid == 1||input.tabid == 2||input.tabid == 3||input.tabid == 4||input.tabid == 5||input.tabid == 6||input.tabid == 7",
                                                 # selectInput("schoolcodeselect", "Select a school:",
                                                 #             choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)), 
                                                 #             selected = NULL
                                                 #             # selected = "Scituate High School"
                                                 #             ),
                                                 tags$div(pickerInput("schoolcodeselect", "Select a school:",
                                                                      choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)), 
                                                                      selected = NULL,
                                                                      options = list(`live-search` = TRUE)), 
                                                          class = "pickerStyle")),

                                #conditionalPanel(condition = "input.tabid == 1||input.tabid == 2||input.tabid == 3||input.tabid == 4||input.tabid == 5||input.tabid == 6||input.tabid == 7",
                                                 #plotOutput("condplot", 
                                                 #           width = "150px",
                                                 #           height = "100px"
                                                            #)
                                                 #),
                       conditionalPanel(condition = "input.tabid == 3||input.tabid == 4||input.tabid == 5",
                                                 selectInput("indicatorselect", "Select an indicator:",
                                                             choices = c("Engagement in Schooling" = "engage","School Climate" = "climate",
                                                                         "Curricular Breadth" = "currbrdth"),
                                                             # selected = "engage"
                                                             )
                                                 ),
                       conditionalPanel(condition = "input.tabid == 5",
                                                 tags$div(pickerInput("peerselect", "Select a Peer School:",
                                                                      choices = sort(unique(equity_purse$coin[[4]]$Meta$Unit$uName)),
                                                                      selected = NULL,
                                                                      options = list(`live-search` = TRUE)), 
                                                          class = "pickerStyle"),
                                                 # materialSwitch("plotBtn", "Toggle Descriptions"),
                                                 # actionButton('plotBtn', 'Toggle Descriptions', "data-toggle"='collapse', "data-target"='#plotDiv',
                                                 #                         style="background-image: none; background-color: #7b7b7b; border-color: #c9c9c9; color: white; align: center"
                                                 #              )
                                                 ),
                       conditionalPanel(condition = "input.tabid == 6",
                                                 # selectizeInput("multiviewselect", "Select up to 4 indicators:",
                                                 #             choices = c("Student Inst. Support Needs" = "stinspnd", "Engagement" = "engage", "Community Env. Health" = "commenvhlth"), 
                                                 #             multiple = TRUE,
                                                 #             options = list(maxItems = 4,
                                                 #                            placeholder = "Click here to select indicators.")),
                                                 # br(),
                                                 h5(tags$b("Use the dropdown above each plot to select an indicator."), style = "color: #656565; font-size: 15px;"),
   
                                ),
                       conditionalPanel(condition = "input.tabid == 7",
                                                 # selectizeInput("multiview2select", "Select up to 4 years:",
                                                 #                choices = c("2017-18" = 1, "2018-19" = 2, "2019-20" = 3, "2020-21" = 4), 
                                                 #                multiple = TRUE,
                                                 #                options = list(maxItems = 4,
                                                 #                               placeholder = "Click here to select school years.")),
                                                 
                                                 # selectInput("multiview3select", "Select an indicator:",
                                                 #             choices = c("Student Inst. Support Needs" = "stinspnd", "Engagement" = "engage", 
                                                 #                         "Community Env. Health" = "commenvhlth", "School Climate" = "climate",
                                                 #                         "Curricular Breadth" = "currbrdth","Teacher Self-Efficacy"="tchefficacy",
                                                 #                         "Leadership"="leadership","Professional Learning"="proflearn",
                                                 #                         "FTE per Pupil"="fteperpupil"
                                                 #                         ), 
                                                 #             selected = "stinspnd"),
                                                 tags$div(pickerInput("multiview3select", "Select an indicator:",
                                                             choices = c("Student Inst. Support Needs" = "stinspnd","Community Env. Health" = "commenvhlth",
                                                                         "Teacher Self-Efficacy"="tchefficacy","Leadership"="leadership","Professional Learning"="proflearn",
                                                                         "FTE per Pupil"="fteperpupil","Per Pupil Spending" = "perpplspend",
                                                                         "Engagement in Schooling" = "engage","Curricular Breadth" = "currbrdth","School Climate" = "climate",
                                                                         "College & Career"="collegecareer"
                                                             ), 
                                                             selected = "stinspnd",
                                                             options = list(`live-search` = TRUE),
                                                             choicesOpt = list(
                                                               style = c("background: #8BE3EC;","background: #8BE3EC;",
                                                                         "background: #fab669;","background: #fab669;","background: #fab669;",
                                                                         "background: #fab669;","background: #fab669;",
                                                                         "background: #6ED1BE;","background: #6ED1BE;","background: #6ED1BE;",
                                                                         "background: #9873BC;"))), 
                                                          class = "pickerStyle"),
                                                 # br(),
                                                 h5(tags$b("Use the dropdown above each plot to select a year."), style = "color: #656565; font-size: 15px;"),
                                                 # div(actionButton('genMultiview', 'Generate MultiView'), align = 'center')
                                ),
                                width = 2),
                   mainPanel(
                          tabsetPanel(id = "tabid",
                             tabPanel("Index", value = 2,
                                     tags$br(),
                                     textOutput("indexNarrative"),
                                     # br(),
                                     htmlOutput("indexNarrativeNote"),
                                     br(),
                                     #textOutput("indexNarrative"),
                                     #br(),
                                     # textOutput("indexSchoolTitle"),
                                     # div(style = "color: #656565; font-size: 24px; font-style: italic; text-align: center","Index Ranking, "),
                                     # h6(tags$i("Index Ranking, "),
                                     #    tags$i(textOutput("indexYearTitle",inline = T)), align = "center",style = "color: #656565; font-size: 18px;"),
                                     girafeOutput("indexPlot", height = "80vh"),
                                     br(),
                                     br(),
                                     absolutePanel(
                                       top = 210, right = 20, 
                                       #width = 300,
                                       draggable = TRUE,
                                       plotOutput("condplot", 
                                                  width = "150px",
                                                  height = "150px"), style ="{background-color: rgba(0,0,255,0.2);;}#sel_date {background-color: rgba(0,0,255,1);}"
                                     ),
                                     ),
                            tabPanel("Indicator", value = 3,
                                     tags$br(),
                                     textOutput("indicatorNarrative"),
                                     # br(),
                                     htmlOutput("indicatorNarrativeNote"),
                                     br(),
                                     girafeOutput("rankPlot", height = "80vh"),
                                     br(),
                                     ),
                            tabPanel("Measures", value = 4,
                                     tags$br(),
                                     fluidRow(
                                       column(8,
                                              textOutput("measureNarrative"),
                                              # br(),
                                              ),
                                       column(8,
                                              girafeOutput("brkdwnPlot", height = "90vh"
                                                           # height = "84vh"
                                                           ),
                                              ),
                                       column(12,
                                              # h5(tags$i("test"),style="align: justify;")
                                       ),
                                       column(1,
                                              # h5(tags$i("test"),style="align: justify;")
                                       ),
                                       column(6,
                                              h5(tags$i("*Note: The normalization process is necessary to ensure that all Measures are on the same scale (0 to 100) before being averaged
                                                      to calculate the Indicator scores. For most Measures, a score of 0 would mean that the school had the lowest raw value for that Measure across all high schools in a given 
                                                      school year, while a score of 100 would mean that the school had the highest raw value for that Measure across all high schools in a given school year. There are certain Measures 
                                                      for which a lower raw value is more desirable, in which case the lowest raw value would become a 100 and the highest raw value would become a 0."),style="text-align: justify;")
                                              ),
                                     ),),
                            tabPanel("Peer Comparison", value = 5,

                                     fluidRow(
                                       column(6,
                                              tags$br(),
                                              uiOutput("yourSchTitle"),
                                              girafeOutput("brkdwnPlotA", height = "65vh",)
                                       ),
                                       column(6,
                                              tags$br(),
                                              tags$h4("Peer School:",style = "color: #6a6a6a; text-align: center; font-weight: bold; font-size: 24px;"),
                                              girafeOutput("brkdwnPlotB", height = "65vh",)
                                       ),
                                       column(12,
                                              tags$br(),
                                              tags$hr(style="border-color: #656565;"),
                                              girafeOutput("rankplotpeer", height = "55vh"),
                                              ),
                                       tags$h4("Note: The selected peer school is highlighted in grey while the four closest peer schools by Conditions Index score are outlined in blue.", 
                                               align = 'center'
                                               ),
                                       # absolutePanel(
                                       #   top = 110, left = "45%", draggable = TRUE, width = 500,
                                       #   tags$div(id = 'plotDiv',  class="collapse", style='background-color: #ffffff; opacity: .95;',
                                       #            tableOutput("measureTablePeer"))),
                                       )
                                     ),
                            tabPanel("Measure Comparison", value = 6,

                                     fluidRow(
                                                        column(6,
                                                               tags$br(),
                                                               # h4(tags$b("(Plot 1)"),style = "color: #656565; font-size: 16px;", align = "center"),
                                                               # div(selectInput("multiIndselect1", "Select Indicator 1:",
                                                               #             choices = c("Student Inst. Support Needs" = "stinspnd", "Engagement" = "engage", 
                                                               #                         "Community Env. Health" = "commenvhlth", "School Climate" = "climate",
                                                               #                         "Curricular Breadth" = "currbrdth","Teacher Self-Efficacy"="tchefficacy",
                                                               #                         "Leadership"="leadership","Professional Learning"="proflearn",
                                                               #                         "FTE per Pupil"="fteperpupil"
                                                               #                         ),
                                                               #             selected = "stinspnd"),align = 'center'),
                                                               tags$div(pickerInput("multiIndselect1", "Select Indicator 1:",
                                                                                    choices = c("Student Inst. Support Needs" = "stinspnd","Community Env. Health" = "commenvhlth",
                                                                                                "Teacher Self-Efficacy"="tchefficacy","Leadership"="leadership","Professional Learning"="proflearn",
                                                                                                "FTE per Pupil"="fteperpupil","Per Pupil Spending" = "perpplspend", 
                                                                                                "Engagement in Schooling" = "engage","Curricular Breadth" = "currbrdth","School Climate" = "climate",
                                                                                                "College & Career"="collegecareer"
                                                                                    ), 
                                                                                    selected = "stinspnd",
                                                                                    options = list(`live-search` = TRUE),
                                                                                    choicesOpt = list(
                                                                                      style = c("background: #8BE3EC;","background: #8BE3EC;",
                                                                                                "background: #fab669;","background: #fab669;","background: #fab669;",
                                                                                                "background: #fab669;","background: #fab669;",
                                                                                                "background: #6ED1BE;","background: #6ED1BE;","background: #6ED1BE;",
                                                                                                "background: #9873BC;"))), 
                                                                        class = "pickerStyle",align = 'center'),
                                                               girafeOutput("multiViewA", height = "65vh",)
                                                        ),

                                                        column(6,
                                                               tags$br(),
                                                               # h4(tags$b("(Plot 2)"),style = "color: #656565; font-size: 16px;", align = "center"),
                                                               # div(selectInput("multiIndselect2", "Select Indicator 2:",
                                                               #             choices = c("Student Inst. Support Needs" = "stinspnd", "Engagement" = "engage", 
                                                               #                         "Community Env. Health" = "commenvhlth", "School Climate" = "climate",
                                                               #                         "Curricular Breadth" = "currbrdth","Teacher Self-Efficacy"="tchefficacy",
                                                               #                         "Leadership"="leadership","Professional Learning"="proflearn",
                                                               #                         "FTE per Pupil"="fteperpupil"
                                                               #                         ),
                                                               #             selected = "engage"),align = 'center'),
                                                               tags$div(pickerInput("multiIndselect2", "Select Indicator 2:",
                                                                                    choices = c("Student Inst. Support Needs" = "stinspnd","Community Env. Health" = "commenvhlth",
                                                                                                "Teacher Self-Efficacy"="tchefficacy","Leadership"="leadership","Professional Learning"="proflearn",
                                                                                                "FTE per Pupil"="fteperpupil","Per Pupil Spending" = "perpplspend", 
                                                                                                "Engagement in Schooling" = "engage","Curricular Breadth" = "currbrdth","School Climate" = "climate",
                                                                                                "College & Career"="collegecareer"
                                                                                    ), 
                                                                                    selected = "engage",
                                                                                    options = list(`live-search` = TRUE),
                                                                                    choicesOpt = list(
                                                                                      style = c("background: #8BE3EC;","background: #8BE3EC;",
                                                                                                "background: #fab669;","background: #fab669;","background: #fab669;",
                                                                                                "background: #fab669;","background: #fab669;",
                                                                                                "background: #6ED1BE;","background: #6ED1BE;","background: #6ED1BE;",
                                                                                                "background: #9873BC;"))), 
                                                                        class = "pickerStyle",align = 'center'),
                                                               girafeOutput("multiViewB", height = "65vh",)
                                                        ),

                                                        column(6,
                                                               # tags$br(),
                                                               # h4(tags$b("(Plot 3)"),style = "color: #656565; font-size: 16px;", align = "center"),
                                                               # div(selectInput("multiIndselect3", "Select Indicator 3:",
                                                               #             choices = c("Student Inst. Support Needs" = "stinspnd", "Engagement" = "engage", 
                                                               #                         "Community Env. Health" = "commenvhlth", "School Climate" = "climate",
                                                               #                         "Curricular Breadth" = "currbrdth","Teacher Self-Efficacy"="tchefficacy",
                                                               #                         "Leadership"="leadership","Professional Learning"="proflearn",
                                                               #                         "FTE per Pupil"="fteperpupil"
                                                               #                         ),
                                                               #             selected = "commenvhlth"),align = 'center'),
                                                               tags$div(pickerInput("multiIndselect3", "Select Indicator 3:",
                                                                                    choices = c("Student Inst. Support Needs" = "stinspnd","Community Env. Health" = "commenvhlth",
                                                                                                "Teacher Self-Efficacy"="tchefficacy","Leadership"="leadership","Professional Learning"="proflearn",
                                                                                                "FTE per Pupil"="fteperpupil","Per Pupil Spending" = "perpplspend",
                                                                                                "Engagement in Schooling" = "engage","Curricular Breadth" = "currbrdth","School Climate" = "climate",
                                                                                                "College & Career"="collegecareer"
                                                                                    ), 
                                                                                    selected = "commenvhlth",
                                                                                    options = list(`live-search` = TRUE),
                                                                                    choicesOpt = list(
                                                                                      style = c("background: #8BE3EC;","background: #8BE3EC;",
                                                                                                "background: #fab669;","background: #fab669;","background: #fab669;",
                                                                                                "background: #fab669;","background: #fab669;",
                                                                                                "background: #6ED1BE;","background: #6ED1BE;","background: #6ED1BE;",
                                                                                                "background: #9873BC;"))), 
                                                                        class = "pickerStyle",align = 'center'),
                                                               girafeOutput("multiViewC", height = "65vh",)
                                                        ),

                                                        column(6,
                                                               # tags$br(),
                                                               # h4(tags$b("(Plot 4)"),style = "color: #656565; font-size: 16px;", align = "center"),
                                                               # div(selectInput("multiIndselect4", "Select Indicator 4:",
                                                               #             choices = c("Student Inst. Support Needs" = "stinspnd", "Engagement" = "engage", 
                                                               #                         "Community Env. Health" = "commenvhlth", "School Climate" = "climate",
                                                               #                         "Curricular Breadth" = "currbrdth","Teacher Self-Efficacy"="tchefficacy",
                                                               #                         "Leadership"="leadership","Professional Learning"="proflearn",
                                                               #                         "FTE per Pupil"="fteperpupil"
                                                               #                         ),
                                                               #             selected = "climate"),align = 'center'),
                                                               tags$div(pickerInput("multiIndselect4", "Select Indicator 4:",
                                                                                    choices = c("Student Inst. Support Needs" = "stinspnd","Community Env. Health" = "commenvhlth",
                                                                                                "Teacher Self-Efficacy"="tchefficacy","Leadership"="leadership","Professional Learning"="proflearn",
                                                                                                "FTE per Pupil"="fteperpupil","Per Pupil Spending" = "perpplspend",
                                                                                                "Engagement in Schooling" = "engage","Curricular Breadth" = "currbrdth","School Climate" = "climate",
                                                                                                "College & Career"="collegecareer"
                                                                                    ), 
                                                                                    selected = "climate",
                                                                                    options = list(`live-search` = TRUE),
                                                                                    choicesOpt = list(
                                                                                      style = c("background: #8BE3EC;","background: #8BE3EC;",
                                                                                                "background: #fab669;","background: #fab669;","background: #fab669;",
                                                                                                "background: #fab669;","background: #fab669;",
                                                                                                "background: #6ED1BE;","background: #6ED1BE;","background: #6ED1BE;",
                                                                                                "background: #9873BC;"))), 
                                                                        class = "pickerStyle",align = 'center'),
                                                               girafeOutput("multiViewD", height = "65vh",)
                                                        ),
                                     )
                            ),
                            tabPanel("Year Comparison", value = 7,
                                     
                                     fluidRow(
                                                        column(6,
                                                               tags$br(),
                                                               # h4(tags$b("(Plot 1)"),style = "color: #656565; font-size: 16px;", align = "center"),
                                                               div(selectInput("multiYrselect1", "Select Year 1:",
                                                                               choices = c("2017-18" = 1, "2018-19" = 2, "2019-20" = 3, "2020-21" = 4),
                                                                               selected = 1),align = 'center'),
                                                               girafeOutput("multiView2A", height = "65vh",)
                                                        ),
                                                        column(6,
                                                               tags$br(),
                                                               # h4(tags$b("(Plot 2)"),style = "color: #656565; font-size: 16px;", align = "center"),
                                                               div(selectInput("multiYrselect2", "Select Year 2:",
                                                                               choices = c("2017-18" = 1, "2018-19" = 2, "2019-20" = 3, "2020-21" = 4),
                                                                               selected = 2),align = 'center'),
                                                               girafeOutput("multiView2B", height = "65vh",)
                                                        ),
                                                        column(6,
                                                               # tags$br(),
                                                               # h4(tags$b("(Plot 3)"),style = "color: #656565; font-size: 15px;", align = "center"),
                                                               div(selectInput("multiYrselect3", "Select Year 3:",
                                                                               choices = c("2017-18" = 1, "2018-19" = 2, "2019-20" = 3, "2020-21" = 4),
                                                                               selected = 3),align = 'center'),
                                                               girafeOutput("multiView2C", height = "65vh",)
                                                        ),
                                                        column(6,
                                                               # tags$br(),
                                                               # h4(tags$b("(Plot 4)"),style = "color: #656565; font-size: 15px;", align = "center"),
                                                               div(selectInput("multiYrselect4", "Select Year 4:",
                                                                               choices = c("2017-18" = 1, "2018-19" = 2, "2019-20" = 3, "2020-21" = 4),
                                                                               selected = 4),align = 'center'),
                                                               girafeOutput("multiView2D", height = "65vh",)
                                                        ),
                                     )
                            ),
                            tabPanel("Index Table", value = 8,
                                     tags$br(),
                                     tags$h4("This table is a feature we are testing as a potential addition to the dashboard."),
                                     fluidRow(
                                       column(7,
                                              dataTableOutput("aggtable")
                                              )
                                       )
                                     )
                            ), width = 10
                          )
              ),
          tabPanel("About", value = "tab3"," ",
                   tags$h4("This page is under construction."),
                   HTML("<p>Functionality of this page will include a list of upcoming indicators, a link the the full data dictionary, a framework visualization, and a download option for the detailed methodology.</p>"),
                   br(),
                   tags$h4("Framework:"),
                   plotOutput("framework", height = "300vh", width = "100vh"),
                   # tags$h4("Link to data dictionary:"),
                   # HTML("<a>https://docs.google.com/spreadsheets/d/17n2SYHog2KZcNSLMBl_4eRCMF4UdrAJAUzMuhf8yxMw/edit#gid=0</a>"),
                   # HTML("<p>(Data dictionary not yet available for use.)</p>"),
                   # br(),
                   # br(),
                   tags$h4("Acknowledgments:"),
                   tags$em("This project was developed in part  using the COINr Package in R. Special thanks to William Becker for developing such an amazing tool!"),
                   br(),
                   br(),
                   br(),
                   )
          ))

