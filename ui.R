library(shinydashboard)
library(shiny)
library(ggplot2)
suppressPackageStartupMessages(library(rtweet))
suppressPackageStartupMessages(library(ROAuth))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(wordcloud))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(SnowballC))
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library("syuzhet"))
library(plotly)
library(tidyr)
library(RCurl)
library(httr)

#Create the token
create_token(
    app = "natural disaster tweets",
    consumer_key = "q8xRvtqjGZ4W0pWB7wN7dVrjK",
    consumer_secret = "CFrT5gW3zrfLPQt2wAJtnW0mVXYLJ0WoK8E0HoNTP0ulbdKtld",
    access_token = "1279334721273520128-pNWAm3u65F4ZII02NOEXWQRslCWC10",
    access_secret = "mbwAy3yU6tC7OnU3bC7lyPMljckyoi2r0UN9WOs6OlZlz")

ui = dashboardPage(skin = "blue",
                   dashboardHeader(title = "Natural Disaster Analytics", titleWidth = 300),
                   dashboardSidebar(width = 300, sidebarMenu(style = "position: fixed; overflow: visible;",
                                                             menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
                                                             menuItem("About", tabName = "about", icon = icon("file-alt", lib="font-awesome")),
                                                                                                                          br(),
                                                             br(),
                                                             textInput("hashtag", "Search hashtag, word or phrase", value = " "),
                                                             sliderInput("searchnumber", "Number of searches for analysis", min = 200, max = 3000, value = 800, step= 200),
                                                             checkboxInput("retweets", "Include retweets", value = FALSE),
                                                             actionButton("save","Add")
                                                             
                                                             
                   )),
                   
                   dashboardBody(tags$script(HTML("$('body').addClass('fixed');")),
                                 tags$script(HTML("
                        var openTab = function(tabName){
                          $('a', $('.sidebar')).each(function() {
                            if(this.getAttribute('data-value') == tabName) {
                              this.click()
                            };
                          });
                        }
                      ")),tabItems(
                          tabItem(tabName = "dashboard", fluidRow(infoBoxOutput(width=3, "tweetloccount"), infoBoxOutput(width=3, "mostactive"), infoBoxOutput(width=3,"mostactivea"), infoBoxOutput(width=3, "mostprominent")),
                                  box(htmlOutput("summary5"), width = 800, height = 600),
                                  fluidRow(
                                      box(title = "Sentiment analysis of tweet usage", status = "primary", solidHeader = TRUE,
                                          collapsible = TRUE,plotlyOutput('sentanalysis')),
                                      box(title = "Word cloud showing most frequent words for sentiments", status = "primary", solidHeader = TRUE,
                                          collapsible = TRUE, plotOutput("wcloudgrouped"))),
                                  fluidRow(
                                      box(title = "Word cloud of generally frequent words in tweet text", status = "primary", solidHeader = TRUE,
                                          collapsible = TRUE, plotOutput('singlewcloud')),
                                      box(title = "Linechart of recent tweet frequency", status = "primary", solidHeader = TRUE,
                                          collapsible = TRUE, plotOutput("linechart")))
                          ),
                          tabItem(tabName="about", h1("About this dashboard"),
                                  p("Having read this article " ,span(a("Locating Natural Disasters through Social Media Feeds with R", href="https://towardsdatascience.com/locating-natural-disasters-through-social-media-feeds-with-r-7c8d3f078750")), 
                                    "this dashboard provides an interactive avenue for individuals to track natural disasters around the world. On startup, the dashboard contains filler data visualizations as place holders. However, twitter data retrieved using the Twitter api is cleaned and related to a given search hashtag or word, is cleaned, and plotted over a google map. This allows users to locate clusters of areas where reactions to that given hashtag is greatest. A sentiment analysis and word cloud visualization also gives a general idea of the aggregate feelings expressed through tweets related to that search. Finally, the change in frequency of that search item over the past couple of days is displayed on a line graph.  "),
                                  p("This dashboard is a member of a series of data visualization applications built by this same author(using Rshiny). Amongst this series is the ", span(a("GDP index data visualization dashboard", href="https://fabianokafor369.shinyapps.io/GDPapp/")),
                                    "which is an interactive platform for the observation of trends in GDP leading index data of US states. It was built using the Rshiny package, together with Rstudio packages such as googlevis, plotly and rtweet."),
                                  p(code("Disclaimer:"), "This app was created using the Twitter API, and its use should be restricted to its purpose of natural disaster information gathering and visualization. Examples of hastags and search items for natural disasters could include '#earthquake', 'hurricane', 'LAwildfire' etc. For more info on the Twitter API use terms and service, visit " , span(a("Twitter Developer Rules", href="https://developer.twitter.com/en/developer-terms/agreement-and-policy.html.")), ".")))
                   )
                   
)

