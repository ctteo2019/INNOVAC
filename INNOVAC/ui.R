library(shiny)
library(shinythemes)
library(tidyverse)
library(tmap)
library(sf)
library(countrycode)
library(plotly)
library(CGPfunctions)
library(gganimate)
library(gifski)
library(png)
library(magick)
library(corrplot)
library(ggstatsplot)
library(heatmaply)
library(rstantools)

data <- read_csv("data/gii_dataset_2015-2020_wide_score.csv")

years <- data %>%
  select(Year) %>%
  unique() %>%
  as.matrix()

cty <- data %>%
  select(Country) %>%
  unique() %>%
  as.matrix()

cty_ranked <- data %>%
  arrange(desc(`GLOBAL INNOVATION INDEX`)) %>%
  select(Country) %>%
  unique() %>%
  as.matrix()

indicators <- data %>%
  pivot_longer(cols = !Country:Category,
               names_to = "Indicator",
               values_to = "Score") %>%
  select(Indicator) %>%
  arrange(Indicator) %>%
  unique() %>%
  as.matrix()


shinyUI(navbarPage(theme = shinytheme("darkly"),"INNOVAC",
                   
                   
                   
    tabPanel("Home",
             
             sidebarPanel(
                 h4("A Project by ISSS608 Team 7 (Year 20/21 Term 2)"),
                 p("Team Members: Elaine Lee, Teo Chye Teck Lance"),
                 br()
                 
             ),
             
             mainPanel(
                 h1(strong("INNOV"), "ation ", strong("A"), "midst ", strong("C"), "ovid-19"),
                 br(),
                 
                 img(src = 'https://social-innovation.hitachi/-/media/project/hitachi/sib/en-us/think-ahead/manufacturing/fighting-covid-19-with-analytics/images/herobanner.ashx?la=en-US&upd=20200813084112Z&hash=A97F68C53694252013B2392B28FA1F61',
                     width = "100%"),
                 
                 br(),
                 br(),
                 
                 p("As the global Covid-19 pandemic hits, the theme of the Global Innovation Index (GII) 2020 is 
                 “Who will Finance Innovation,” which presents the current state and evolution of financial support 
                 mechanisms while exploring needed advances and remaining challenges. The GII model includes 131 
                 countries/economies, which represent 93.5% of the world’s population. The GII relies on two sub-indices 
                 – the Innovation Input Sub-Index and the Innovation Output Sub-Index."),
                 
                 p("Our project aims to analyse and identify patterns regarding the GII during Covid-19 pandemic. 
                   We intend to draw conclusions from the data and generate visualization of the data for the respective
                   countries or regions especially in Singapore.")
                 
             )
             
             ),
                   
                   
    navbarMenu("Exploratory Data Analysis",
               
               
               
               tabPanel("Choropleth",
                        
                        titlePanel("Choropleth Map", "EDA > Choropleth Map"),
                        
                        sidebarPanel(
                            selectInput(inputId = "year",
                                        label = "Year",
                                        choices = years,
                                        selected = 2020),
                            
                            selectInput(inputId = "indicator",
                                        label = "Indicator",
                                        choices = indicators,
                                        selected = "GLOBAL INNOVATION INDEX")
                            ),
                        
                        mainPanel( tmapOutput("map", height = 800) )
                        
                        ),
               
               
               tabPanel("Radar Chart",
                        
                        titlePanel("Radar Chart", "EDA > Radar Chart"),
                        
                        sidebarPanel(
                            selectInput(inputId = "year_2",
                                        label = "Year",
                                        choices = years,
                                        selected = 2020),
                            
                            selectInput(inputId = "indicator_2",
                                        label = "Indicator",
                                        choices = indicators,
                                        multiple = TRUE,
                                        selected = c("1. Institutions", 
                                                     "2. Human capital and research",
                                                     "3. Infrastructure",
                                                     "4. Market sophistication",
                                                     "5. Business sophistication",
                                                     "6. Knowledge and technology outputs",
                                                     "7. Creative outputs")),
                            
                            selectInput(inputId = "cty",
                                        label = "Country",
                                        choices = cty,
                                        multiple = TRUE,
                                        selected = cty_ranked[1:10])
                        ),
                        
                        mainPanel( plotlyOutput("radar", height = 800) )
                        
                        ),
               
               
               tabPanel("Slope Graph", 
                        
                        titlePanel("Slope Graph", "EDA > Slope Graph"),
                        
                        sidebarPanel(
                            selectInput(inputId = "year_3",
                                        label = "Year",
                                        choices = years,
                                        multiple = TRUE,
                                        selected = years),
                            
                            selectInput(inputId = "indicator_3",
                                        label = "Indicator",
                                        choices = indicators,
                                        selected = "GLOBAL INNOVATION INDEX"),
                            
                            selectInput(inputId = "cty_2",
                                        label = "Country",
                                        choices = cty,
                                        multiple = TRUE,
                                        selected = cty_ranked[1:20])
                        ),
                        
                        mainPanel( plotOutput("slope", height = 800) )
                        
                        ),
               
               
               tabPanel("Bubble Plot", 
                        
                        titlePanel("Animated Bubble Plot", "EDA > Bubble Plot"),
                        
                        sidebarPanel(
                            
                            p("The initial plot will take around 20 seconds to load. Thank you for your patience."),
                            
                            br(),

                            selectInput(inputId = "indicator_4_x",
                                        label = "X Axis",
                                        choices = indicators,
                                        selected = "2.3.2. Gross expenditure on R&D (GERD)"),
                            
                            selectInput(inputId = "indicator_4_y",
                                        label = "Y Axis",
                                        choices = indicators,
                                        selected = "1.3.1. Ease of starting a business"),
                            
                            selectInput(inputId = "indicator_4_sz",
                                        label = "Size",
                                        choices = indicators,
                                        selected = "GLOBAL INNOVATION INDEX"),
                            
                            selectInput(inputId = "cty_2",
                                        label = "Country",
                                        choices = cty,
                                        multiple = TRUE,
                                        selected = cty_ranked[1:20]),
                            
                            actionButton("submit_bubble", "Submit"),
                            
                            br(),
                            
                            p("Change the parameters and click on ", strong("Submit"), "button above to load new plot (around 20 seconds required).")
                        ),
                        
                        mainPanel( imageOutput("bubble", height = 800) )
                        )
               
               ),
    
    navbarMenu("Statistical Analysis",
               
               tabPanel("Correlation Analysis", 
                        
                        titlePanel("Correlation Analysis", "EDA > Correlation Analysis"),
                        
                        sidebarPanel(
                            selectInput(inputId = "year_5",
                                        label = "Year",
                                        choices = years,
                                        multiple = TRUE,
                                        selected = years),
                            
                            selectInput(inputId = "indicator_5",
                                        label = "Indicator",
                                        choices = indicators,
                                        multiple = TRUE,
                                        selected = c("GLOBAL INNOVATION INDEX",
                                                     "1. Institutions", 
                                                     "2. Human capital and research",
                                                     "3. Infrastructure",
                                                     "4. Market sophistication",
                                                     "5. Business sophistication",
                                                     "6. Knowledge and technology outputs",
                                                     "7. Creative outputs")),
                            
                            selectInput(inputId = "cty_5",
                                        label = "Country",
                                        choices = cty,
                                        multiple = TRUE,
                                        selected = cty_ranked[1:10]),
                            
                            tags$hr(),
                            
                            selectInput("corMethod", "Correlation Method",
                                        eval(formals(cor)$method)),
                            selectInput("corUse", "NA Action",
                                        c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")),
                            tags$hr(),
                            

                            selectInput("plotMethod", "Plot Method",
                                        list(all = eval(formals(corrplot)$method)), "circle"),

                            
                            selectInput("plotOrder", "Reorder Correlation",
                                        eval(formals(corrplot)$order)),
                            conditionalPanel("input.plotOrder === 'hclust'",
                                             wellPanel(
                                                 selectInput("plotHclustMethod", "Method",
                                                             eval(formals(corrplot)$hclust.method)),
                                                 numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA)))
                            
                            ),
                        
                        mainPanel( plotOutput("correlation", height = 800) )
                        
                        ),
               
               
               tabPanel("Statistical Plot",
                        
                        titlePanel("Statistical Plot", "EDA > Statistical Plot"),
                        
                        sidebarPanel(
                            selectInput(inputId = "year_6",
                                        label = "Year",
                                        choices = years,
                                        multiple = TRUE,
                                        selected = years),
                            
                            selectInput(inputId = "indicator_6",
                                        label = "Indicator",
                                        choices = indicators,
                                        selected = "GLOBAL INNOVATION INDEX")
                        
                        ),
                        
                        mainPanel( plotOutput("stats", height = 800))
               
               ),
               
               
               tabPanel("Hierarchical Clustering", 
                        
                        titlePanel("Hierarchical Clustering", "EDA > Hierarchical Clustering"),
                        
                        sidebarPanel(
                            selectInput(inputId = "year_7",
                                        label = "Year",
                                        choices = years,
                                        selected = 2020),
                            
                            selectInput(inputId = "indicator_7",
                                        label = "Indicator",
                                        choices = indicators,
                                        multiple = TRUE,
                                        selected = c("1. Institutions", 
                                                     "2. Human capital and research",
                                                     "3. Infrastructure",
                                                     "4. Market sophistication",
                                                     "5. Business sophistication",
                                                     "6. Knowledge and technology outputs",
                                                     "7. Creative outputs")),
                            
                            selectInput("distMethod", "Distance Method",
                                        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary","minkowski"),
                                        selected = "euclidean"),
                            
                            selectInput("hclusMethod", "H Clustering Method",
                                        choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid" ), 
                                        selected = "complete"),
                            
                            numericInput("krow", "K Rows", value = 3, min = 2, max = 10),
                            
                            numericInput("kcol", "K Columns", value = 2, min = 2, max = 10)
                            
                        ),
                        
                        mainPanel( plotlyOutput("hcluster", height = 3000) ))
               
               )
))
