# Name: Vincent
# Student ID: 30052386
# Program: Summer Vacation Research Scholarship Program
# Project Title: Time Series Data Analytics and Visualization App
# Supervisors: Dr. Pooia Lalbakhsh, Prof. Ariel Liebman, and Dr. Hao Wang
# University: Monash University Clayton

#-------------------------------------------------------------------------------

# Import Libraries #

#-------------------------------------------------------------------------------

        #----- Additional Libraries / Packages only for Shiny App -----#

library(shiny) # For UI and Server interaction
library(shinydashboard) # For creating dashboard
library(shinythemes) # For theme
library(shinyWidgets) # For dropdown widget
library(DT) # For datatable output in the App

                    #----- Main Libraries / Packages -----#

library(tidyverse) # Preliminary tidying package
library(naniar) # Missing data package
library(plotly) # Interactive plot package
library(ggplot2) # For data Visualization
library(e1071) # Calculate skewness
library(lubridate) # For date datatype manipulation
library(caret) # Classification And Regression Training package
library(randomForest) # For fitting random forest model
library(earth) # For MARS Method (a.k.a. Multivariate Adaptive Regression Splines)
library(Boruta) # For Boruta Method
library(zoo) # For converting class dataframe object into class ts object (ts = time series)

#-------------------------------------------------------------------------------

# Data Reading and Preparation #

#-------------------------------------------------------------------------------

# Reading data and modify the ReadTime variable structure into multiple helper variables (i.e., splitting ReadTime into date and time variables then further splitting date and time variables into some variables)
pv <- as_tibble(read.csv(("./data/time_series_df.csv"))) %>% 
  separate(ReadTime, into = c("PVdate", "PVtime"), sep = "\\s", extra = "merge", remove = FALSE) %>% 
  separate(PVdate, into = c("dayofmonth", "month", "year"), sep = "/", extra = "merge", remove = FALSE) %>% 
  separate(PVtime, into = c("timefactor", "AMorPM"), sep = "\\s", extra = "merge", remove = FALSE)

# pivot_longer all variables except the helper variables (i.e., observed variables)
pv_data <- pv %>% 
  pivot_longer(cols = !c("ReadTime", "PVdate", "dayofmonth", "month", "year", "timefactor", "AMorPM", "PVtime"),
               names_to = "observed_variables",
               values_to = "observed_values")

# Convert PVDate variable into Date data type -> for date filter in server environment of the RShiny App
pv_data$PVdate <- as.Date(pv_data$PVdate, "%d/%m/%Y") # All pages except trend page
pv$PVdate <- as.Date(pv$PVdate, "%d/%m/%Y") # Trend Page

# Create raw data table -> for the Home page interactive table
pv_raw <- pv_data %>% 
  dplyr::select(c(ReadTime, observed_variables, observed_values))

# Create small_df for the Missing Value page's plot 5 (because we need to use varSelectInput() for 
# gg_miss_fct() function in the server environment to filter by factor)
small_df <- pv %>% 
  dplyr::select(c(dayofmonth, month, year, AMorPM, PVtime))

# Create selection_df for the feature selection part
selection_df <- as_tibble(read.csv(("./data/selection_df.csv"))) %>%
  dplyr::select(!ReadTime)

#-------------------------------------------------------------------------------

# Web UI layout #

#-------------------------------------------------------------------------------

# Create header
header <- dashboardHeader( 
  title = HTML("Time Series Data Analytics and Visualization App"),
  titleWidth = 700,
  disable = FALSE
)

# Create sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Home", tabName = "dash1", icon = icon("home")),
    menuItem("Global View", tabName = "dash2", icon = icon("globe-americas")),
    menuItem("Analysis", tabName = "dash3", icon = icon("chart-bar")),
    menuItem("About", tabName = "dash4", icon = icon("question"))
  )
)

# Create body page for each sidebar item
body <- dashboardBody(
  
#-----------------------------Dash1 (Home)--------------------------------------
  tabItems(
    tabItem(
      tabName = "dash1",
      fluidPage(
        theme = shinytheme("flatly"),
        fluidRow(
          column(width = 1),
          column(width = 2, div(tags$img(src="monash.png", height="100px", width="135px", alt="Something went wrong", deleteFile=FALSE), 
                                style="text-align: center;")),
          column(width = 8, 
                 h1(strong("Time Series Data Analytics and Visualization App"), style="text-align:center")),
          column(width = 1)
          ),
        br(),
        fluidRow(
          column(width = 1),
          column(width = 10,
                 box(width = 3,
                     p(strong("Project Focus")),
                     p(strong("Developer")),
                     p(strong("University Name")),
                     p(strong("Program")),
                     p(strong("Contact Number")),
                     p(strong("Email")),
                     p(strong("Supervisors")),
                     background = "light-blue"),
                 box(width = 9,
                     p("AI and Optimisation Empowered Net Zero Transition - Monash Campus as A Case Study"),
                     p("Vincent"),
                     p("Monash University Clayton"),
                     p("Summer Vacation Research Scholarship Program"),
                     p("0466598833"),
                     p(a(href="mailto:vinc0003@student.monash.edu", "vinc0003@student.monash.edu")),
                     p("Dr. Pooia Lalbakhsh, Prof. Ariel Liebman, and Dr. Hao Wang"))),
          column(width = 1)
        ),
        # Database + icon
        h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
        # Database (using pv_raw in the server environment)
        fluidRow(
          column(width = 1),
          column(width = 10,
                 dataTableOutput("RawData")),
          column(width = 1)
        ),
        # Next page (Global View)
        br(),
        br(),
        fluidRow(p(actionButton("link_to_gv", "Global View", icon = icon("globe-americas")), style="text-align:center"))
  )),
  
#-----------------------------Dash2 (Global View)-------------------------------
  tabItem(
    tabName = "dash2",
                        ###### Distribution Part ######
    fluidPage(
      box(
        box(strong(p("User Guide:")),
            p("User can manipulate the plot by changing the setting (i.e., filtered data) in the gear icon button. 
              By hovering the cursor to a plot, there is a tooltip's text that shows the details about a specific
              part of the plot. Finally, user can also hide part(s) of the plot(s) by clicking the legend label(s).",
              style="text-align:justify"),
            background = "light-blue", width = 12),
        br(),
        fluidRow(
          column(width = 5),
          column(width = 7,
                 box(strong(h4("Distribution", style="text-align:center")),
                     background = "light-blue", width = 4))
        ),
        br(),
        fluidRow(
          column(width = 1,
                 dropdown(tags$h3("Table & Graph Inputs"),
                          pickerInput(inputId = "gvD_observed_variables", # Filter variable
                                      label = "Observed Variables",
                                      choices = unique(pv_data$observed_variables),
                                      selected = unique(pv_data$observed_variables)[1],
                                      options = list(`actions-box` = TRUE),
                                      multiple = FALSE),
                          dateRangeInput(inputId = "gvD_dates", # Filter date
                                         label = "Date Range",
                                         start = as.character(min(pv_data$PVdate)),
                                         end = as.character(max(pv_data$PVdate)),
                                         format = "dd/mm/yy"),
                          pickerInput(inputId = "gvD_pvtime", # Filter time
                                      label = "Time",
                                      choices = unique(pv_data$PVtime),
                                      selected = unique(pv_data$PVtime),
                                      options = list(`actions-box` = TRUE),
                                      multiple = TRUE),
                          style = "unite", icon = icon("gear"), # Icon
                          status = "primary", width = "300px",
                          animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                   exit = animations$fading_exits$fadeOutLeftBig),
                          tooltip = tooltipOptions(title = "Input here!")
                 ))
        ),
        br(),
        fluidRow(
          column(width = 1,
                 dropdown(tags$h3("Table & Graph Inputs"),
                          pickerInput(inputId = "gvD_zero", # Filter zero value(s)
                                      label = "Remove Zero Observation(s)?",
                                      choices = c("Yes", "No"),
                                      selected = "No",
                                      options = list(`actions-box` = TRUE),
                                      multiple = FALSE),
                          pickerInput(inputId = "gvD_neg", # Filter negative value(s)
                                      label = "Remove Negative Observation(s)?",
                                      choices = c("Yes", "No"),
                                      selected = "Yes",
                                      options = list(`actions-box` = TRUE),
                                      multiple = FALSE),
                          pickerInput(inputId = "gvD_outlier", # Filter outlier(s)
                                      label = "Remove Outlier(s)?",
                                      choices = c("No", "Yes, Z-Score Approach", "Yes, Inter-Quartile Range (IQR) Approach"),
                                      selected = "No",
                                      options = list(`actions-box` = TRUE),
                                      multiple = FALSE),
                          pickerInput(inputId = "gvD_transformation", # Filter variable
                                      label = "Transformation Method",
                                      choices = c("Raw", "Square", "Cube", "Square Root",
                                                  "Cube Root", "Logarithm (Base 10)",
                                                  "Natural Logarithm"),
                                      selected = "Raw",
                                      options = list(`actions-box` = TRUE),
                                      multiple = FALSE),
                          style = "unite", icon = icon("gear"), # Icon
                          status = "danger", width = "300px",
                          animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                   exit = animations$fading_exits$fadeOutLeftBig),
                          tooltip = tooltipOptions(title = "Input here!")
                 ))
        ),
        fluidRow(
          box(h4(strong("Distribution Plot"), 
                 style="text-align:center"),
              # Output: Distribution of Density Plot
              plotlyOutput(outputId = "gvDplot1"),
              p(strong("Data source"),": Monash University"),
              width = 7,style="border:1px solid black"),
          box(background = "light-blue", 
              width = 5,
              column(width = 12,
                     p(strong("Variable Name: "),
                       textOutput("gvD_text1")),
                     p(strong("Number of Observations: "),
                       textOutput("gvD_text2")),
                     p(strong("Descriptive Statistics: "),
                       verbatimTextOutput("gvD_text3")),
                     p(strong("Standard Deviation: "),
                       textOutput("gvD_text4")),
                     p(strong("Skewness: "),
                       textOutput("gvD_text5")))
              )),
        fluidRow(
          box(h4(strong("Time Series Plot (Without Interactive Features)"), 
                 style="text-align:center"),
              # Output: Time Series Plot
              plotOutput(outputId = "gvDplot4"),
              p(strong("Data source"),": Monash University"),
              width = 12,style="border:1px solid black")
        ),
        # Go to Analysis Page -> Distribution Part
        fluidRow(p(actionButton("gv_to_distribution", "More Information", icon = icon("book")), style="text-align:center")),
        width = 12
      )
  ),
                        ###### Missing Value Part ######
  fluidPage(
    box(
      fluidRow(
        column(width = 5),
        column(width = 7,
               box(strong(h4("Missing Value", style="text-align:center")),
                   background = "light-blue", width = 4))
      ),
      br(),
      fluidRow(
        column(width = 1,
               dropdown(tags$h3("Table & Graph Inputs"),
                        pickerInput(inputId = "gvMV_observed_variables", # Filter variable
                                    label = "Observed Variables",
                                    choices = unique(pv_data$observed_variables),
                                    selected = head(unique(pv_data$observed_variables), 5),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE),
                        dateRangeInput(inputId = "gvMV_dates", # Filter date
                                       label = "Date Range",
                                       start = as.character(min(pv_data$PVdate)),
                                       end = as.character(max(pv_data$PVdate)),
                                       format = "dd/mm/yy"),
                        pickerInput(inputId = "gvMV_pvtime", # Filter time
                                    label = "Time",
                                    choices = unique(pv_data$PVtime),
                                    selected = unique(pv_data$PVtime),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE),
                        style = "unite", icon = icon("gear"), # Icon
                        status = "primary", width = "300px",
                        animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutLeftBig),
                        tooltip = tooltipOptions(title = "Input here!")
               ))
      ),
      fluidRow(
        column(width = 8, 
               h3(strong("Missing Value Table"), 
                  style="text-align:center"),
               dataTableOutput("gvMVdataset")),
        column(width = 4,
               br(),
               box(background = "light-blue", 
                   width = 12,
                   column(width = 12,
                          p(strong("Number of observations: "),
                            textOutput("gvMV_textA")),
                          p(strong("Total problematic rows: "),
                            textOutput("gvMV_textB")),
                          p(strong("Propotion of the problematic rows: "),
                            textOutput("gvMV_textC")),
                          p(strong("Number of cells: "),
                            textOutput("gvMV_text1")),
                          p(strong("Total missing by the number of cells: "),
                            textOutput("gvMV_text2")),
                          p(strong("Proportion of missing by the number of cells: "),
                            textOutput("gvMV_text3")))
               ))),
      fluidRow(
        box(h4(strong("Missing Values in Rows"), 
               style="text-align:center"),
            # Output: Stacked Bar-Chart Plot
            plotOutput(outputId = "gvplot3"),
            p(strong("Data source"),": Monash University"),
            width = 12,style="border:1px solid black")
      ),
      # Go to Analysis Page -> Missing Value Part
      fluidRow(p(actionButton("gv_to_missing", "More Information", icon = icon("book")), style="text-align:center")),
      width = 12
    )
  ),
                        ###### Outlier Part ######
  fluidPage(
    box(
      fluidRow(
        column(width = 5),
        column(width = 7,
               box(strong(h4("Outlier", style="text-align:center")),
                   background = "light-blue", width = 4))
      ),
      br(),
      fluidRow(
        column(width = 1,
               dropdown(tags$h3("Table & Graph Inputs"),
                        pickerInput(inputId = "gvO_observed_variables", # Filter variable
                                    label = "Observed Variables",
                                    choices = unique(pv_data$observed_variables),
                                    selected = unique(pv_data$observed_variables)[1],
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE),
                        dateRangeInput(inputId = "gvO_dates", # Filter date
                                       label = "Date Range",
                                       start = as.character(min(pv_data$PVdate)),
                                       end = as.character(max(pv_data$PVdate)),
                                       format = "dd/mm/yy"),
                        pickerInput(inputId = "gvO_pvtime", # Filter time
                                    label = "Time",
                                    choices = unique(pv_data$PVtime),
                                    selected = unique(pv_data$PVtime),
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE),
                        style = "unite", icon = icon("gear"), # Icon
                        status = "primary", width = "300px",
                        animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutLeftBig),
                        tooltip = tooltipOptions(title = "Input here!")
               ))
      ),
      tabsetPanel(
        id = "tabpanels_ogv",
        tabPanel("Z-Score Approach",
                 br(),
                 fluidRow(
                   # Potential Outlier(s) Table
                   column(width = 7,
                          h3(strong("Potential Outlier(s) Table"), 
                             style="text-align:center"),
                          dataTableOutput("gvOZdataset")),
                   # Descriptive stats, upper limit, and lower limit
                   column(width = 5,
                          br(),
                          box(background = "light-blue",
                              width = 12,
                              p(strong("Variable Name: "),
                                textOutput("gvO_varname1")),
                              p(strong("Descriptive Statistics: "),
                                verbatimTextOutput("gvO_descstat1")),
                              p(strong("Standard Deviation: "),
                                textOutput("gvO_std1")),
                              p(strong("Skewness before Removing Outliers: "),
                                textOutput("gvO_skewness1")),
                              p(strong("Upper Limit: mean + 3 * stdev"),
                                textOutput("gvO_upper1")),
                              p(strong("Lower Limit: mean - 3 * stdev"),
                                textOutput("gvO_lower1")))),
                 ),
                 br(),
                 p(strong("Before Removing Outlier(s)")),
                 fluidRow(
                   box(h4(strong("Density"), 
                          style="text-align:center"),
                       # Output: Density Plot (Before Removing Outlier(s))
                       plotlyOutput(outputId = "gvoplot1"),
                       p(strong("Data source"),": Monash University"),
                       width = 6,style="border:1px solid black"),
                   box(h4(strong("Boxplot"), 
                          style="text-align:center"),
                       # Output: Boxplot Plot (Before Removing Outlier(s))
                       plotOutput(outputId = "gvoplot2"),
                       p(strong("Data source"),": Monash University"),
                       width = 6,style="border:1px solid black")
                 )),
        tabPanel("Inter-Quartile Range (IQR) Approach",
                 br(),
                 fluidRow(
                   # Potential Outlier(s) Table
                   column(width = 7,
                          h3(strong("Potential Outlier(s) Table"), 
                             style="text-align:center"),
                          dataTableOutput("gvOIQRdataset")),
                   # Descriptive stats, upper limit, and lower limit
                   column(width = 5,
                          br(),
                          box(background = "light-blue",
                              width = 12,
                              p(strong("Variable Name: "),
                                textOutput("gvIQR_varname1")),
                              p(strong("Descriptive Statistics: "),
                                verbatimTextOutput("gvIQR_descstat1")),
                              p(strong("Inter-Quartile Range (IQR): "),
                                textOutput("gvIQR_iqr1")),
                              p(strong("Skewness before Removing Outliers: "),
                                textOutput("gvIQR_skewness1")),
                              p(strong("Upper Limit: Q3 + 1.5 * IQR"),
                                textOutput("gvIQR_upper1")),
                              p(strong("Lower Limit: Q1 - 1.5 * IQR"),
                                textOutput("gvIQR_lower1")))),
                 ),
                 br(),
                 p(strong("Before Removing Outlier(s)")),
                 fluidRow(
                   box(h4(strong("Density"), 
                          style="text-align:center"),
                       # Output: Density Plot (Before Removing Outlier(s))
                       plotlyOutput(outputId = "gviqrplot1"),
                       p(strong("Data source"),": Monash University"),
                       width = 6,style="border:1px solid black"),
                   box(h4(strong("Boxplot"), 
                          style="text-align:center"),
                       # Output: Boxplot Plot (Before Removing Outlier(s))
                       plotOutput(outputId = "gviqrplot2"),
                       p(strong("Data source"),": Monash University"),
                       width = 6,style="border:1px solid black")
                 )
        )),
      # Go to Analysis Page -> Outlier Part
      fluidRow(p(actionButton("gv_to_outlier", "More Information", icon = icon("book")), style="text-align:center")),
      width = 12
    )
  ),
                        ###### Feature Selection Part ######
  fluidPage(
    box(
      fluidRow(
        column(width = 5),
        column(width = 7,
               box(strong(h4("Feature Selection", style="text-align:center")),
                   background = "light-blue", width = 4))
      ),
      br(),
      fluidRow(
        column(width = 1,
               dropdown(tags$h3("Feature Selection Inputs"),
                        pickerInput(inputId = "gvFS_explanatory_variable", # Filter response variable
                                    label = "Explanatory Variable(s)",
                                    choices = colnames(selection_df)[2:ncol(selection_df)],
                                    selected = colnames(selection_df)[2:ncol(selection_df)],
                                    options = list(`actions-box` = TRUE),
                                    multiple = TRUE),
                        style = "unite", icon = icon("gear"), # Icon
                        status = "primary", width = "300px",
                        animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutLeftBig),
                        tooltip = tooltipOptions(title = "Input here!")
               ))
      ),
      tabsetPanel(
        id = "tabpanels_fs",
        tabPanel("Linear Regression",
                 fluidRow(
                   column(width = 12,
                          box(h4(strong("Variable Importance Plot"), 
                                 style="text-align:center"),
                              # Output: Variable Importance Plot
                              plotlyOutput(outputId = "gvfsplot1"),
                              p(strong("Data source"),": Monash University"),
                              width = 12,style="border:1px solid black"))
                 )
        ),
        tabPanel("Random Forest",
                 br(),
                 fluidRow(
                   column(width = 1,
                          dropdown(tags$h3("Random Forest Method Inputs"),
                                   pickerInput(inputId = "gvrffs_ntree", # Filter number of trees grown
                                               label = "Number of Trees Grown (ntree)",
                                               choices = c("10", "50", "100", "250", "500", "1000"),
                                               selected = "50",
                                               options = list(`actions-box` = TRUE),
                                               multiple = FALSE),
                                   pickerInput(inputId = "gvrffs_mtry", # Filter number of predictors sampled for spliting at each node
                                               label = "Number of Predictors Sampled for Spliting at each Node (mtry)",
                                               choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                               selected = "10",
                                               options = list(`actions-box` = TRUE),
                                               multiple = FALSE),
                                   style = "unite", icon = icon("gear"), # Icon
                                   status = "danger", width = "300px",
                                   animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                            exit = animations$fading_exits$fadeOutLeftBig),
                                   tooltip = tooltipOptions(title = "Input here!")
                          ))
                 ),
                 fluidRow(
                   column(width = 12,
                          box(h4(strong("Variable Importance Plot"), 
                                 style="text-align:center"),
                              # Output: Variable Importance Plot
                              plotlyOutput(outputId = "gvfsplot2"),
                              p(strong("Data source"),": Monash University"),
                              width = 12,style="border:1px solid black"))
                 )
        ),
        tabPanel("MARS",
                 fluidRow(
                   column(width = 12,
                          box(h4(strong("Variable Importance Plot"), 
                                 style="text-align:center"),
                              # Output: Variable Importance Plot
                              plotOutput(outputId = "gvfsplot3"),
                              p(strong("Data source"),": Monash University"),
                              width = 12,style="border:1px solid black"))
                 )
        ),
        tabPanel("Boruta",
                 fluidRow(
                   column(width = 12,
                          box(h4(strong("Variable Importance Plot"), 
                                 style="text-align:center"),
                              # Output: Variable Importance Plot
                              plotOutput(outputId = "gvfsplot4"),
                              p(strong("Data source"),": Monash University"),
                              width = 12,style="border:1px solid black"))
                 )
        )
      ),
      # Go to Analysis Page -> Feature Selection Part
      fluidRow(p(actionButton("gv_to_feature_selection", "More Information", icon = icon("book")), style="text-align:center")),
      width = 12
    )
  ),
                        ###### Trend Part ######
  fluidPage(
    box(
      fluidRow(
        column(width = 5),
        column(width = 7,
               box(strong(h4("Trend", style="text-align:center")),
                   background = "light-blue", width = 4))
      ),
      br(),
      fluidRow(
        column(width = 1,
               dropdown(tags$h3("Table & Graph Inputs"),
                        pickerInput(inputId = "gvtrend_observed_variables", # Filter variable
                                    label = "Observed Variables",
                                    choices = unique(pv_data$observed_variables),
                                    selected = unique(pv_data$observed_variables)[1],
                                    options = list(`actions-box` = TRUE),
                                    multiple = FALSE),
                        dateRangeInput(inputId = "gvtrend_dates", # Filter date
                                       label = "Date Range",
                                       start = as.character(min(pv$PVdate)),
                                       end = as.character(max(pv$PVdate)),
                                       format = "dd/mm/yy"),
                        style = "unite", icon = icon("gear"), # Icon
                        status = "primary", width = "300px",
                        animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                 exit = animations$fading_exits$fadeOutLeftBig),
                        tooltip = tooltipOptions(title = "Input here!")
               ))
      ),
      br(),
      fluidRow(
        column(width = 12,
               box(h4(strong("Decomposition of Multiplicative Time Series Plot"), 
                      style="text-align:center"),
                   # Output: Decomposition of multiplicative time series plot
                   plotOutput(outputId = "gvtrendplot1"),
                   p(strong("Data source"),": Monash University"),
                   width = 12,style="border:1px solid black"))
      ),
      # Go to Analysis Page -> Trend Part
      fluidRow(p(actionButton("gv_to_trend", "More Information", icon = icon("book")), style="text-align:center")),
      width = 12
    )
  ),
  # From Global View Page to About Page
  fluidRow(p(actionButton("link_to_about", "About", icon = icon("question")), style="text-align:center"))
  ),
  
#-----------------------------Dash 3 (Analysis)----------------------------------
    tabItem(
      tabName = "dash3",
      fluidPage(
        box(
          box(strong(p("User Guide:")),
              p("User can manipulate the plot by changing the setting (i.e., filtered data) in the gear icon button. 
                By hovering the cursor to a plot, there is a tooltip's text that shows the details about a specific
                part of the plot. Finally, user can also hide part(s) of the plot(s) by clicking the legend label(s).",
                style="text-align:justify"),
              background = "light-blue", width = 12),
          # Tab Panel
          tabsetPanel(
            id = "tabpanels",
#-----------------------------Distribution--------------------------------------
            tabPanel("Distribution",
                     br(),
                     fluidRow(
                       column(width = 1,
                              dropdown(tags$h3("Table & Graph Inputs"),
                                       pickerInput(inputId = "D_observed_variables", # Filter variable
                                                   label = "Observed Variables",
                                                   choices = unique(pv_data$observed_variables),
                                                   selected = unique(pv_data$observed_variables)[1],
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       dateRangeInput(inputId = "D_dates", # Filter date
                                                      label = "Date Range",
                                                      start = as.character(min(pv_data$PVdate)),
                                                      end = as.character(max(pv_data$PVdate)),
                                                      format = "dd/mm/yy"),
                                       pickerInput(inputId = "D_pvtime", # Filter time
                                                   label = "Time",
                                                   choices = unique(pv_data$PVtime),
                                                   selected = unique(pv_data$PVtime),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "primary", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                     ),
                     br(),
                     fluidRow(
                       column(width = 1,
                              dropdown(tags$h3("Distribution Plot Inputs"),
                                       pickerInput(inputId = "D_zero", # Filter zero value(s)
                                                   label = "Remove Zero Observation(s)?",
                                                   choices = c("Yes", "No"),
                                                   selected = "No",
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       pickerInput(inputId = "D_neg", # Filter negative value(s)
                                                   label = "Remove Negative Observation(s)?",
                                                   choices = c("Yes", "No"),
                                                   selected = "Yes",
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       pickerInput(inputId = "D_outlier", # Filter outlier(s)
                                                   label = "Remove Outlier(s)?",
                                                   choices = c("No", "Yes, Z-Score Approach", "Yes, Inter-Quartile Range (IQR) Approach"),
                                                   selected = "No",
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       pickerInput(inputId = "D_transformation", # Filter transformation method
                                                   label = "Transformation Method",
                                                   choices = c("Raw", "Square", "Cube", "Square Root",
                                                               "Cube Root", "Logarithm (Base 10)",
                                                               "Natural Logarithm"),
                                                   selected = "Raw",
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "danger", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                     ),
                     fluidRow(
                       box(h4(strong("Distribution Plot"), 
                              style="text-align:center"),
                           # Output: Distribution of Density Plot
                           plotlyOutput(outputId = "Dplot1"),
                           p(strong("Data source"),": Monash University"),
                           width = 7,style="border:1px solid black"),
                       box(background = "light-blue", 
                           width = 5,
                           column(width = 12,
                                  p(strong("Variable Name: "),
                                    textOutput("D_text1")),
                                  p(strong("Number of Observations: "),
                                    textOutput("D_text2")),
                                  p(strong("Descriptive Statistics: "),
                                    verbatimTextOutput("D_text3")),
                                  p(strong("Standard Deviation: "),
                                    textOutput("D_text4")),
                                  p(strong("Skewness: "),
                                    textOutput("D_text5")))
                       )),
                     fluidRow(
                       # Negative values interactive table
                       column(width = 7,
                              h3(strong("Negative Value Table"), 
                                 style="text-align:center"),
                              dataTableOutput("D_dataset")),
                       # Descriptive stats of the negative values
                       column(width = 5,
                              br(),
                              box(background = "light-blue",
                                  width = 12,
                                  p(strong("Variable Name: "),
                                    textOutput("D_neg1")),
                                  p(strong("Number of Observations: "),
                                    textOutput("D_neg2")),
                                  p(strong("Descriptive Statistics: "),
                                    verbatimTextOutput("D_neg3")),
                                  p(strong("Standard Deviation: "),
                                    textOutput("D_neg4")),
                                  p(strong("Skewness: "),
                                    textOutput("D_neg5")))),
                     ),
                     fluidRow(
                       column(width = 7,
                              # Zero Value Table
                              h3(strong("Zero Value Table"), 
                                 style="text-align:center"),
                              dataTableOutput("zero_dataset")),
                       column(width = 5,
                              box(h4(strong("Zero Value Count by Time Plot"), 
                                     style="text-align:center"),
                                  # Output: Zero Value Count by Time Plot
                                  plotlyOutput(outputId = "Dplot3"),
                                  p(strong("Data source"),": Monash University"),
                                  width = 12,style="border:1px solid black"))
                     ),
                     fluidRow(
                       box(h4(strong("Time Series Plot (Without Interactive Features)"), 
                              style="text-align:center"),
                           # Output: Time Series Plot
                           plotOutput(outputId = "Dplot4"),
                           p(strong("Data source"),": Monash University"),
                           width = 12,style="border:1px solid black")
                     ),
                     fluidRow(
                       box(h4(strong("Time Series Plot (With Interactive Features)"), 
                              style="text-align:center"),
                           # Output: Time Series Plot
                           plotlyOutput(outputId = "Dplot5"),
                           p(strong("Data source"),": Monash University"),
                           width = 12,style="border:1px solid black")
                     ),
                     # From Distribution Page to Global View Page
                     fluidRow(p(actionButton("distribution_to_gv", "Global View", icon = icon("globe-americas")), style="text-align:center"))
            ),

#-----------------------------Missing Value-------------------------------------
# https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html
# https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html
            tabPanel("Missing Value",
                     br(),
                     fluidRow(
                       column(width = 1,
                              dropdown(tags$h3("Table & Graph Inputs"),
                                       pickerInput(inputId = "MV_observed_variables", # Filter variable
                                                   label = "Observed Variables",
                                                   choices = unique(pv_data$observed_variables),
                                                   selected = head(unique(pv_data$observed_variables), 5),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       dateRangeInput(inputId = "MV_dates", # Filter date
                                                      label = "Date Range",
                                                      start = as.character(min(pv_data$PVdate)),
                                                      end = as.character(max(pv_data$PVdate)),
                                                      format = "dd/mm/yy"),
                                       pickerInput(inputId = "MV_pvtime", # Filter time
                                                   label = "Time",
                                                   choices = unique(pv_data$PVtime),
                                                   selected = unique(pv_data$PVtime),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "primary", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                                       ))
                     ),
                     fluidRow(
                       column(width = 8, 
                              h3(strong("Missing Value Table"), 
                                 style="text-align:center"),
                              dataTableOutput("MVdataset")),
                       column(width = 4,
                              br(),
                              box(background = "light-blue", 
                                  width = 12,
                                  column(width = 12,
                                         p(strong("Number of observations: "),
                                           textOutput("MV_textA")),
                                         p(strong("Total problematic rows: "),
                                           textOutput("MV_textB")),
                                         p(strong("Propotion of the problematic rows: "),
                                           textOutput("MV_textC")),
                                         p(strong("Number of cells: "),
                                           textOutput("MV_text1")),
                                         p(strong("Total missing by the number of cells: "),
                                           textOutput("MV_text2")),
                                         p(strong("Proportion of missing by the number of cells: "),
                                           textOutput("MV_text3")))
                                  ))),
                     br(),
                     br(),
                     fluidRow(
                       box(h4(strong("Count of Missing Values"), 
                              style="text-align:center"),
                           # Output: Count of Missing Value Plot
                           plotlyOutput(outputId = "plot1"),
                           p(strong("Data source"),": Monash University"),
                           width = 8,style="border:1px solid black"),
                       box(h4(strong("Proportion of Missing Values"), 
                              style="text-align:center"),
                           # Output: Proportion of Missing Plot
                           plotlyOutput(outputId = "plot2"),
                           p(strong("Data source"),": Monash University"),
                           width = 4,style="border:1px solid black")
                     ),
                     fluidRow(
                       box(h4(strong("Missing Values in Rows"), 
                              style="text-align:center"),
                           # Output: Missing Values in Rows Plot
                           plotOutput(outputId = "plot3"),
                           p(strong("Data source"),": Monash University"),
                           width = 12,style="border:1px solid black")
                     ),
                     fluidRow(
                       # UpSet Plot Filter
                       column(width = 1,
                              dropdown(tags$h3("Table & Graph Inputs"),
                                       pickerInput(inputId = "MV_observed_variables1", # Filter variable
                                                   label = p("Observed Variables (Max: 5)",
                                                             br(),
                                                             strong("Note: "), "Variables must exist in the
                                                             previous plot"),
                                                   choices = unique(pv_data$observed_variables),
                                                   selected = head(unique(pv_data$observed_variables), 5),
                                                   options = list(`actions-box` = TRUE,
                                                                  `max-options` = 5),
                                                   multiple = TRUE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "danger", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                     ),
                     fluidRow(
                       box(h4(strong("UpSet Plot"), 
                              style="text-align:center"),
                           # Output: Upset Plot (Exploring patterns with UpSetR)
                           plotOutput(outputId = "plot4"),
                           p(strong("Data source"),": Monash University"),
                           width = 12,style="border:1px solid black")
                     ),
                     fluidRow(
                       # Missingness across Factors Plot Filter
                       column(width = 1,
                              dropdown(tags$h3("Table & Graph Inputs"),
                                       varSelectInput(inputId = "MV_observed_variables2", # Filter variable
                                                      label = "Categorical Variable (Factor)",
                                                      data = small_df,
                                                      selected = "AMorPM",
                                                      multiple = FALSE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "danger", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                     ),
                     fluidRow(
                       box(h4(strong("Missingness across Factors"), 
                              style="text-align:center"),
                           # Output: Missingness across Factors Plot
                           plotlyOutput(outputId = "plot5"),
                           p(strong("Data source"),": Monash University"),
                           width = 12,style="border:1px solid black")
                     ),
                     # From Missing Value Page to Global View Page
                     fluidRow(p(actionButton("missing_to_gv", "Global View", icon = icon("globe-americas")), style="text-align:center"))
            ),

# https://rstudio.github.io/DT/options.html
# fluidRow(column(width = 1, dropdown(tags$h3("Table Inputs"),
#                                    pickerInput(inputId = ""))))
#-----------------------------Outlier-------------------------------------------
            tabPanel("Outlier",
                     br(),
                     fluidRow(
                       column(width = 5),
                       column(width = 7,
                              box(strong(h4("Univariate Analysis", style="text-align:center")),
                                  background = "light-blue", width = 4))
                     ),
                     fluidRow(
                       column(width = 1,
                              dropdown(tags$h3("Table & Graph Inputs"),
                                       pickerInput(inputId = "O_observed_variables", # Filter variable
                                                   label = "Observed Variables",
                                                   choices = unique(pv_data$observed_variables),
                                                   selected = unique(pv_data$observed_variables)[1],
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       dateRangeInput(inputId = "O_dates", # Filter date
                                                      label = "Date Range",
                                                      start = as.character(min(pv_data$PVdate)),
                                                      end = as.character(max(pv_data$PVdate)),
                                                      format = "dd/mm/yy"),
                                       pickerInput(inputId = "O_pvtime", # Filter time
                                                   label = "Time",
                                                   choices = unique(pv_data$PVtime),
                                                   selected = unique(pv_data$PVtime),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "primary", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                       ),
                     tabsetPanel(
                       id = "tabpanels_o",
                       tabPanel("Z-Score Approach",
                                br(),
                                fluidRow(
                                  # Potential Outlier(s) Table
                                  column(width = 7,
                                         h3(strong("Potential Outlier(s) Table"), 
                                            style="text-align:center"),
                                         dataTableOutput("OZdataset")),
                                  # Descriptive stats, upper limit, and lower limit
                                  column(width = 5,
                                         br(),
                                         box(background = "light-blue",
                                             width = 12,
                                             p(strong("Variable Name: "),
                                               textOutput("O_varname1")),
                                             p(strong("Descriptive Statistics: "),
                                               verbatimTextOutput("O_descstat1")),
                                             p(strong("Standard Deviation: "),
                                               textOutput("O_std1")),
                                             p(strong("Skewness before Removing Outliers: "),
                                               textOutput("O_skewness1")),
                                             p(strong("Upper Limit: mean + 3 * stdev"),
                                               textOutput("O_upper1")),
                                             p(strong("Lower Limit: mean - 3 * stdev"),
                                               textOutput("O_lower1")))),
                                ),
                                br(),
                                p(strong("Before Removing Outlier(s)")),
                                fluidRow(
                                  box(h4(strong("Density"), 
                                         style="text-align:center"),
                                      # Output: Density Plot (Before Removing Outlier(s))
                                      plotlyOutput(outputId = "oplot1"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black"),
                                  box(h4(strong("Boxplot"), 
                                         style="text-align:center"),
                                      # Output: Boxplot Plot (Before Removing Outlier(s))
                                      plotOutput(outputId = "oplot2"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black")
                                ),
                                br(),
                                p(strong("After Removing Outlier(s)")),
                                fluidRow(
                                  box(h4(strong("Density"), 
                                         style="text-align:center"),
                                      # Output: Density Plot (After Removing Outlier(s))
                                      plotlyOutput(outputId = "oplot3"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black"),
                                  box(h4(strong("Boxplot"), 
                                         style="text-align:center"),
                                      # Output: Boxplot Plot (After Removing Outlier(s))
                                      plotOutput(outputId = "oplot4"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black")
                                )),
                       tabPanel("Inter-Quartile Range (IQR) Approach",
                                br(),
                                fluidRow(
                                  # Potential Outlier(s) Table
                                  column(width = 7,
                                         h3(strong("Potential Outlier(s) Table"), 
                                            style="text-align:center"),
                                         dataTableOutput("OIQRdataset")),
                                  # Descriptive stats, upper limit, and lower limit
                                  column(width = 5,
                                         br(),
                                         box(background = "light-blue",
                                             width = 12,
                                             p(strong("Variable Name: "),
                                               textOutput("IQR_varname1")),
                                             p(strong("Descriptive Statistics: "),
                                               verbatimTextOutput("IQR_descstat1")),
                                             p(strong("Inter-Quartile Range (IQR): "),
                                               textOutput("IQR_iqr1")),
                                             p(strong("Skewness before Removing Outliers: "),
                                               textOutput("IQR_skewness1")),
                                             p(strong("Upper Limit: Q3 + 1.5 * IQR"),
                                               textOutput("IQR_upper1")),
                                             p(strong("Lower Limit: Q1 - 1.5 * IQR"),
                                               textOutput("IQR_lower1"))))
                                ),
                                br(),
                                p(strong("Before Removing Outlier(s)")),
                                fluidRow(
                                  box(h4(strong("Density"), 
                                         style="text-align:center"),
                                      # Output: Density Plot (Before Removing Outlier(s))
                                      plotlyOutput(outputId = "iqrplot1"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black"),
                                  box(h4(strong("Boxplot"), 
                                         style="text-align:center"),
                                      # Output: Boxplot Plot (Before Removing Outlier(s))
                                      plotOutput(outputId = "iqrplot2"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black")
                                ),
                                br(),
                                p(strong("After Removing Outlier(s)")),
                                fluidRow(
                                  box(h4(strong("Density"), 
                                         style="text-align:center"),
                                      # Output: Density Plot (After Removing Outlier(s))
                                      plotlyOutput(outputId = "iqrplot3"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black"),
                                  box(h4(strong("Boxplot"), 
                                         style="text-align:center"),
                                      # Output: Boxplot Plot (After Removing Outlier(s))
                                      plotOutput(outputId = "iqrplot4"),
                                      p(strong("Data source"),": Monash University"),
                                      width = 6,style="border:1px solid black")
                                )
                                )),
                     # From Outlier Page to Global View Page
                     fluidRow(p(actionButton("outlier_to_gv", "Global View", icon = icon("globe-americas")), style="text-align:center"))
            ),
#-----------------------------Feature Selection---------------------------------
            tabPanel("Feature Selection",
                     br(),
                     fluidRow(
                       column(width = 1,
                              dropdown(tags$h3("Feature Selection Inputs"),
                                       pickerInput(inputId = "FS_explanatory_variable", # Filter response variable
                                                   label = "Explanatory Variable(s)",
                                                   choices = colnames(selection_df)[2:ncol(selection_df)],
                                                   selected = colnames(selection_df)[2:ncol(selection_df)],
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "primary", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                     ),
                     tabsetPanel(
                       id = "tabpanels_fs",
                       tabPanel("Linear Regression",
                                fluidRow(
                                  # Template for fitted model and Variable importance estimations
                                  column(width = 12,
                                         br(),
                                         box(background = "light-blue",
                                             width = 12,
                                             p(strong("Dependent Variable: "),
                                               verbatimTextOutput("ri_dependent_variable")),
                                             p(strong("Independent Variable(s): "),
                                               verbatimTextOutput("ri_independent_variable")),
                                             p(strong("Fitted Linear Model Output: "),
                                               verbatimTextOutput("ri_model_output")),
                                             p(strong("Variable Importance Estimation: "),
                                               verbatimTextOutput("ri_estimation"))))
                                ),
                                fluidRow(
                                  column(width = 12,
                                         box(h4(strong("Variable Importance Plot"), 
                                                style="text-align:center"),
                                             # Output: Variable Importance Plot
                                             plotlyOutput(outputId = "fsplot1"),
                                             p(strong("Data source"),": Monash University"),
                                             width = 12,style="border:1px solid black"))
                                )
                                ),
                       tabPanel("Random Forest",
                                br(),
                                fluidRow(
                                  column(width = 1,
                                         dropdown(tags$h3("Random Forest Method Inputs"),
                                                  pickerInput(inputId = "rffs_ntree", # Filter number of trees grown
                                                              label = "Number of Trees Grown (ntree)",
                                                              choices = c("10", "50", "100", "250", "500", "1000"),
                                                              selected = "50",
                                                              options = list(`actions-box` = TRUE),
                                                              multiple = FALSE),
                                                  pickerInput(inputId = "rffs_mtry", # Filter number of predictors sampled for spliting at each node
                                                              label = "Number of Predictors Sampled for Spliting at each Node (mtry)",
                                                              choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                                              selected = "10",
                                                              options = list(`actions-box` = TRUE),
                                                              multiple = FALSE),
                                                  style = "unite", icon = icon("gear"), # Icon
                                                  status = "danger", width = "300px",
                                                  animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                           exit = animations$fading_exits$fadeOutLeftBig),
                                                  tooltip = tooltipOptions(title = "Input here!")
                                         ))
                                ),
                                fluidRow(
                                  # Template for fitted model and variable importance estimations
                                  column(width = 12,
                                         br(),
                                         box(background = "light-blue",
                                             width = 12,
                                             p(strong("Dependent Variable: "),
                                               verbatimTextOutput("rffs_dependent_variable")),
                                             p(strong("Independent Variable(s): "),
                                               verbatimTextOutput("rffs_independent_variable")),
                                             p(strong("Number of Trees Grown (ntree): "),
                                               verbatimTextOutput("rffs_ntree_text")),
                                             p(strong("Number of Predictors Sampled for Splitting at each Node (mtry): "),
                                               verbatimTextOutput("rffs_mtry_text")),
                                             p(strong("Fitted Random Forest Model Output: "),
                                               verbatimTextOutput("rffs_model_output")),
                                             p(strong("Variable Importance Estimation: "),
                                               verbatimTextOutput("rffs_estimation"))))
                                ),
                                fluidRow(
                                  column(width = 12,
                                         box(h4(strong("Variable Importance Plot"), 
                                                style="text-align:center"),
                                             # Output: Variable Importance Plot
                                             plotlyOutput(outputId = "fsplot2"),
                                             p(strong("Data source"),": Monash University"),
                                             width = 12,style="border:1px solid black"))
                                )
                                ),
                       tabPanel("MARS",
                                fluidRow(
                                  # Template for fitted model and variable importance estimations
                                  column(width = 12,
                                         br(),
                                         box(background = "light-blue",
                                             width = 12,
                                             p(strong("Dependent Variable: "),
                                               verbatimTextOutput("mars_dependent_variable")),
                                             p(strong("Independent Variable(s): "),
                                               verbatimTextOutput("mars_independent_variable")),
                                             p(strong("Fitted MARS Model Output: "),
                                               verbatimTextOutput("mars_model_output")),
                                             p(strong("Variable Importance Estimation: "),
                                               verbatimTextOutput("mars_estimation"))))
                                ),
                                fluidRow(
                                  column(width = 12,
                                         box(h4(strong("Variable Importance Plot"), 
                                                style="text-align:center"),
                                             # Output: Variable Importance Plot
                                             plotOutput(outputId = "fsplot3"),
                                             p(strong("Data source"),": Monash University"),
                                             width = 12,style="border:1px solid black"))
                                )
                                ),
                       tabPanel("Boruta",
                                fluidRow(
                                  # Template for fitted model and variable importance estimations
                                  column(width = 12,
                                         br(),
                                         box(background = "light-blue",
                                             width = 12,
                                             p(strong("Dependent Variable: "),
                                               verbatimTextOutput("bfs_dependent_variable")),
                                             p(strong("Independent Variable(s): "),
                                               verbatimTextOutput("bfs_independent_variable")),
                                             p(strong("Number of Trees Grown (ntree): "),
                                               verbatimTextOutput("bfs_ntree_text")),
                                             p(strong("Number of Predictors Sampled for Splitting at each Node (mtry): "),
                                               verbatimTextOutput("bfs_mtry_text")),
                                             p(strong("Fitted Boruta Model Output: "),
                                               verbatimTextOutput("bfs_model_output")),
                                             p(strong("Variable(s) Confirmed Important by Boruta: "),
                                               verbatimTextOutput("bfs_confirmed")),
                                             p(strong("Variable Importance Estimations: "),
                                               verbatimTextOutput("bfs_estimation"))))
                                ),
                                fluidRow(
                                  column(width = 12,
                                         box(h4(strong("Variable Importance Plot"), 
                                                style="text-align:center"),
                                             # Output: Variable Importance Plot
                                             plotOutput(outputId = "fsplot4"),
                                             p(strong("Data source"),": Monash University"),
                                             width = 12,style="border:1px solid black"))
                                )
                                )
                       ),
                     # From Feature Selection Page to Global View Page
                     fluidRow(p(actionButton("feature_selection_to_gv", "Global View", icon = icon("globe-americas")), style="text-align:center"))
            ),

#-----------------------------Trend---------------------------------------------
            tabPanel("Trend",
                     br(),
                     fluidRow(
                       column(width = 1,
                              dropdown(tags$h3("Table & Graph Inputs"),
                                       pickerInput(inputId = "trend_observed_variables", # Filter variable
                                                   label = "Observed Variables",
                                                   choices = unique(pv_data$observed_variables),
                                                   selected = unique(pv_data$observed_variables)[1],
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = FALSE),
                                       dateRangeInput(inputId = "trend_dates", # Filter date
                                                      label = "Date Range",
                                                      start = as.character(min(pv$PVdate)),
                                                      end = as.character(max(pv$PVdate)),
                                                      format = "dd/mm/yy"),
                                       style = "unite", icon = icon("gear"), # Icon
                                       status = "primary", width = "300px",
                                       animate = animateOptions(enter = animations$fading_entrances$fadeInLeftBig,
                                                                exit = animations$fading_exits$fadeOutLeftBig),
                                       tooltip = tooltipOptions(title = "Input here!")
                              ))
                     ),
                     br(),
                     fluidRow(
                       column(width = 12,
                              box(h4(strong("Decomposition of Multiplicative Time Series Plot"), 
                                     style="text-align:center"),
                                  # Output: Decomposition of multiplicative time series plot
                                  plotOutput(outputId = "trendplot1"),
                                  p(strong("Data source"),": Monash University"),
                                  width = 12,style="border:1px solid black"))
                     ),
                     # From Trend Page to Global View Page
                     fluidRow(p(actionButton("trend_to_gv", "Global View", icon = icon("globe-americas")), style="text-align:center"))
            )
          ), width = 12
    ))),
#-----------------------------Dash4 (About)-------------------------------------
      tabItem(
        tabName = "dash4",
        fluidPage(
          h3("About this application", style = "color:black; font-weight: 600"),
          p("Time Series Data Analytics and Visualization web application allows users to explore the 
            various time series datasets in terms of their distribution, missing value, outlier, and 
            trend. Furthermore, this application also allows users to perform feature selection on the
            training dataset with various methods such as linear regression, random forest, 
            multivariate adaptive regression (MARS), and Boruta.", style = "text-align:justify"),
          p("The application are intended to assist the users in understanding the data anomalies by 
            providing an easy-to-use dashboard for a multi-level view of the time series data 
            (originally Monash Photovoltaic (PV) Generation data). Another aim is to substantially 
            improve the efficiency of the users in wrangling and cleaning the time series data, 
            thereby focusing more time on training and building a machine learning model.", 
            style = "text-align:justify"),
          h3("Packages", style = "color:black; font-weight: 600"),
          tags$div(tags$ul(
            tags$li("Shiny, Chang et al. (2022)"),
            tags$li("Shinydashboard, Chang et al. (2021)"),
            tags$li("Shinythemes, Chang et al. (2021)"),
            tags$li("shinyWidgets, Perrier et al. (2022)"),
            tags$li("DT, Xie et al. (2022)"),
            tags$li("Tidyverse, Wickham et al. (2019)"),
            tags$li("naniar, Tierney et al. (2021)"),
            tags$li("Plotly, Chapman & Hall/CRC (2020)"),
            tags$li("ggplot2, Whickham. H. (2016)"),
            tags$li("e1071, Mayer et al. (2022)"),
            tags$li("lubridate, Grolemund et al. (2011)"),
            tags$li("caret, Kuhn. M. (2022)"),
            tags$li("randomForest, Liaw. A. &  Wiener. M. (2002)"),
            tags$li("earth, Hastie SMDfmbT, wrapper. RTUAMFuwTLl (2023)"),
            tags$li("Boruta, Miron et al. (2010)"),
            tags$li("zoo, Zeileis et al. (2005)"),
          ), style = "font-size: 15px"),
          h3("Bibliography", style = "color:black; font-weight: 600"),
          p("Chang, Winston, Cheng, J., Allaire, J.J., Xie, Y., and McPherson, J. (2022, December 15).", 
            em("Shiny: Web Application Framework for R."), 
            a(href="https://CRAN.R-project.org/package=shiny", 
              "https://CRAN.R-project.org/package=shiny")),
          p("Kassambara. (2017, November 17).", 
            em("Plot time series data using GGplot."), 
            "STHDA.", 
            a(href="http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/", 
              "http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/")),
          p("Laufer, J. (2019, February 5).", 
            em("Missing value visualization with tidyverse in R."),
            a(href="https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html", 
              "https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html")),
          p("Marriane. (2021, September 30).", 
            em("9 principles of good web design."), 
            "Feeling Peaky.", 
            a(href="https://www.feelingpeaky.com/9-principles-of-good-web-design/", 
              "https://www.feelingpeaky.com/9-principles-of-good-web-design/")),
          p("Nikola, O. (2021, December 29).", 
            em("Random Forest Regression in R: Code and Interpretation."), 
            "Feeling Peaky.", 
            a(href="https://hackernoon.com/random-forest-regression-in-r-code-and-interpretation", 
              "https://hackernoon.com/random-forest-regression-in-r-code-and-interpretation")),
          p("sgr308. (2022, July 22).", 
            em("how to see second tabitem after clicking action button??"), 
            "Posit Community.", 
            a(href="https://community.rstudio.com/t/how-to-see-second-tabitem-after-clicking-action-button/142783", 
              "https://community.rstudio.com/t/how-to-see-second-tabitem-after-clicking-action-button/142783")),
          p("Tierney, N. (2023, February 2).", 
            em("Getting started with naniar."),
            a(href="https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html", 
              "https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html")),

          
          # Back to Home page
          br(),
          br(),
          fluidRow(p(actionButton("link_to_home", "Home", icon = icon("home")), style="text-align:center"))
      ))
  ))

# Define UI for application
ui <- dashboardPage(header,
                    sidebar,
                    body,
                    skin = "blue"
)

#-------------------------------------------------------------------------------

# Web Server #

#-------------------------------------------------------------------------------

# Define server logic 
server <- function(input, output, session) {

  ################################### HOME #####################################
  
  # Interactive raw dataset table
  output$RawData <- renderDataTable({
    datatable(pv_raw, options = list(
      pageLength = 5,
      lengthMenu = c(5, 10)
    ))
  })
  
  # Go to Global View Page
  # https://community.rstudio.com/t/how-to-see-second-tabitem-after-clicking-action-button/142783
  observeEvent(input$link_to_gv, {
    updateTabItems(session, "tabs", selected = "dash2")
  })
  
  ################################### GLOBAL VIEW ##############################
  
                            ##### Distribution Part #####
  
  # Step 1. Create gvfiltered_df by filtering observed.variables, PVdate, and PVtime
  gvfiltered_df <- reactive({ 
    
    newdata <- pv_data %>% 
      filter(observed_variables %in% input$gvD_observed_variables) %>% # filter observed_variables
      filter(between(PVdate, input$gvD_dates[1], input$gvD_dates[2])) %>% # filter PVdate
      filter(PVtime %in% input$gvD_pvtime) # filter PVtime
    
    return(newdata)
  })
  
  # Step 2. Missing Value DF (after removing missing value)
  gvrmv_na_df <- reactive({
    # Since it is a transformation method, we need to remove the missing values before plotting to avoid errors
    temp_df <- gvfiltered_df() %>% 
      # Remove NA(s) / Missing Value(s)
      na.omit()
    
    return(temp_df)
  })
  
  # Step 3. Remove Zero Observation(s) Option
  gvrmv_zero_df <- reactive({
    
    temp_df <- gvrmv_na_df()
    
    newdata <- temp_df %>% 
      # Remove 0 observation(s)
      filter(if (input$gvD_zero=="Yes") {
        observed_values != 0
      } else if (input$gvD_zero=="No") {
        observed_values != ""
      })
    
    return(newdata)
    
  })
  
  # Step 4. Remove Negative Value(s) Option
  gvrmv_negative_df <- reactive({
    
    temp_df <- gvrmv_zero_df()
    
    newdata <- temp_df %>% 
      # Remove negative value(s)
      filter(if (input$gvD_neg=="Yes") {
        observed_values >= 0
      } else if (input$gvD_neg=="No") {
        observed_values != ""
      }) 
    
    return(newdata)
    
  })
  
  # Step 5. Remove Outlier(s) Option
  gvrmv_outlier_df <- reactive({
    
    temp_df <- gvrmv_negative_df()
    
    # Z-Score Calculation
    right_tailz <- mean(temp_df$observed_values) + 3*sd(temp_df$observed_values)
    left_tailz <- mean(temp_df$observed_values) - 3*sd(temp_df$observed_values)
    
    # IQR Calculation
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(temp_df$observed_values)
    q1 <- as.numeric(quantile(temp_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(temp_df$observed_values, probs = 0.75))
    
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    newdata <- temp_df %>% 
      # Remove outlier(s) with method
      filter(if (input$gvD_outlier=="No") {
        observed_values != ""
      } else if (input$gvD_outlier=="Yes, Z-Score Approach") {
        observed_values < right_tailz & observed_values > left_tailz
      } else if (input$gvD_outlier=="Yes, Inter-Quartile Range (IQR) Approach") {
        observed_values < right_taili & observed_values > left_taili
      })
    
    return(newdata)
    
  })
  
  # Step 6. Transformation Method option
  gvdused_df1 <- reactive({
    
    newdata <- gvrmv_outlier_df()
    
    if (input$gvD_transformation=="Raw") {
      return(newdata)
    } else if (input$gvD_transformation=="Square") {
      temp_df <- newdata %>% 
        mutate(observed_values = observed_values^2)
      
      return(temp_df)
    } else if (input$gvD_transformation=="Cube") {
      temp_df <- newdata %>% 
        mutate(observed_values = observed_values^3)
      
      return(temp_df)
    } else if (input$gvD_transformation=="Square Root") {
      temp_df <- newdata %>% 
        mutate(observed_values = sqrt(observed_values))
      
      return(temp_df)
    } else if (input$gvD_transformation=="Cube Root") {
      temp_df <- newdata %>% 
        mutate(observed_values = observed_values^(1/3))
      
      return(temp_df)
    } else if (input$gvD_transformation=="Logarithm (Base 10)") {
      temp_df <- newdata %>% 
        mutate(observed_values = log10(observed_values))
      
      return(temp_df)
    } else if (input$gvD_transformation=="Natural Logarithm") {
      temp_df <- newdata %>% 
        mutate(observed_values = log(observed_values))
      
      return(temp_df)
    }
    
  })
  
  # Step 7. Distribution of Density Plot
  output$gvDplot1 <- renderPlotly({
    used_df <- gvdused_df1()
    
    ggplot(data=used_df, aes(x=observed_values, fill="purple")) + 
      geom_density(adjust=1.5, alpha=.4) +
      xlab(as.character(input$gvD_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 8. Distribution Plot's Descriptive Statistics
  
  # Step 8.1. Variable Name
  # gvD_text1 -> selected observed variable name
  output$gvD_text1 <- renderText({
    as.character(input$gvD_observed_variables)
  })
  
  # Step 8.2. Number of Observations
  # gvD_text2 -> count number of observed cells
  output$gvD_text2 <- renderText({
    nrow(gvdused_df1())
  })
  
  # Step 8.3. Descriptive Statistics
  # gvD_text3 -> create the selected observed variable descriptive statistic table
  output$gvD_text3 <- renderPrint({
    summary(gvdused_df1()$observed_values)
  })
  
  # Step 8.4. Standard Deviation
  # gvD_text4 -> calculate the selected observed variable standard deviation
  output$gvD_text4 <- renderText({
    round(sd(gvdused_df1()$observed_values), 3)
  })
  
  # Step 8.5. Skewness
  # gvD_text5 -> calculate the selected observed variable skewness
  output$gvD_text5 <- renderText({
    round(skewness(gvdused_df1()$observed_values), 3)
  })
  
  # Step 9. Time Series Plot
  
  # Step 9.a. Create gvdatetime_df
  gvdatetime_df <- reactive({
    
    gvdatetime_df <- gvfiltered_df()
    
    # Step 9.1. Convert ReadTime into datetime data type by using Lubridate package 
    gvdatetime_df$ReadTime <- dmy_hm(gvdatetime_df$ReadTime)
    
    # Step 9.2. Create dummy if NA = Missing, and present = Present
    gvdatetime_df$missing_dummy <- ifelse(is.na(gvdatetime_df$observed_values), "Missing", "Present")
    
    return(gvdatetime_df)
  })
  
  # Step 9.b. Plotting (Without Interactive Feature)
  output$gvDplot4 <- renderPlot({
    
    temp_df <- gvdatetime_df()
    
    # Step 9.b. Plotting (Without Interactive Feature)
    ggplot(temp_df, aes(x = ReadTime, y = observed_values, group = 1)) +
      geom_line(color="#0288d1") +
      geom_vline(xintercept = temp_df$ReadTime[temp_df$missing_dummy == "Missing"], color = "red") +
      xlab("Time") +
      ylab(as.character(input$gvD_observed_variables)) +
      scale_color_discrete(name = "Status") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
                          ##### Missing Value Part #####
  
  # Step 1. Create filtered_df by filtering observed_variables, PVdate, and PVtime
  gvmfiltered_df <- reactive({ 
    
    newdata <- pv_data %>% 
      filter(observed_variables %in% input$gvMV_observed_variables) %>% # observed_variables
      filter(between(PVdate, input$gvMV_dates[1], input$gvMV_dates[2])) %>% # PVdate
      filter(PVtime %in% input$gvMV_pvtime) # PVtime
    
    return(newdata)
  })
  
  # Step 2. Creating Missing Value Table
  
  # Step 2.1. Reformat gvmfiltered_df from long to wide format to calculate the number of missing rows easily
  gvmfiltered_df_wider <- reactive({
    
    gvmfiltered_df <- gvmfiltered_df()
    
    newdata <- gvmfiltered_df %>% 
      # Change from long into wide format
      pivot_wider(names_from = observed_variables, values_from = observed_values) %>% 
      # Deselect helper variables
      dplyr::select(!c(PVdate, dayofmonth, month, year, timefactor, AMorPM, PVtime))
    
    return(newdata)
  })
  
  # Step 2.2. Creating Missing Value Dataframe
  gvmmissing_df <- reactive({ 
    gvmfiltered_df_wider <- gvmfiltered_df_wider()
    
    newdata <- as_tibble(lapply(gvmfiltered_df_wider, function(x) sum(is.na(x)))) %>% 
      gather(key = "variable_name", value = "missing_count") %>% 
      # Calculate the total missing values category in the dataset
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~'Total')))
    
    return(newdata)
  })
  
  # Step 2.3. Send the output of missing_df in step 2.2. above to the frontend after filter out the
  # Total category that we created previously
  output$gvMVdataset <- renderDataTable({
    newdata <- gvmmissing_df()
    
    # Filter out the Total category that we created previously
    newdata <- newdata %>% 
      filter(variable_name != "Total")
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 3. Missing Value Table's Descriptive Statistics
  
  # Step 3.1. Create gvmfiltered_df_nona to count problematic row (i.e., there is an existence of missing values in one or more columns)
  gvmfiltered_df_nona <- reactive({
    
    gvmfiltered_df_wider <- gvmfiltered_df_wider()
    # Remove missing value in the gvmfiltered_df_wider dataframe to find # of rows that is fine (i.e., no missing value)
    newdata <- na.omit(gvmfiltered_df_wider)
    
    return(newdata)
  })
  
  # Step 3.2. Number of observations
  # gvMV_textA -> count total observations
  output$gvMV_textA <- renderText({
    nrow(gvmfiltered_df_wider())
  })
  
  # Step 3.3. Total problematic rows
  # gvMV_textB -> count number of problematic rows (i.e., has missing value)
  output$gvMV_textB <- renderText({
    nrow(gvmfiltered_df_wider()) - nrow(gvmfiltered_df_nona())
  })
  
  # Step 3.4. Proportion of problematic rows
  # gvMV_textC -> count the percentage of problematic rows (i.e., has missing value)
  output$gvMV_textC <- renderText({
    x <- nrow(gvmfiltered_df_wider())
    y <- nrow(gvmfiltered_df_wider()) - nrow(gvmfiltered_df_nona())
    z <- round((y / x) * 100, 2)
    
    # Concatenate string
    paste(as.character(z), "%", sep = "")
  })
  
  # Step 3.5. Number of cells
  # gvMV_text1 -> count number of observed cells
  output$gvMV_text1 <- renderText({
    nrow(gvmfiltered_df())
  })
  
  # Step 3.6. Total missing by the number of cells
  # gvMV_text2 -> count number of missing values in the observed cells
  output$gvMV_text2 <- renderText({
    gvmmissing_df <- gvmmissing_df()
    
    as.integer(gvmmissing_df[gvmmissing_df$variable_name == "Total", "missing_count"])
  })
  
  # Step 3.7. Proportion of missing by the number of cells
  # gvMV_text3 -> count the percentage of missing values (by cells)
  output$gvMV_text3 <- renderText({
    gvmmissing_df <- gvmmissing_df()
    gvmfiltered_df <- gvmfiltered_df()
    
    x <- round(as.integer(gvmmissing_df[gvmmissing_df$variable_name == "Total", "missing_count"]) / nrow(gvmfiltered_df) * 100, 2)
    
    # Concatenate string
    paste(as.character(x), "%", sep = "")
  })
  
  # Step 4. Missing Value Plot 1 (Stacked Bar-Chart / Where the Missing Plot)
  output$gvplot3 <- renderPlot({
    
    # Create new dataframe that include the calculation of proportion of the missing values for each variable
    newdata <- gvmmissing_df() %>% 
      rename(Missing = missing_count) %>% 
      filter(variable_name != "Total") %>% 
      mutate(Present = nrow(gvmfiltered_df_wider()) - Missing) %>% 
      pivot_longer(cols = !c("variable_name"),
                   names_to = "isna",
                   values_to = "total_count") %>% 
      mutate(pct = total_count / nrow(gvmfiltered_df_wider()) * 100)
    
    # Sort from highest to lowest
    levels <-
      (newdata  %>% filter(isna == "Missing") %>% arrange(pct))$variable_name
    
    # Plotting
    gvmfiltered_df_wider() %>%
      mutate(id = row_number()) %>%
      gather(-id, key = "key", value = "val") %>%
      mutate(isna = is.na(val)) %>%
      ggplot(aes(x = key, y = id, fill = isna)) +
      geom_tile() +
      scale_fill_manual(name = "",
                        values = c('steelblue', 'tomato3'),
                        labels = c("Present", "Missing"))  +
      scale_x_discrete(limits = levels) +
      labs(x = "Variable",
           y = "Row Number") +
      coord_flip() +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
                          ##### Outlier Part #####
  
  # Step 1. Create gvofiltered_df by filtering observed_variables, PVdate, and PVtime
  gvofiltered_df <- reactive({ 
    
    newdata <- pv_data %>% 
      filter(observed_variables %in% input$gvO_observed_variables) %>% # observed_variables
      filter(between(PVdate, input$gvO_dates[1], input$gvO_dates[2])) %>% # PVdate
      filter(PVtime %in% input$gvO_pvtime) # PVtime
    
    return(newdata)
  })
  
  # Step 2. Create gvoused_df dataframe for plotting histograms and boxplots (after removing NAs)
  gvoused_df <- reactive({ 
    
    newdata <- gvofiltered_df() %>% 
      na.omit()
    
    return(newdata)
  })
  
  ##### Z-Score Approach #####
  
  # Step 3. Interactive table description (Outlier -> Z-Score Approach)
  output$gvOZdataset <- renderDataTable({
    temp_df <- gvoused_df()
    
    right_tailz <- mean(temp_df$observed_values) + 3*sd(temp_df$observed_values)
    left_tailz <- mean(temp_df$observed_values) - 3*sd(temp_df$observed_values)
    
    newdata <- temp_df %>% 
      filter(observed_values > right_tailz | observed_values < left_tailz) %>% 
      dplyr::select(ReadTime, observed_variables, observed_values)
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 4. Z-Score Approach Table's Descriptive Statistics
  
  # Step 4.1. Variable Name
  # gvO_varname1 -> selected observed variable name
  output$gvO_varname1 <- renderText({
    as.character(input$gvO_observed_variables)
  })
  
  # Step 4.2. Descriptive Statistics
  # gvO_descstat1 -> create the selected observed variable descriptive statistic table
  output$gvO_descstat1 <- renderPrint({
    summary(gvofiltered_df()$observed_values)
  })
  
  # Step 4.3. Standard Deviation
  # gvO_std1 -> calculate the selected observed variable standard deviation
  output$gvO_std1 <- renderText({
    round(sd(gvoused_df()$observed_values), 3)
  })
  
  # Step 4.4. Skewness before removing outliers
  # gvO_skewness1 -> calculate the selected observed variable skewness
  output$gvO_skewness1 <- renderText({
    round(skewness(gvoused_df()$observed_values), 3)
  })
  
  # Step 4.5. Upper Limit: mean + 3 * stdev
  # gvO_upper1 -> calculate upper limit by using z-score approach
  output$gvO_upper1 <- renderText({
    round(mean(gvoused_df()$observed_values) + 3*sd(gvoused_df()$observed_values), 3)
  })
  
  # Step 4.6. Lower Limit: mean - 3 * stdev
  # gvO_lower1 -> calculate lower limit by using z-score approach
  output$gvO_lower1 <- renderText({
    round(mean(gvoused_df()$observed_values) - 3*sd(gvoused_df()$observed_values), 3)
  })
  
  # Step 5. Z-Score outlier density plot (before -> plot 1)
  output$gvoplot1 <- renderPlotly({
    used_df <- gvoused_df()
    
    # Vertical line for the right and left tails
    right_tailz <- mean(used_df$observed_values) + 3*sd(used_df$observed_values)
    left_tailz <- mean(used_df$observed_values) - 3*sd(used_df$observed_values)
    
    # Plotting
    ggplot(data=used_df, aes(x=observed_values, fill="purple")) +
      geom_density(adjust=1.5, alpha=.4) +
      geom_vline(xintercept=right_tailz, linetype="dashed", color = "red") +
      geom_vline(xintercept=left_tailz, linetype="dashed", color = "red") +
      xlab(as.character(input$gvO_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 6. Z-Score outlier boxplot (before -> plot 2)
  output$gvoplot2 <- renderPlot({
    used_df <- gvoused_df()
    
    # Plotting
    used_df %>% 
      ggplot(aes(x = observed_values, y = observed_variables)) +
      geom_boxplot(outlier.color="red") +
      labs(x = "Value",
           y = "Observed Variables") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
  })
  
  ###### IQR Approach ######
  
  # Step 3. Interactive table description (Outlier -> IQR Approach)
  output$gvOIQRdataset <- renderDataTable({
    temp_df <- gvoused_df()
    
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(temp_df$observed_values)
    q1 <- as.numeric(quantile(temp_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(temp_df$observed_values, probs = 0.75))
    
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    newdata <- temp_df %>% 
      filter(observed_values > right_taili | observed_values < left_taili) %>% 
      dplyr::select(ReadTime, observed_variables, observed_values)
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 4. IQR Approach Table's Descriptive Statistics
  
  # Step 4.1. Variable Name
  # gvIQR_varname1 -> selected observed variable name
  output$gvIQR_varname1 <- renderText({
    as.character(input$gvO_observed_variables)
  })
  
  # Step 4.2. Descriptive Statistics
  # gvIQR_descstat1 -> create the selected observed variable descriptive statistic table
  output$gvIQR_descstat1 <- renderPrint({
    summary(gvofiltered_df()$observed_values)
  })
  
  # Step 4.3. Inter-Quartile Range (IQR)
  # gvIQR_iqr1 -> calculate the selected observed variable interquartile range
  output$gvIQR_iqr1 <- renderText({
    round(IQR(gvoused_df()$observed_values), 3)
  })
  
  # Step 4.4. Skewness before removing outliers
  # gvIQR_skewness1 -> calculate the selected observed variable skewness
  output$gvIQR_skewness1 <- renderText({
    round(skewness(gvoused_df()$observed_values), 3)
  })
  
  # Step 4.5. Upper Limit: Q3 + 1.5 * IQR
  # gvIQR_upper1 -> calculate upper limit by using z-score approach
  output$gvIQR_upper1 <- renderText({
    temp_df <- gvoused_df()
    
    # Calculate IQR, and Q3
    calc_iqr <- IQR(temp_df$observed_values)
    q3 <- as.numeric(quantile(temp_df$observed_values, probs = 0.75))
    
    round(q3 + 1.5 * calc_iqr, 3)
  })
  
  # Step 4.6. Lower Limit: Q1 - 1.5 * IQR
  # IQR_lower1 -> calculate lower limit by using z-score approach
  output$gvIQR_lower1 <- renderText({
    temp_df <- gvoused_df()
    
    # Calculate IQR and Q1
    calc_iqr <- IQR(temp_df$observed_values)
    q1 <- as.numeric(quantile(temp_df$observed_values, probs = 0.25))
    
    round(q1 - 1.5 * calc_iqr, 3)
  })
  
  # Step 5. IQR outlier density plot (before -> plot 1)
  output$gviqrplot1 <- renderPlotly({
    used_df <- gvoused_df()
    
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(used_df$observed_values)
    q1 <- as.numeric(quantile(used_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(used_df$observed_values, probs = 0.75))
    
    # Vertical line for the right and left tails
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    # Plotting
    ggplot(data=used_df, aes(x=observed_values, fill="purple")) +
      geom_density(adjust=1.5, alpha=.4) +
      geom_vline(xintercept=right_taili, linetype="dashed", color = "red") +
      geom_vline(xintercept=left_taili, linetype="dashed", color = "red") +
      xlab(as.character(input$gvO_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 6. IQR outlier boxplot (before -> plot 2)
  output$gviqrplot2 <- renderPlot({
    used_df <- gvoused_df()
    
    # Plotting
    used_df %>% 
      ggplot(aes(x = observed_values, y = observed_variables)) +
      geom_boxplot(outlier.color="red") +
      labs(x = "Value",
           y = "Observed Variables") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
  })
  
                      ##### Feature Selection Part #####
  
  # Step 1. Randomly sampled 10000 observations from the big dataset 
  # (To reduce the expensive running time for the Random Forest and Boruta methods)
  gvmini_selection_df <- reactive({
    # Set the random sample seed
    set.seed(8)
    
    mini_selection_df <- if (nrow(selection_df)  > 10000) {
      selection_df[sample(nrow(selection_df), 10000), ]
    } else {
      selection_df
    }
    
    return(mini_selection_df)
    
  })
  
  # Step 2a. Linear Regression Method
  
  # Fit the model
  gvlmMod <- reactive({
    # Step 2a.1. Set Seed
    set.seed(8)
    
    # Step 2a.2. Fitting the model
    lmMod <- lm(as.formula(paste(colnames(selection_df)[1]," ~ ", paste(input$FS_explanatory_variable,collapse="+"))), data = selection_df)  # fit lm() model
    
    return(lmMod)
  })
  
  # Step 2a.3. Calculate and print the variable importance
  
  # Calculate the variable importance
  gvrelImportance <- reactive({
    gvlmMod = gvlmMod()
    
    # Step 2a.4. Calculate the variable importance
    relImportance <- varImp(gvlmMod, scale = FALSE)  # calculate Variable importance scaled to 100
  })
  
  # Step 2a.5. Visualize variable importance
  output$gvfsplot1 <- renderPlotly({
    gvrelImportance = gvrelImportance()
    
    # Get variable importance from the model fit
    gvImpDataRI <- as.data.frame(gvrelImportance) %>% 
      rename(VImportance = "Overall") %>% 
      arrange(desc(VImportance))
    gvImpDataRI$Var_Names <- row.names(gvImpDataRI)
    
    # Plotting
    ggplot(gvImpDataRI, aes(x=Var_Names, y=VImportance)) +
      geom_segment( aes(x=Var_Names, xend=Var_Names, y=0, yend=VImportance), color="skyblue") +
      geom_point(color="blue", alpha=0.6) +
      coord_flip() +
      xlab("Variable") +
      ylab("Variable Importance") +
      theme(
        legend.position="bottom",
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
  
  
  # Step 2b. Random Forest Method
  # Ref: https://hackernoon.com/random-forest-regression-in-r-code-and-interpretation
  
  # Fit the model
  gvrf_fit <- reactive({
    gvmini_selection_df = gvmini_selection_df()
    
    # Step 2b.1. Set Seed
    set.seed(8)
    
    # Step 2b.2. Fit the random forest model with mtry = 10 and ntree = 50
    rf_fit <- randomForest(as.formula(paste(colnames(gvmini_selection_df)[1]," ~ ", paste(input$gvFS_explanatory_variable,collapse="+"))), data=gvmini_selection_df, mtry=as.integer(input$gvrffs_mtry), ntree=as.integer(input$gvrffs_ntree),
                           keep.forest=FALSE, importance=TRUE)
    
    return(rf_fit)
  })
  
  # Step 2b.3. Visualize variable importance
  output$gvfsplot2 <- renderPlotly({
    gvrf_fit = gvrf_fit()
    
    # Get variable importance from the model fit
    gvImpData <- as.data.frame(importance(gvrf_fit))
    gvImpData$Var_Names <- row.names(gvImpData)
    
    # Plotting
    ggplot(gvImpData, aes(x=Var_Names, y=`%IncMSE`)) +
      geom_segment( aes(x=Var_Names, xend=Var_Names, y=0, yend=`%IncMSE`), color="skyblue") +
      geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
      coord_flip() +
      xlab("Variable") +
      theme(
        legend.position="bottom",
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
  
  
  # Step 2c. MARS Method
  
  # Fit the model
  gvmarsModel <- reactive({
    # Step 2c.1. Set Seed
    set.seed(8)
    
    # Step 2c.2. Fitting the model
    marsModel <- earth(as.formula(paste(colnames(selection_df)[1]," ~ ", paste(input$FS_explanatory_variable,collapse="+"))), data=selection_df) # build model
    
    return(marsModel)
  })
  
  # Step 2c.3. Estimate the variable importance
  gvev <- reactive({
    gvmarsModel = gvmarsModel()
    
    gvev <- evimp(gvmarsModel) # estimate variable importance
    
    return(gvev)
    
  })
  
  # Step 2c.4. Visualize variable importance
  output$gvfsplot3 <- renderPlot({
    gvev = gvev()
    
    plot(gvev)
  })
  
  
  
  # Step 2d. Boruta Method
  
  # Fitting the model
  gvboruta_output <- reactive({
    gvmini_selection_df = gvmini_selection_df()
    
    # Step 2d.1. Set Seed
    set.seed(8)
    
    # Step 2d.2. Fitting the model and calculate the variable importance
    boruta_output <- Boruta(as.formula(paste(colnames(gvmini_selection_df)[1]," ~ ", paste(input$gvFS_explanatory_variable,collapse="+"))), data=gvmini_selection_df, doTrace=2, maxRuns=25)  # perform Boruta search
    
    return(boruta_output)
  })
  
  # Step 2d.3. Plotting the variable importance
  output$gvfsplot4 <- renderPlot({
    gvboruta_output = gvboruta_output()
    
    # Step 2d.3. Plotting the variable importance
    plot(gvboruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance
  })
  
  
                            ##### Trend Part #####
  
  # Step 1. Create trend_df -> the subset of pv dataset after filtered by date and variable
  gvtrend_df <- reactive({
    newdata <- pv %>% 
      filter(between(PVdate, input$gvtrend_dates[1], input$gvtrend_dates[2])) %>% # Date
      dplyr::select(c(ReadTime, !!input$gvtrend_observed_variables)) %>% # filter column
      na.omit() # remove NAs
    
    return(newdata)
    
  })
  
  # Step 2. Change the trend_df into time series (ts) object
  gvtseries_ts <- reactive({
    gvtrend_df = gvtrend_df()
    
    # Step 2a. Convert ReadTime into datetime data type by using Lubridate package 
    gvtrenddatetime_df <- gvtrend_df
    gvtrenddatetime_df$ReadTime <- dmy_hm(gvtrenddatetime_df$ReadTime)
    
    # Step 2b. Convert dataframe to time series
    gvtseries <- read.zoo(gvtrenddatetime_df)
    
    # Step 2c. Convert to ts object
    gvtseries_ts <- ts(gvtseries, frequency = 96)
    
    return(gvtseries_ts)
    
  })
  
  # Step 3. Decompose the trend
  gvdecomposed_tseries_ts <- reactive({
    gvtseries_ts = gvtseries_ts()
    
    gvdecomposed_tseries_ts <- decompose(gvtseries_ts,type='multiplicative')
    
  })
  
  # Step 4. Plotting
  output$gvtrendplot1 <- renderPlot({
    gvdecomposed_tseries_ts = gvdecomposed_tseries_ts()
    
    plot(gvdecomposed_tseries_ts)
  })
  
  # From Global View Page to Analysis Page
  observeEvent(input$gv_to_distribution, {
    updateTabItems(session, "tabs", selected = "dash3")
  })
  
  # From Global View Page to Analysis Page
  observeEvent(input$gv_to_missing, {
    updateTabItems(session, "tabs", selected = "dash3")
  })
  
  # From Global View Page to Analysis Page
  observeEvent(input$gv_to_outlier, {
    updateTabItems(session, "tabs", selected = "dash3")
  })
  
  # From Global View Page to Analysis Page
  observeEvent(input$gv_to_feature_selection, {
    updateTabItems(session, "tabs", selected = "dash3")
  })
  
  # From Global View Page to Analysis Page
  observeEvent(input$gv_to_trend, {
    updateTabItems(session, "tabs", selected = "dash3")
  })

  # From Global View Page to About Page
  observeEvent(input$link_to_about, {
    updateTabItems(session, "tabs", selected = "dash4")
  })
  
  ################################ ANALYSIS ####################################
  ########################### ANALYSIS DISTRIBUTION ############################
  
  # Step 1. Create dfiltered_df by filtering observed.variables, Date, and PVtime
  dfiltered_df <- reactive({ 
    
    newdata <- pv_data %>% 
      filter(observed_variables %in% input$D_observed_variables) %>% # filter observed.variables
      filter(between(PVdate, input$D_dates[1], input$D_dates[2])) %>% # filter Date
      filter(PVtime %in% input$D_pvtime) # filter PVtime
    
    return(newdata)
  })
  
  # Step 2. Missing Value DF
  rmv_na_df <- reactive({
    # Since it is a transformation method, we need to remove the missing values before plotting to avoid errors
    temp_df <- dfiltered_df() %>% 
      # Remove NA(s) / Missing Value(s)
      na.omit()
    
    return(temp_df)
  })
  
  # Step 3. Remove Zero Observation(s) Option
  rmv_zero_df <- reactive({
    
    temp_df <- rmv_na_df()
    
    newdata <- temp_df %>% 
      # Remove 0 observation(s)
      filter(if (input$D_zero=="Yes") {
        observed_values != 0
      } else if (input$D_zero=="No") {
        observed_values != ""
      })
    
    return(newdata)
    
  })
  
  # Step 4. Remove Negative Value(s) Option
  rmv_negative_df <- reactive({
    
    temp_df <- rmv_zero_df()
    
    newdata <- temp_df %>% 
      # Remove negative value(s)
      filter(if (input$D_neg=="Yes") {
        observed_values >= 0
      } else if (input$D_neg=="No") {
        observed_values != ""
      }) 
    
    return(newdata)
    
  })
  
  # Step 5. Remove Outlier(s) Option
  rmv_outlier_df <- reactive({
    
    temp_df <- rmv_negative_df()
    
    # Z-Score Calculation
    right_tailz <- mean(temp_df$observed_values) + 3*sd(temp_df$observed_values)
    left_tailz <- mean(temp_df$observed_values) - 3*sd(temp_df$observed_values)
    
    # IQR Calculation
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(temp_df$observed_values)
    q1 <- as.numeric(quantile(temp_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(temp_df$observed_values, probs = 0.75))
    
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    newdata <- temp_df %>% 
      # Remove outlier(s) with method
      filter(if (input$D_outlier=="No") {
        observed_values != ""
      } else if (input$D_outlier=="Yes, Z-Score Approach") {
        observed_values < right_tailz & observed_values > left_tailz
      } else if (input$D_outlier=="Yes, Inter-Quartile Range (IQR) Approach") {
        observed_values < right_taili & observed_values > left_taili
      })
    
    return(newdata)
    
  })
  
  # Step 6. Transformation Method option
  dused_df1 <- reactive({
    
    newdata <- rmv_outlier_df()
    
    if (input$D_transformation=="Raw") {
      return(newdata)
    } else if (input$D_transformation=="Square") {
      temp_df <- newdata %>% 
        mutate(observed_values = observed_values^2)
      
      return(temp_df)
    } else if (input$D_transformation=="Cube") {
      temp_df <- newdata %>% 
        mutate(observed_values = observed_values^3)
      
      return(temp_df)
    } else if (input$D_transformation=="Square Root") {
      temp_df <- newdata %>% 
        mutate(observed_values = sqrt(observed_values))
      
      return(temp_df)
    } else if (input$D_transformation=="Cube Root") {
      temp_df <- newdata %>% 
        mutate(observed_values = observed_values^(1/3))
      
      return(temp_df)
    } else if (input$D_transformation=="Logarithm (Base 10)") {
      temp_df <- newdata %>% 
        mutate(observed_values = log10(observed_values))
      
      return(temp_df)
    } else if (input$D_transformation=="Natural Logarithm") {
      temp_df <- newdata %>% 
        mutate(observed_values = log(observed_values))
      
      return(temp_df)
    }
    
  })
  
  # Step 7. Distribution of Density Plot
  output$Dplot1 <- renderPlotly({
    used_df <- dused_df1()
    
    ggplot(data=used_df, aes(x=observed_values, fill="purple")) + 
      geom_density(adjust=1.5, alpha=.4) +
      xlab(as.character(input$D_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 8. Distribution Plot's Descriptive Statistics
  
  # Step 8.1. Variable Name
  # D_text1 -> selected observed variable name
  output$D_text1 <- renderText({
    as.character(input$D_observed_variables)
  })
  
  # Step 8.2. Number of Observations
  # D_text2 -> count number of observed cells
  output$D_text2 <- renderText({
    nrow(dused_df1())
  })
  
  # Step 8.3. Descriptive Statistics
  # D_text3 -> create the selected observed variable descriptive statistic table
  output$D_text3 <- renderPrint({
    summary(dused_df1()$observed_values)
  })
  
  # Step 8.4. Standard Deviation
  # D_text4 -> calculate the selected observed variable standard deviation
  output$D_text4 <- renderText({
    round(sd(dused_df1()$observed_values), 3)
  })
  
  # Step 8.5. Skewness
  # D_text5 -> calculate the selected observed variable skewness
  output$D_text5 <- renderText({
    round(skewness(dused_df1()$observed_values), 3)
  })
  
  # Step 9. Negative Values Table
  output$D_dataset <- renderDataTable({
    newdata <- dfiltered_df()
    
    newdata <- newdata %>% 
      # Filter negative value(s)
      filter(observed_values < 0) %>% 
      dplyr::select(ReadTime, observed_variables, observed_values)
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 10. Negative Values Table's Descriptive Statistics
  
  # Step 10a. Create negative_df
  negative_df <- reactive({
    
    temp_df <- dfiltered_df() %>% 
      # Filter negative value(s)
      filter(observed_values < 0)
    
    return(temp_df)
  })
  
  # Step 10.1. Variable Name
  # D_neg1 -> selected observed variable name
  output$D_neg1 <- renderText({
    as.character(input$D_observed_variables)
  })
  
  # Step 10.2. Number of Observations
  # D_neg2 -> count number of observed cells
  output$D_neg2 <- renderText({
    nrow(negative_df())
  })
  
  # Step 10.3. Descriptive Statistics
  # D_neg3 -> create the selected observed variable descriptive statistic table
  output$D_neg3 <- renderPrint({
    summary(negative_df()$observed_values)
  })
  
  # Step 10.4. Standard Deviation
  # D_neg4 -> calculate the selected observed variable standard deviation
  output$D_neg4 <- renderText({
    round(sd(negative_df()$observed_values), 3)
  })
  
  # Step 10.5. Skewness
  # D_neg5 -> calculate the selected observed variable skewness
  output$D_neg5 <- renderText({
    round(skewness(negative_df()$observed_values), 3)
  })
  
  # Step 11. Zero Value Table
  output$zero_dataset <- renderDataTable({
    newdata <- dfiltered_df()
    
    newdata <- newdata %>% 
      # Filter negative value(s)
      filter(observed_values == 0) %>% 
      dplyr::select(ReadTime, observed_variables, observed_values)
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 12. Zero Value Count Plot
  output$Dplot3 <- renderPlotly({
    
    # Step 12.1. Create Wider format dataset after filtering only zero values
    zero_df_step12 <- dfiltered_df() %>%
      filter(observed_values == 0) %>% 
      dplyr::select(PVtime, observed_values) %>% 
      count(PVtime, sort = TRUE) %>% 
      rename(zero_count = n)
    
    # Step 12.2. Plotting
    p <- zero_df_step12 %>% 
      ggplot(aes(x = zero_count, y = PVtime, fill = as.factor(PVtime),
                 text=paste("Time: ", PVtime,
                            "\nZero Value: ", zero_count))) +
      geom_bar(stat="identity", alpha=.6, width=.4) +
      xlab("Number of Zero Values") +
      ylab("Time") +
      theme(legend.position="none") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Step 13. Time Series Plot
  
  # Step 13.a. Create datetime_df
  datetime_df <- reactive({
    
    datetime_df <- dfiltered_df()
    
    # Step 13.1. Convert ReadTime into datetime data type by using Lubridate package 
    datetime_df$ReadTime <- dmy_hm(datetime_df$ReadTime)
    
    # Step 13.2. Create dummy if NA = Missing, and present = Present
    datetime_df$missing_dummy <- ifelse(is.na(datetime_df$observed_values), "Missing", "Present")
    
    return(datetime_df)
  })
  
  # Step 13.b. Plotting (Without Interactive Feature)
  output$Dplot4 <- renderPlot({
    
    temp_df <- datetime_df()
    
    # Step 13.b. Plotting (Without Interactive Feature)
    ggplot(temp_df, aes(x = ReadTime, y = observed_values, group = 1)) +
      geom_line(color="#0288d1") +
      geom_vline(xintercept = temp_df$ReadTime[temp_df$missing_dummy == "Missing"], color = "red") +
      xlab("Time") +
      ylab(as.character(input$D_observed_variables)) +
      scale_color_discrete(name = "Status") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
  # Step 13.c. Plotting (With Interactive Feature)
  output$Dplot5 <- renderPlotly({
    
    temp_df <- datetime_df()
    
    # Step 13.c. Plotting (With Interactive Feature)
    p <- ggplot(temp_df, aes(x = ReadTime, y = observed_values, group = 1, 
                                 text=paste("Date: ", PVdate,
                                            "\nTime: ", PVtime,
                                            "\nValue: ", observed_values,
                                            "\nStatus: ", missing_dummy))) +
      geom_line(color="#0288d1") +
      xlab("Time") +
      ylab(as.character(input$D_observed_variables)) +
      scale_color_discrete(name = "Status") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
    
    ggplotly(p, tooltip = "text")
    
  })
  
  # From Analysis Page to Global View Page
  observeEvent(input$distribution_to_gv, {
    updateTabItems(session, "tabs", selected = "dash2")
  })
  
  ########################### ANALYSIS MISSING VALUE ###########################
  
  # Step 1. Create filtered_df by filtering observed.variables, Date, and PVtime
  filtered_df <- reactive({ 
    
    newdata <- pv_data %>% 
      filter(observed_variables %in% input$MV_observed_variables) %>% # observed.variables
      filter(between(PVdate, input$MV_dates[1], input$MV_dates[2])) %>% # Date
      filter(PVtime %in% input$MV_pvtime) # PVtime
    
    return(newdata)
  })
  
  # Step 2. Creating Missing Value Table
  
  # Step 2.1. Reformat filtered_df from long to wide format to calculate the number of missing rows easily
  filtered_df_wider <- reactive({
    
    filtered_df <- filtered_df()
    
    newdata <- filtered_df %>% 
      # Change from long into wide format
      pivot_wider(names_from = observed_variables, values_from = observed_values) %>% 
      # Deselect helper variables
      dplyr::select(!c(PVdate, dayofmonth, month, year, timefactor, AMorPM, PVtime))
    
    return(newdata)
  })
  
  # Step 2.2. Creating Missing Value Dataframe
  missing_df <- reactive({ 
    filtered_df_wider <- filtered_df_wider()
    
    newdata <- as_tibble(lapply(filtered_df_wider, function(x) sum(is.na(x)))) %>% 
      gather(key = "variable_name", value = "missing_count") %>% 
      # Calculate the total missing values category in the dataset
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~'Total')))
    
    return(newdata)
  })
  
  # Step 2.3. Send the output of missing_df in step 2.2. above to the frontend after filter out the
  # Total category that we created previously
  output$MVdataset <- renderDataTable({
    newdata <- missing_df()
    
    # Filter out the Total category that we created previously
    newdata <- newdata %>% 
      filter(variable_name != "Total")
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 3. Missing Value Table's Descriptive Statistics
  
  # Step 3.1. Create filtered_df_nona to count problematic row (i.e., there is an existence of missing values in one or more columns)
  filtered_df_nona <- reactive({
    
    filtered_df_wider <- filtered_df_wider()
    # Remove missing value in the filtered_df_wider dataframe to find # of rows that is fine (i.e., no missing value)
    newdata <- na.omit(filtered_df_wider)
    
    return(newdata)
  })
  
  # Step 3.2. Number of observations
  # MV_textA -> count total observations
  output$MV_textA <- renderText({
    nrow(filtered_df_wider())
  })
  
  # Step 3.3. Total problematic rows
  # MV_textB -> count number of problematic rows (i.e., has missing value)
  output$MV_textB <- renderText({
    nrow(filtered_df_wider()) - nrow(filtered_df_nona())
  })
  
  # Step 3.4. Proportion of problematic rows
  # MV_textC -> count the percentage of problematic rows (i.e., has missing value)
  output$MV_textC <- renderText({
    x <- nrow(filtered_df_wider())
    y <- nrow(filtered_df_wider()) - nrow(filtered_df_nona())
    z <- round((y / x) * 100, 2)
    
    # Concatenate string
    paste(as.character(z), "%", sep = "")
  })
  
  # Step 3.5. Number of cells
  # MV_text1 -> count number of observed cells
  output$MV_text1 <- renderText({
    nrow(filtered_df())
  })
  
  # Step 3.6. Total missing by the number of cells
  # MV_text2 -> count number of missing values in the observed cells
  output$MV_text2 <- renderText({
    missing_df <- missing_df()
    
    as.integer(missing_df[missing_df$variable_name == "Total", "missing_count"])
  })
  
  # Step 3.7. Proportion of missing by the number of cells
  # MV_text3 -> count the percentage of missing values (by cells)
  output$MV_text3 <- renderText({
    missing_df <- missing_df()
    filtered_df <- filtered_df()
    
    x <- round(as.integer(missing_df[missing_df$variable_name == "Total", "missing_count"]) / nrow(filtered_df) * 100, 2)
    
    # Concatenate string
    paste(as.character(x), "%", sep = "")
  })
  
  # Step 4. Missing Value Plot 1 (Count Plot)
  output$plot1 <- renderPlotly({
    
    # Filter out Total category in the variable_name variable
    missing_df <- missing_df() %>% 
      filter(variable_name != "Total")
    
    p <- missing_df %>% 
      # Order by missing_count
      arrange(missing_count) %>% 
      mutate(variable_name=factor(variable_name, levels=variable_name)) %>% 
      ggplot(aes(x=variable_name, y=missing_count,
                 text=paste("Building Name: ", variable_name,
                            "\nMissing Value: ", missing_count))) +
      geom_segment(aes(x=variable_name, xend=variable_name, y=0, yend=missing_count), color="black") +
      geom_point(color="orange", size=1, alpha=1) +
      theme_light() +
      coord_flip() +
      xlab("Variable") +
      ylab("Number of Missing Values") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Step 5. Missing Value Plot 2 (Proportion Plot)  
  output$plot2 <- renderPlotly({
    
    # Create new dataframe that include the calculation of proportion of the missing values for each variable
    newdata <- missing_df() %>% 
      rename(Missing = missing_count) %>% 
      filter(variable_name != "Total") %>% 
      mutate(Present = nrow(filtered_df_wider()) - Missing) %>% 
      pivot_longer(cols = !c("variable_name"),
                   names_to = "isna",
                   values_to = "total_count") %>% 
      mutate(pct = total_count / nrow(filtered_df_wider()) * 100)
    
    # Sort from highest to lowest
    levels <-
      (newdata  %>% filter(isna == "Missing") %>% arrange(pct))$variable_name
    
    # Plotting
    percentage_plot <- newdata %>%
      ggplot(aes(x = reorder(variable_name, pct), 
                 y = pct, fill=isna,
                 text=paste("Building Name: ", variable_name,
                            "\nStatus: ", isna,
                            "\nCount: ", total_count,
                            "\nPercentage: ", paste(round(pct, 2), "%", sep = "")))) +
      geom_bar(stat = 'identity', alpha=0.8) +
      scale_x_discrete(limits = levels) +
      scale_fill_manual(name = "", 
                        values = c('tomato3', 'steelblue')) +
      coord_flip() +
      labs(x ='Variable', 
           y = "Percentage of missing values") +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
    
    ggplotly(percentage_plot, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
    
  })
  
  # Step 6. Missing Value Plot 3 (Stacked Bar-Chart / Where the Missing Plot)
  output$plot3 <- renderPlot({
    
    # Create new dataframe that include the calculation of proportion of the missing values for each variable
    newdata <- missing_df() %>% 
      rename(Missing = missing_count) %>% 
      filter(variable_name != "Total") %>% 
      mutate(Present = nrow(filtered_df_wider()) - Missing) %>% 
      pivot_longer(cols = !c("variable_name"),
                   names_to = "isna",
                   values_to = "total_count") %>% 
      mutate(pct = total_count / nrow(filtered_df_wider()) * 100)
    
    # Sort from highest to lowest
    levels <-
      (newdata  %>% filter(isna == "Missing") %>% arrange(pct))$variable_name
    
    # Plotting
    filtered_df_wider() %>%
      mutate(id = row_number()) %>%
      gather(-id, key = "key", value = "val") %>%
      mutate(isna = is.na(val)) %>%
      ggplot(aes(x = key, y = id, fill = isna)) +
      geom_tile() +
      scale_fill_manual(name = "",
                        values = c('steelblue', 'tomato3'),
                        labels = c("Present", "Missing"))  +
      scale_x_discrete(limits = levels) +
      labs(x = "Variable",
           y = "Row Number") +
      coord_flip() +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
  # Step 7. Missing Value Plot 4 (Exploring patterns with UpSetR)
  output$plot4 <- renderPlot({
    filtered_df <- filtered_df()
    
    newdata <- filtered_df %>% 
      filter(observed_variables %in% input$MV_observed_variables1) %>% 
      # Change into pivot wider
      pivot_wider(names_from = observed_variables, values_from = observed_values) %>% 
      dplyr::select(!c(PVdate, dayofmonth, month, year, timefactor, AMorPM, PVtime))
    
    gg_miss_upset(newdata, nsets = 5,
                  mainbar.y.label="Number of NAs based on Intersection Size (Group)")
  })
  
  # Step 8. Missing Value Plot 5 (Missingness across Factors Plot)
  output$plot5 <- renderPlotly({
    filtered_df <- filtered_df()
    
    newdata <- filtered_df %>% 
      pivot_wider(names_from = observed_variables, values_from = observed_values) %>% 
      dplyr::select(!c(PVdate, timefactor))
    
    gg_miss_fct(x = newdata, fct = !!input$MV_observed_variables2) +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.x = element_text(angle = 0)) +
      ylab("Variable")
  })
  
  # From Analysis Page to Global View Page
  observeEvent(input$missing_to_gv, {
    updateTabItems(session, "tabs", selected = "dash2")
  })
  
  ########################### ANALYSIS OUTLIER #################################

  # Step 1. Create ofiltered_df by filtering observed_variables, PVdate, and PVtime
  ofiltered_df <- reactive({ 

    newdata <- pv_data %>% 
      filter(observed_variables %in% input$O_observed_variables) %>% # observed_variables
      filter(between(PVdate, input$O_dates[1], input$O_dates[2])) %>% # PVdate
      filter(PVtime %in% input$O_pvtime) # PVtime
    
    return(newdata)
  })
  
  # Step 2. Create oused_df dataframe for plotting histograms and boxplots (after removing NAs)
  oused_df <- reactive({ 
    
    newdata <- ofiltered_df() %>% 
      na.omit()
    
    return(newdata)
  })
  
                          ##### Z-Score Approach #####
  
  # Step 3. Interactive table description (Outlier -> Z-Score Approach)
  output$OZdataset <- renderDataTable({
    temp_df <- oused_df()
    
    right_tailz <- mean(temp_df$observed_values) + 3*sd(temp_df$observed_values)
    left_tailz <- mean(temp_df$observed_values) - 3*sd(temp_df$observed_values)
    
    newdata <- temp_df %>% 
      filter(observed_values > right_tailz | observed_values < left_tailz) %>% 
      dplyr::select(ReadTime, observed_variables, observed_values)
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 4. Z-Score Approach Table's Descriptive Statistics
  
  # Step 4.1. Variable Name
  # O_varname1 -> selected observed variable name
  output$O_varname1 <- renderText({
    as.character(input$O_observed_variables)
  })
  
  # Step 4.2. Descriptive Statistics
  # O_descstat1 -> create the selected observed variable descriptive statistic table
  output$O_descstat1 <- renderPrint({
    summary(ofiltered_df()$observed_values)
  })
  
  # Step 4.3. Standard Deviation
  # O_std1 -> calculate the selected observed variable standard deviation
  output$O_std1 <- renderText({
    round(sd(oused_df()$observed_values), 3)
  })
  
  # Step 4.4. Skewness before removing outliers
  # O_skewness1 -> calculate the selected observed variable skewness
  output$O_skewness1 <- renderText({
    round(skewness(oused_df()$observed_values), 3)
  })
  
  # Step 4.5. Upper Limit: mean + 3 * stdev
  # O_upper1 -> calculate upper limit by using z-score approach
  output$O_upper1 <- renderText({
    round(mean(oused_df()$observed_values) + 3*sd(oused_df()$observed_values), 3)
  })
  
  # Step 4.6. Lower Limit: mean - 3 * stdev
  # O_lower1 -> calculate lower limit by using z-score approach
  output$O_lower1 <- renderText({
    round(mean(oused_df()$observed_values) - 3*sd(oused_df()$observed_values), 3)
  })

  # Step 5. Z-Score outlier density plot (before -> plot 1)
  output$oplot1 <- renderPlotly({
    used_df <- oused_df()
    
    # Vertical line for the right and left tails
    right_tailz <- mean(used_df$observed_values) + 3*sd(used_df$observed_values)
    left_tailz <- mean(used_df$observed_values) - 3*sd(used_df$observed_values)
    
    # Plotting
    ggplot(data=used_df, aes(x=observed_values, fill="purple")) +
      geom_density(adjust=1.5, alpha=.4) +
      geom_vline(xintercept=right_tailz, linetype="dashed", color = "red") +
      geom_vline(xintercept=left_tailz, linetype="dashed", color = "red") +
      xlab(as.character(input$O_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 6. Z-Score outlier boxplot (before -> plot 2)
  output$oplot2 <- renderPlot({
    used_df <- oused_df()
    
    # Plotting
    used_df %>% 
      ggplot(aes(x = observed_values, y = observed_variables)) +
      geom_boxplot(outlier.color="red") +
      labs(x = "Value",
           y = "Observed Variables") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
  })
  
  # Step 7. Z-Score outlier density plot (after -> plot 3)
  output$oplot3 <- renderPlotly({
    used_df <- oused_df()
    
    # Vertical line for the right and left tails
    right_tailz <- mean(used_df$observed_values) + 3*sd(used_df$observed_values)
    left_tailz <- mean(used_df$observed_values) - 3*sd(used_df$observed_values)
    
    # Plotting
    used_df %>% 
      filter(observed_values < right_tailz & observed_values > left_tailz) %>% 
      ggplot(aes(x=observed_values, fill="purple")) +
        geom_density(adjust=1.5, alpha=.4) +
        geom_vline(xintercept=right_tailz, linetype="dashed", color = "red") +
        geom_vline(xintercept=left_tailz, linetype="dashed", color = "red") +
        xlab(as.character(input$O_observed_variables)) +
        ylab("Density") +
        theme(panel.background = element_rect(fill = "linen")) +
        theme(plot.background = element_rect(fill = "linen")) +
        theme(legend.position = "none")
  })
  
  # Step 8. Z-Score outlier boxplot (after -> plot 4)
  output$oplot4 <- renderPlot({
    used_df <- oused_df()
    
    # Vertical line for the right and left tails
    right_tailz <- mean(used_df$observed_values) + 3*sd(used_df$observed_values)
    left_tailz <- mean(used_df$observed_values) - 3*sd(used_df$observed_values)
    
    # Plotting
    used_df %>% 
      filter(observed_values < right_tailz & observed_values > left_tailz) %>%
      ggplot(aes(x = observed_values, y = observed_variables)) +
      geom_boxplot(outlier.color="red") +
      labs(x = "Value",
           y = "Observed Variables") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
  })
  
  
                        ###### IQR Approach ######
  
  # Step 3. Interactive table description (Outlier -> IQR Approach)
  output$OIQRdataset <- renderDataTable({
    temp_df <- oused_df()
    
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(temp_df$observed_values)
    q1 <- as.numeric(quantile(temp_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(temp_df$observed_values, probs = 0.75))
    
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    newdata <- temp_df %>% 
      filter(observed_values > right_taili | observed_values < left_taili) %>% 
      dplyr::select(ReadTime, observed_variables, observed_values)
    
    datatable(data = newdata, options = list(
      pageLength = 5,
      lengthMenu = c(5,10)
    ))
  })
  
  # Step 4. IQR Approach Table's Descriptive Statistics
  
  # Step 4.1. Variable Name
  # IQR_varname1 -> selected observed variable name
  output$IQR_varname1 <- renderText({
    as.character(input$O_observed_variables)
  })
  
  # Step 4.2. Descriptive Statistics
  # IQR_descstat1 -> create the selected observed variable descriptive statistic table
  output$IQR_descstat1 <- renderPrint({
    summary(ofiltered_df()$observed_values)
  })
  
  # Step 4.3. Inter-Quartile Range (IQR)
  # IQR_iqr1 -> calculate the selected observed variable interquartile range
  output$IQR_iqr1 <- renderText({
    round(IQR(oused_df()$observed_values), 3)
  })
  
  # Step 4.4. Skewness before removing outliers
  # IQR_skewness1 -> calculate the selected observed variable skewness
  output$IQR_skewness1 <- renderText({
    round(skewness(oused_df()$observed_values), 3)
  })
  
  # Step 4.5. Upper Limit: Q3 + 1.5 * IQR
  # IQR_upper1 -> calculate upper limit by using z-score approach
  output$IQR_upper1 <- renderText({
    temp_df <- oused_df()
    
    # Calculate IQR, and Q3
    calc_iqr <- IQR(temp_df$observed_values)
    q3 <- as.numeric(quantile(temp_df$observed_values, probs = 0.75))
    
    round(q3 + 1.5 * calc_iqr, 3)
  })
  
  # Step 4.6. Lower Limit: Q1 - 1.5 * IQR
  # IQR_lower1 -> calculate lower limit by using z-score approach
  output$IQR_lower1 <- renderText({
    temp_df <- oused_df()
    
    # Calculate IQR and Q1
    calc_iqr <- IQR(temp_df$observed_values)
    q1 <- as.numeric(quantile(temp_df$observed_values, probs = 0.25))
    
    round(q1 - 1.5 * calc_iqr, 3)
  })
  
  # Step 5. IQR outlier density plot (before -> plot 1)
  output$iqrplot1 <- renderPlotly({
    used_df <- oused_df()
    
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(used_df$observed_values)
    q1 <- as.numeric(quantile(used_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(used_df$observed_values, probs = 0.75))
    
    # Vertical line for the right and left tails
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    # Plotting
    ggplot(data=used_df, aes(x=observed_values, fill="purple")) +
      geom_density(adjust=1.5, alpha=.4) +
      geom_vline(xintercept=right_taili, linetype="dashed", color = "red") +
      geom_vline(xintercept=left_taili, linetype="dashed", color = "red") +
      xlab(as.character(input$O_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 6. IQR outlier boxplot (before -> plot 2)
  output$iqrplot2 <- renderPlot({
    used_df <- oused_df()
    
    # Plotting
    used_df %>% 
      ggplot(aes(x = observed_values, y = observed_variables)) +
      geom_boxplot(outlier.color="red") +
      labs(x = "Value",
           y = "Observed Variables") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
  })
  
  # Step 7. IQR outlier density plot (after -> plot 3)
  output$iqrplot3 <- renderPlotly({
    used_df <- oused_df()
    
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(used_df$observed_values)
    q1 <- as.numeric(quantile(used_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(used_df$observed_values, probs = 0.75))
    
    # Vertical line for the right and left tails
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    # Plotting
    used_df %>% 
      filter(observed_values < right_taili & observed_values > left_taili) %>% 
      ggplot(aes(x=observed_values, fill="purple")) +
      geom_density(adjust=1.5, alpha=.4) +
      geom_vline(xintercept=right_taili, linetype="dashed", color = "red") +
      geom_vline(xintercept=left_taili, linetype="dashed", color = "red") +
      xlab(as.character(input$O_observed_variables)) +
      ylab("Density") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(legend.position = "none")
  })
  
  # Step 8. Z-Score outlier boxplot (after -> plot 4)
  output$iqrplot4 <- renderPlot({
    used_df <- oused_df()
    
    # Calculate IQR, Q3, and Q1
    calc_iqr <- IQR(used_df$observed_values)
    q1 <- as.numeric(quantile(used_df$observed_values, probs = 0.25))
    q3 <- as.numeric(quantile(used_df$observed_values, probs = 0.75))
    
    # Vertical line for the right and left tails
    right_taili <- q3 + 1.5 * calc_iqr
    left_taili <- q1 - 1.5 * calc_iqr
    
    # Plotting
    used_df %>% 
      filter(observed_values < right_taili & observed_values > left_taili) %>%
      ggplot(aes(x = observed_values, y = observed_variables)) +
      geom_boxplot(outlier.color="red") +
      labs(x = "Value",
           y = "Observed Variables") +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen")) +
      theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))
  })
  
  # From Analysis Page to Global View Page
  observeEvent(input$outlier_to_gv, {
    updateTabItems(session, "tabs", selected = "dash2")
  })
  
  ########################### FEATURE SELECTION ################################
  
  # Step 1. Randomly sampled 10000 observations from the big dataset 
  # (To reduce the expensive running time for the Random Forest and Boruta methods)
  mini_selection_df <- reactive({
    # Set the random sample seed
    set.seed(8)
    
    mini_selection_df <- if (nrow(selection_df)  > 10000) {
      selection_df[sample(nrow(selection_df), 10000), ]
    } else {
      selection_df
    }
    
    return(mini_selection_df)
    
  })
  
  # Step 2a. Linear Regression Method
  
  # Print the dependent variable in the App
  # ri_dependent_variable -> first column after ReadTime is always the dependent variable
  output$ri_dependent_variable <- renderText({
    as.character(colnames(selection_df)[1])
  })
  
  # Print the independent variable in the App
  # ri_independent_variable -> the filtered dependent variables
  output$ri_independent_variable <- renderText({
    as.character(paste(input$FS_explanatory_variable,collapse=", "))
  })
  
  # Fit the model
  lmMod <- reactive({
    # Step 2a.1. Set Seed
    set.seed(8)
    
    # Step 2a.2. Fitting the model
    lmMod <- lm(as.formula(paste(colnames(selection_df)[1]," ~ ", paste(input$FS_explanatory_variable,collapse="+"))), data = selection_df)  # fit lm() model
    
    return(lmMod)
  })
  
  # Step 2a.3. Print the fitted linear model
  # ri_model_output -> the output of the fitted linear model
  output$ri_model_output <- renderPrint({
    lmMod()
  })
  
  # Step 2a.4. Calculate and print the variable importance
  
  # Calculate the variable importance
  relImportance <- reactive({
    lmMod = lmMod()
    
    # Step 2a.4. Calculate and print the variable importance
    relImportance <- varImp(lmMod, scale = FALSE)  # calculate variable importance scaled to 100
  })
  
  # ri_estimation <- the output of the variable importance estimation
  output$ri_estimation <- renderPrint({
    relImportance = relImportance()
    
    # Sort in descending order
    relImportance %>% arrange(desc(Overall))  # Variable importance
  })
  
  # Step 2a.5. Visualize variable importance
  output$fsplot1 <- renderPlotly({
    relImportance = relImportance()
    
    # Get variable importance from the model fit
    ImpDataRI <- as.data.frame(relImportance) %>% 
      rename(VImportance = "Overall") %>% 
      arrange(desc(VImportance))
    ImpDataRI$Var_Names <- row.names(ImpDataRI)
    
    # Plotting
    ggplot(ImpDataRI, aes(x=Var_Names, y=VImportance)) +
      geom_segment( aes(x=Var_Names, xend=Var_Names, y=0, yend=VImportance), color="skyblue") +
      geom_point(color="blue", alpha=0.6) +
      coord_flip() +
      xlab("Variable") +
      ylab("Variable Importance") +
      theme(
        legend.position="bottom",
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
  
  
  # Step 2b. Random Forest Method
  # Ref: https://hackernoon.com/random-forest-regression-in-r-code-and-interpretation
  
  # Print the dependent variable in the App
  # rffs_dependent_variable -> first column after ReadTime is always the dependent variable
  output$rffs_dependent_variable <- renderText({
    as.character(colnames(mini_selection_df())[1])
  })
  
  # Print the independent variable in the App
  # rffs_independent_variable -> the filtered dependent variables
  output$rffs_independent_variable <- renderText({
    as.character(paste(input$FS_explanatory_variable,collapse=", "))
  })
  
  # Print the selected number of trees grown in the App
  # rffs_ntree_text -> the selected number of trees grown
  output$rffs_ntree_text <- renderText({
    as.character(input$rffs_ntree)
  })
  
  # Print the selected number of predictors sampled for splitting at each node in the App
  # rffs_mtry_text -> the selected number of predictors sampled for splitting at each node
  output$rffs_mtry_text <- renderText({
    as.character(input$rffs_mtry)
  })
  
  # Fit the model
  rf_fit <- reactive({
    mini_selection_df = mini_selection_df()
    
    # Step 2b.1. Set Seed
    set.seed(8)
    
    # Step 2b.2. Fit the random forest model with mtry = 10 and ntree = 50
    rf_fit <- randomForest(as.formula(paste(colnames(mini_selection_df)[1]," ~ ", paste(input$FS_explanatory_variable,collapse="+"))), data=mini_selection_df, mtry=as.integer(input$rffs_mtry), ntree=as.integer(input$rffs_ntree),
                           keep.forest=FALSE, importance=TRUE)
    
    return(rf_fit)
  })
  
  # Step 2b.3. Print the fitted random forest model
  # rffs_model_output -> the output of the fitted random forest model
  output$rffs_model_output <- renderPrint({
    rf_fit()
  })
  
  # Step 2b.4. Print the variable importance
  # rffs_estimation <- the output of the estimated variable importance
  output$rffs_estimation <- renderPrint({
    rf_fit = rf_fit()
    
    importance(rf_fit)
  })

  # Step 2b.5. Visualize variable importance
  output$fsplot2 <- renderPlotly({
    rf_fit = rf_fit()
    
    # Get variable importance from the model fit
    ImpData <- as.data.frame(importance(rf_fit))
    ImpData$Var_Names <- row.names(ImpData)
    
    # Plotting
    ggplot(ImpData, aes(x=Var_Names, y=`%IncMSE`)) +
      geom_segment( aes(x=Var_Names, xend=Var_Names, y=0, yend=`%IncMSE`), color="skyblue") +
      geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
      coord_flip() +
      xlab("Variable") +
      theme(
        legend.position="bottom",
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
      ) +
      theme(panel.background = element_rect(fill = "linen")) +
      theme(plot.background = element_rect(fill = "linen"))
  })
  
  
  
  # Step 2c. MARS Method
  
  # Print the dependent variable in the App
  # mars_dependent_variable -> first column after ReadTime is always the dependent variable
  output$mars_dependent_variable <- renderText({
    as.character(colnames(selection_df)[1])
  })
  
  # Print the independent variable in the App
  # mars_independent_variable -> the filtered dependent variables
  output$mars_independent_variable <- renderText({
    as.character(paste(input$FS_explanatory_variable,collapse=", "))
  })
  
  # Fit the model
  marsModel <- reactive({
    # Step 2c.1. Set Seed
    set.seed(8)
    
    # Step 2c.2. Fitting the model
    marsModel <- earth(as.formula(paste(colnames(selection_df)[1]," ~ ", paste(input$FS_explanatory_variable,collapse="+"))), data=selection_df) # build model
    
    return(marsModel)
  })
  
  # Step 2c.3. Print the fitted MARS model
  # mars_model_output -> the output of the fitted MARS model
  output$mars_model_output <- renderPrint({
    marsModel()
  })
  
  # Step 2c.4. Estimate the variable importance
  ev <- reactive({
    marsModel = marsModel()
    
    ev <- evimp(marsModel) # estimate variable importance
    
    return(ev)
    
  })
  
  # Print the estimated variable importance by MARS method
  # mars_estimation <- the output of the estimated variable importance
  output$mars_estimation <- renderPrint({
    ev()
  })
  
  # Step 2c.5. Visualize variable importance
  output$fsplot3 <- renderPlot({
    ev = ev()
    
    plot(ev)
  })
  
  
  
  # Step 2d. Boruta Method
  
  # Print the dependent variable in the App
  # bfs_dependent_variable -> first column after ReadTime is always the dependent variable
  output$bfs_dependent_variable <- renderText({
    as.character(colnames(mini_selection_df())[1])
  })
  
  # Print the independent variable in the App
  # bfs_independent_variable -> the filtered dependent variables
  output$bfs_independent_variable <- renderText({
    as.character(paste(input$FS_explanatory_variable,collapse=", "))
  })
  
  # Print the selected number of trees grown in the App
  # bfs_ntree_text -> the selected number of trees grown
  output$bfs_ntree_text <- renderText({
    as.character(input$bfs_ntree)
  })
  
  # Print the selected number of predictors sampled for splitting at each node in the App
  # bfs_mtry_text -> the selected number of predictors sampled for splitting at each node
  output$bfs_mtry_text <- renderText({
    as.character(input$bfs_mtry)
  })
  
  # Fitting the model
  boruta_output <- reactive({
    mini_selection_df = mini_selection_df()
    
    # Step 2d.1. Set Seed
    set.seed(8)
    
    # Step 2d.2. Fitting the model and calculate the variable importance
    boruta_output <- Boruta(as.formula(paste(colnames(mini_selection_df)[1]," ~ ", paste(input$FS_explanatory_variable,collapse="+"))), data=mini_selection_df, doTrace=2, maxRuns=25)  # perform Boruta search
    
    return(boruta_output)
  })
  
  # Step 2d.3. Print the boruta_output
  # bfs_model_output -> the output of the fitted Boruta model
  output$bfs_model_output <- renderPrint({
    boruta_output()
  })
  
  # Step 2d.4. Print the confirmed variable(s) by Boruta model
  # bfs_confirmed -> the output of the confirmed variable(s) by Boruta model
  output$bfs_confirmed <- renderPrint({
    boruta_output = boruta_output()
    
    # Step 2d.4. Collect Confirmed variables (i.e., confirmed that it is important by Boruta Algorithm)
    boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed")])  # collect Confirmed variables
    
    # Print the confirmed variables
    return(boruta_signif)
  })
  
  # Step 2d.5. Print the estimations of variable importance
  # bfs_estimation -> the output of the estimations of variable importance
  output$bfs_estimation <- renderPrint({
    boruta_output = boruta_output()
    
    # Step 2d.5. Print the estimations of variable importance
    boruta_output[["ImpHistory"]]
  })
  
  # Step 2d.5. Plotting the variable importance
  output$fsplot4 <- renderPlot({
    boruta_output = boruta_output()
    
    # Step 2d.5. Plotting the variable importance
    plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance
  })
  
  # From Analysis Page to Global View Page
  observeEvent(input$feature_selection_to_gv, {
    updateTabItems(session, "tabs", selected = "dash2")
  })
  
  ################################ TREND #######################################
  
  # Step 1. Create trend_df -> the subset of pv dataset after filtered by date and variable
  trend_df <- reactive({
    newdata <- pv %>% 
      filter(between(PVdate, input$trend_dates[1], input$trend_dates[2])) %>% # Date
      dplyr::select(c(ReadTime, !!input$trend_observed_variables)) %>% # filter column
      na.omit() # remove NAs
    
    return(newdata)
    
  })
  
  # Step 2. Change the trend_df into time series (ts) object
  tseries_ts <- reactive({
    trend_df = trend_df()
    
    # Step 2a. Convert ReadTime into datetime data type by using Lubridate package 
    trenddatetime_df <- trend_df
    trenddatetime_df$ReadTime <- dmy_hm(trenddatetime_df$ReadTime)
    
    # Step 2b. Convert dataframe to time series
    tseries <- read.zoo(trenddatetime_df)
    
    # Step 2c. Convert to ts object
    tseries_ts <- ts(tseries, frequency = 96)
    
    return(tseries_ts)
    
  })
  
  # Step 3. Decompose the trend
  decomposed_tseries_ts <- reactive({
    tseries_ts = tseries_ts()
    
    decomposed_tseries_ts <- decompose(tseries_ts,type='multiplicative')
    
  })
  
  # Step 4. Plotting
  output$trendplot1 <- renderPlot({
    decomposed_tseries_ts = decomposed_tseries_ts()
    
    plot(decomposed_tseries_ts)
  })
  
  # From Analysis Page to Global View Page
  observeEvent(input$trend_to_gv, {
    updateTabItems(session, "tabs", selected = "dash2")
  })

  ################################ ABOUT #######################################
  
  observeEvent(input$link_to_home, {
    updateTabItems(session, "tabs", selected = "dash1")
  })
  
}

#-------------------------------------------------------------------------------

# Run the application #

#-------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
