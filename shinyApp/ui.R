# ui.R for lineups shiny app
library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Picking Lineups"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: which model are you designating? The null (M-1 plots) or the alternate (1 plot)
      radioButtons(inputId = "whichModel", 
                    label = 'Which model are you designating?', 
                    choices = list("Null (simulate M-1 new plots)" = "null",
                                   "Alternative (simulate 1 new plot)" = "alt"),
                    selected = "alt"),
      # Input: pick a model
      selectInput(inputId = "model",
                  label = "Select an effect to test:",
                  choices = list("basic" = "basic", "jttp (-3.45)" = "jttp", "jtts (3.34)"= "jtts", 
                              "samep (.20)" = "samep", "samettp (1.33)" = "samettp", "simttb (10.09)" = "simttb"),
                  selected = "basic"),
      # Input: pick a wave 
      numericInput(inputId = "wave",
                   label = "Pick a wave:",
                   value = 1, min = 1, max = 3),
      # Input: pick density, reciprocity, or both when basic is selected above 
      selectInput(inputId = "basicParm", 
                  label = "Select a parameter (density, reciprocity, or both)\n
                  for basic model to test. Select none for all other models:",
                  choices = list("density (-4.90)"= "density", "reciprocity (4.89)" = "reciprocity", 
                                 "both", "none"), 
                  selected = "none"),
      # Input: pick a lineup size
      numericInput(inputId = "M",
                   label = "Choose size of lineup:",
                   value = 6, min = 3, max = 20),
      # Input: multiplier of effect size
      numericInput(inputId = "mult", 
                   label = "Choose effect multiplier:", 
                   value = 5, min = -1000, max = 1000),
      # Input: set a seed
      numericInput(inputId = "seed",
                   label = "Set a random seed (10,000-999,999):", 
                   value = 123456, min = 10000, max = 999999, step = 1),
      # Submit button 
      submitButton(text = "Generate lineup!", icon = icon("connectdevelop"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      navbarPage(title = "",
                 # Output: Lineup
                 tabPanel("Lineup", plotOutput(outputId = "lineup", height = 600)), 
                 # Output: which is the data plot
                 tabPanel("Data plot", textOutput(outputId = "dataPlot")),
                 # Output: data from the lineup
                 tabPanel("Lineup data", dataTableOutput(outputId = "lineupData"),
                          downloadButton("downloadData", "Download")
                 )
      )
    )
  )
  )