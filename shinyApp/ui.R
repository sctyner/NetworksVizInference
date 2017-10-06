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
      
      # Input: pick a model
      selectInput(inputId = "model",
                  label = "Select a model:",
                  choices = c("basic", "jttp", "jtts", "samep", "samettp", "simttb"),
                  selected = "basic"),
      # Input: pick a wave 
      numericInput(inputId = "wave",
                   label = "Pick a wave:",
                   value = 1, min = 1, max = 3),
      # Input: pick density, reciprocity, or both when basic is selected above 
      selectInput(inputId = "basicParm", 
                  label = "Select a parameter (density, reciprocity, or both)\n
                  for basic model multiplier. Select none for all other models:",
                  choices = c("density", "reciprocity", "both", "none"), 
                  selected = "none"),
      # Input: pick a lineup size
      numericInput(inputId = "M",
                   label = "Choose size of lineup:",
                   value = 6, min = 3, max = 20),
      # Input: multiplier of effect size
      numericInput(inputId = "mult", 
                   label = "Choose effect multiplier:", 
                   value = 5, min = -10, max = 10),
      # Input: set a seed
      numericInput(inputId = "seed",
                   label = "Set a random seed:", 
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