# server.R for lineups shiny app
library(shiny)
library(tidyverse)
library(RSiena)
library(geomnet)
# senate siena data
load('dat/senateSienaNoHRC.rda')
# all data for the "data plot"
alldat <- read_csv("dat/alldatasims.csv")
# get model means that we start with
load("dat/allModelMeans.RDS")
# siena models to simulate from 
SenBasic <- getEffects(senateSiena)
Senjtt_p <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Senjtt_s <- includeEffects(SenBasic, "jumpXTransTrip", include = TRUE, type = "eval", interaction1 = "sex", character = TRUE)
Sensame_p <- includeEffects(SenBasic, "sameX", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Senstt_p <- includeEffects(SenBasic, "sameXTransTrip", include = TRUE, type = "eval", interaction1 = "party", character = TRUE)
Senstt_b <- includeEffects(SenBasic, "simXTransTrip", include = TRUE, type = "eval", interaction1 = "bills", character = TRUE)
allStructs <- tibble(model = c("basic", "jttp", "jtts", "samep", "simttb", "samettp"),
       effStruct = list(SenBasic, Senjtt_p, Senjtt_s, Sensame_p, Senstt_b, Senstt_p))

# simulation function
saom_simulate2 <- function(dat, struct, parms, N) {
  require(RSiena)
  struct$initialValue[struct$include] <- parms
  myalgorithm <- sienaAlgorithmCreate(projname = Sys.time(), 
                                      useStdInits = FALSE, cond = TRUE, nsub = 0, simOnly = TRUE, 
                                      n3 = N)
  getsims <- siena07(myalgorithm, data = dat, returnDeps = TRUE, 
                     effects = struct, batch = TRUE, verbose = FALSE, silent = TRUE)
  return(getsims$sims)
}
# turn simulation lists into dfs 
sims_to_df <- function(sims) {
  N <- length(sims)
  waves <- length(sims[[1]][[1]][[1]])
  simsdf <- NULL
  counter <- 1
  for (i in 1:N) {
    for (j in 1:waves) {
      dat <- as.data.frame(sims[[i]][[1]][[1]][[j]])
      names(dat) <- c("from", "to", "dep.var.id")
      ids <- unique(c(unique(dat$from), unique(dat$to)))
      nodes <- data.frame(id = ids)
      dat2 <- merge(dat, nodes, by.x = "from", by.y = "id", 
                    all = T)
      dat2$wave = j
      dat2$sim = i
      simsdf[[counter]] <- dat2
      counter <- counter + 1
    }
  }
  mydf <- plyr::rbind.fill(simsdf)
  return(mydf)
}

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # model <- reactive({ input$model })
  # basicParm <- reactive({ input$basicParm })
  # mult <- reactive({ input$mult })
  # M <- reactive({ input$M })
  # seed <- reactive({ input$seed })
  
  dat_for_lu <- reactive({
    # get a null data set
    nulldat <- filter(alldat, model == input$model)
    set.seed(input$seed)
    whichdat <- sample(max(nulldat$sim), 1)
    nulldat <- filter(nulldat, sim == whichdat, wave == input$wave) %>% mutate(sim = 1001)
    
    # make new parameter vectors
    if (input$model == "basic"){
      if(input$basicParm == "both"){
        newparms <- c(1,1,1,input$mult, input$mult)
      } else if(input$basicParm == "density"){
        newparms <- c(1,1,1,input$mult, 1)
      } else if(input$basicParm == "reciprocity"){
        newparms <- c(1,1,1,1,input$mult)
      } else{
        newparms <- c(1,1,1,1,1)
      }
        
    } else {
      newparms <- c(1,1,1,1,1,input$mult)
    }
    
    #simulate alternate data
    sienaSim <- saom_simulate2(dat = senateSiena,
                               struct = allStructs[which(allStructs$model == input$model),][[2]][[1]],
                               parms = modelMeanEsts[which(modelMeanEsts$model == input$model),][[2]][[1]]*newparms,
                               N = input$M - 1)
    altdat <- sims_to_df(sienaSim)
    altdat <- altdat %>% filter(wave == input$wave)
    altdat$model <- ifelse(input$model == "basic", 
                           paste0(input$basicParm,input$mult),
                           paste0(input$model, "x", input$mult))
    # combine data 
    dat_for_lu <- bind_rows(altdat, nulldat) %>%  
      mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) 
    dat_for_lu$ord <- rep(sample(input$M), as.numeric(table(dat_for_lu$sim)))
    dat_for_lu
  })
  # # subset the data to one plot
  #   Nulldat <- reactive({
  #     nulldat <- filter(alldat, model == model)
  #     set.seed(seed)
  #     whichdat <- sample(max(nulldat$sim), 1)
  #     nulldat <- filter(nulldat, sim == whichdat) %>% mutate(sim = 1001)
  #     nulldat
  #   })
    
  # get the parameters to simulate from
  # if (model == "basic"){
  #   if(basicParm == "both"){
  #     newparms <- reactive({c(1,1,1,mult, mult)})
  #   } else if(basicParm() == "density"){
  #     newparms <- reactive({c(1,1,1,mult, 1)})
  #   } else{
  #     newparms <- reactive({c(1,1,1,1,mult)})
  #   }
  # } else {
  #   newparms <- reactive({c(1,1,1,1,1,mult)})
  # }

  # get alternative dat
  #dat_for_lu <- reactive({
  #  nulldat <- Nulldat()
  #  parms <- newparms()
    # sienaSim <- saom_simulate2(dat = senateSiena,
    #                struct = allStructs[which(allStructs$model == model),][[2]][[1]],
    #                          parms = modelMeanEsts[which(modelMeanEsts$model == model),][[2]][[1]]*parms,
    #                          N = M-1)
    # altdat <- sims_to_df(sienaSim)
    # altdat <- altdat %>% filter(wave == input$wave)
    # altdat$model <- ifelse(mod == "basic", 
    #                        paste0(basicParm(),mult()),
    #                        paste0(model(), "x", mult()))
    # # combine data 
    # dat_for_lu <- bind_rows(altdat, nulldat) %>%  
    #   mutate(from = paste0("V", from), to = ifelse(is.na(to), to, paste0("V", to))) 
    # dat_for_lu$ord <- rep(sample(M()), as.numeric(table(dat_for_lu$sim)))
    # dat_for_lu
    #})
  
  # need outputs for plot, data, data pot
  
  output$lineup <- renderPlot({
    dat <- dat_for_lu()
    ggplot(data = dat) + 
      geom_net(aes(from_id = from, to_id = to), 
               arrow = arrow(type = 'open', length = unit(2, "points") ), 
               linewidth = .25, singletons = T, fiteach = T, directed = T, 
               color = 'black', arrowgap = .015, arrowsize = .3, size = 1) + 
      theme_net() + 
      theme(panel.background = element_rect(color = 'black')) +
      facet_wrap(~ord)
  })
  output$dataPlot <- renderText({
    dat <- dat_for_lu()
    Mm <- max(dat$ord)
    expr = which(table(dat$sim, dat$ord)[Mm,] != 0)
  })
  output$lineupData <- renderDataTable({
    dat <- dat_for_lu()
    dat
    })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$model, "x", input$mult,"sd",input$seed, ".csv", sep = "")
    },
    content = function(file) {
      dat <- dat_for_lu()
      write.csv(dat, file, row.names = FALSE)
    }
  )

}