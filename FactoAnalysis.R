library("plotly")
library("SciViews")
library("stats")
library("glogis")
library("ggplot2")
library("GGally")
library("igraph") 
library("network") 
library("sna")
library("visNetwork")
library("threejs")
library("networkD3")
library("ndtv")
library("reshape2")

script.dir <- dirname(sys.frame(1)$ofile)

setwd(script.dir)
if(!exists("insertAnalysisData", mode="function")) source("db.R")
if(!exists("researchObjectiveGraph", mode="function")) source("formatter.R")


getValues <- function (table, data_type){
  return(table[table$type==data_type,]$value)
}

getLastValue <- function (table, data_type){
  return(getValues[order(-table$ig_tick), 1, ]$value)
}

getFirstValue <- function (table, data_type){
  return(getValues[order(table$ig_tick), 1, ]$value)
}

getTypeFilteredData <- function (table, filter_table){
  return(table[table$data_name %in% filter_table, ])
}

getTendanceTable <- function (t){
  tendTable <- t[order(t$ig_tick), ]
  tendTable$value <- diff(c(0, 0, tendTable$value), differences = 2)
  return(tendTable)
}

generateProdStats <- function(filter=c(), window=-1, extension=".png"){
  
  print("PRODUCTION STATISTICS GENERATION")
  flush.console()
  
  pb <- txtProgressBar(min=0, max=6*length(getStatsViewNames())*length(getAllForces()), style=3)
  i <- 0
  for(view_name in getStatsViewNames()){
    all_table <- getView(view_name)
    
    all_table$input <- as.numeric(all_table$input)
    all_table$output <- as.numeric(all_table$output)
    all_table$stock <- as.numeric(all_table$stock)
    
    for(f in getAllForces()){
      table <- all_table[all_table$force==f,]
      if(window > 0){
        windowStr <- paste("OF_LAST_", as.character(timespan), "_TICKS", sep="")
        table <- table[order(table$ig_tick) & table$ig_tick >= max(table$ig_tick) - timespan,]
      }else{
        windowStr <- paste("OF_ALL_TIME", sep="")
        table <- table[order(table$ig_tick),]
      }
      filterStr <- "FULL"
      if(length(filter)!= 0){
        filterStr <- "FILTERED"
        table<- getTypeFilteredData(table, filter)
      }
      # INPUT PLOT GENERATION
      view_data <- "input"
      title<-paste("TEAM", f, view_data, view_name, filterStr, "DATA :",sep=" ")
      
      i<-i+1
      setTxtProgressBar(pb, i)
      
      
      plot <- ggplot(table) +
                   geom_line(aes(ig_tick, input, color=data_name))+
                   labs(x="TIME (ticks)", y="UNIT", title=title)
      filename <-paste(f,view_data,view_name,filterStr,windowStr, sep="_" )
      ggsave(plot, file=paste(filename,extension,sep=""), width=16, height = 9, dpi=300)
      
      # OUTPUT PLOT GENERATION
      view_data <- "output"
      title<-paste("TEAM", f, view_data, view_name, filterStr, "DATA :",sep=" ")
      
      i<-i+1
      setTxtProgressBar(pb, i)
      
      plot <- ggplot(table) +
                   geom_line(aes(ig_tick, output, color=data_name))+
                   labs(x="TIME (ticks)", y="UNIT", title=title)
      filename <-paste(f,view_data,view_name,filterStr,windowStr, sep="_" )
      ggsave(plot, file=paste(filename,extension,sep=""), width=16, height = 9, dpi=300)
      # STOCK PLOT GENERATION
      view_data <- "stock"
      title<-paste("TEAM", f, view_data, view_name, filterStr, "DATA :",sep=" ")
      
      i<-i+1
      setTxtProgressBar(pb, i)
      
      plot <- ggplot(table) +
                   geom_line(aes(ig_tick, stock, color=data_name))+
                   labs(x="TIME (ticks)", y="UNIT", title=title)
      filename <-paste(f,view_data,view_name,filterStr,windowStr, sep="_" )
      ggsave(plot, file=paste(filename,extension,sep=""), width=16, height = 9, dpi=300)
      
      ### TENDANCE GENERATION
      tendTable <- table
      
      
      # INPUT TENDANCE PLOT GENERATION
      
      tendTable$value <- table$input
      tendTable <- getTendanceTable(tendTable)
      table$input <- tendTable$value
      
      
      view_data <- "input"
      title<-paste("TEAM", f, view_data, view_name, filterStr, "TENDANCE DATA :",sep=" ")
      
      i<-i+1
      setTxtProgressBar(pb, i)
      
      plot <- ggplot(table) +
        geom_line(aes(ig_tick, input, color=data_name))+
        labs(x="TIME (ticks)", y="UNIT", title=title)
      filename <-paste(f,view_data,view_name,filterStr,windowStr,"TENDANCE", sep="_" )
      ggsave(plot, file=paste(filename,extension,sep=""), width=16, height = 9, dpi=300)
      
      insertAnalysisData(table$input_id,
                         table$date,
                         table$game_name,
                         table$force,
                         table$ig_date,
                         table$ig_tick,
                         view_name,
                         view_data,
                         table$data_name,
                         "INTERPOLATION",
                         "TENDANCE",
                         paste(view_name,view_data,sep="_"),
                         table$input)
      
      
      
      # OUTPUT TENDANCE PLOT GENERATION
      
      tendTable$value <- table$output
      tendTable <- getTendanceTable(tendTable)
      table$output <- tendTable$value
      
      
      view_data <- "output"
      title<-paste("TEAM", f, view_data, view_name, filterStr, "TENDANCE DATA :",sep=" ")
      
      i<-i+1
      setTxtProgressBar(pb, i)
      
      plot <- ggplot(table) +
        geom_line(aes(ig_tick, output, color=data_name))+
        labs(x="TIME (ticks)", y="UNIT", title=title)
      filename <-paste(f,view_data,view_name,filterStr,windowStr,"TENDANCE", sep="_" )
      ggsave(plot, file=paste(filename,extension,sep=""), width=16, height = 9, dpi=300)
      insertAnalysisData(table$output_id,
                         table$date,
                         table$game_name,
                         table$force,
                         table$ig_date,
                         table$ig_tick,
                         view_name,
                         view_data,
                         table$data_name,
                         "INTERPOLATION",
                         "TENDANCE",
                         paste(view_name,view_data,sep="_"),
                         table$output) 
      
      # STOCK TENDANCE PLOT GENERATION
      
      tendTable$value <- table$stock
      tendTable <- getTendanceTable(tendTable)
      table$stock <- tendTable$value
      
      view_data <- "stock"
      title<-paste("TEAM", f, view_data, view_name, filterStr, "TENDANCE DATA :",sep=" ")
      
      i<-i+1
      setTxtProgressBar(pb, i)
      
      plot <- ggplot(table) +
        geom_line(aes(ig_tick, stock, color=data_name))+
        labs(x="TIME (ticks)", y="UNIT", title=title)
      filename <-paste(f,view_data,view_name,filterStr,windowStr,"TENDANCE", sep="_" )
      ggsave(plot, file=paste(filename,extension,sep=""), width=16, height = 9, dpi=300)
      insertAnalysisData(table$input_id,
                         table$date,
                         table$game_name,
                         table$force,
                         table$ig_date,
                         table$ig_tick,
                         view_name,
                         view_data,
                         table$data_name,
                         "INTERPOLATION",
                         "TENDANCE",
                         paste(view_name,view_data,sep="_"),
                         table$stock)
    }
  }
  close(pb)
}

generateEventChronograms <- function(window=-1){
  #GENERATING EVENTS CHRONOGRAMS
  
  print("COMPLETED RESEARCHES CHRONOGRAMS GENERATION")
  flush.console()
  
  tablename <- "completed_researches"
  table <- getCompletedResearches()

  
  table <- table[table$data_name == "name", ]
  
  
  pb <- txtProgressBar(min = 0, max= length(getAllForces()), style=3)
  i <- 0
  if(window > 0){
    windowStr <- paste("OF_LAST_", as.character(window), "_TICKS", sep="")
    
    table <- table[order(table$ig_tick) & table$ig_tick >= max(table$ig_tick) - window,]
  }else{
    windowStr <- paste("OF_ALL_TIME", sep="")
    
    table <- table[order(table$ig_tick),]
  }
  
  
  for (f in getAllForces()){
    n <- table[table$data_name == "name" & table$force == f,]
    if(nrow(n) > 0){ 

      plot <- ticksChronogramPlot(n, tablename, f)
      filename <- paste("EV_ECARTS_", f,"_",tablename,"_", windowStr,".png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
  
      plot <- timestampChronogramPlot(n, tablename, f)
      filename <- paste("EV_CHRONO_", f,"_",tablename,"_", windowStr,".png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
      
    }
    i<- i+1
    setTxtProgressBar(pb, i)
  }
  close(pb)

}
#GENERATING PENURY DATA (concerns solid+fluid data)


generatePenuryStats <- function(window=-1, filter=c()){
  print("PENURY TIME ESTIMATIONS GENERATION")
  flush.console()
  pb <- txtProgressBar(min=0, max=length(getAllForces())*length(getStatsViewNames()), style=3)
  i <- 0
  for (f in getAllForces()) {
    for (view in getStatsViewNames()) {
      stock <- getView(view)

      stock$input <- as.numeric(stock$input)
      stock$output <- as.numeric(stock$output)
      stock$stock <- as.numeric(stock$stock)
      
      if(window > 0){
        windowStr <- paste("OF_LAST_", as.character(window), "_TICKS", sep="")
        stock <- stock[stock$force==f & stock$ig_tick >= max(stock$ig_tick) - timespan,]
        
      }else{
        
        windowStr <- paste("OF_ALL_TIME", sep="")
        stock <- stock[stock$force==f,]
      }
      filterStr <- "FULL"
      if(length(filter)!= 0){
        filterStr <- "FILTERED"
        stock<- getTypeFilteredData(stock, filter)
      }
      
      stock$value <- stock$stock
      stock_t <- getTendanceTable(stock)
      stock$estimated_time <- rep.int(-1,nrow(stock))
      
      
      for (ti in unique(stock$ig_tick)) {
        for (ty in unique(stock[stock$ig_tick==ti,]$data_name)) {
           
          #big penury =not enough stock+input for 1 minute (60**3 ticks)
          #fatal penury= not enough stock+input for 1 output cycle (1  tick)
          #time in ticks before zero = -current stock /(current production - max consumption recorded)
          curr_st <- stock[stock$ig_tick==ti & stock$data_name == ty,]$stock
          curr_st_t <- stock_t[stock_t$ig_tick==ti & stock_t$data_name == ty,]$stock
          t <- -10000
          #we avoid any division par zero et on ne s occupe que du t > 0, qui veut dire que les stocks diminuent
          if (curr_st_t != 0){
            t <- -curr_st/curr_st_t
          }
          stock[stock$ig_tick == ti & stock$data_name ==ty, ]$estimated_time<-t
          
        }
      }
      
      if (nrow(stock) > 0){
        
        insertAnalysisData(stock$input_id,
                           stock$date,
                           stock$game_name,
                           stock$force,
                           stock$ig_date,
                           stock$ig_tick,
                           view,
                           "stock",
                           stock$data_name,
                           "INTERPOLATION",
                           "PENURY",
                           "TIME_ESTIMATION",
                           stock$estimated_time)

        plot <- penuryIndicatorPlot(stock, paste("TEAM", f," FILTERED PENURY LEVELS ",windowStr," :", sep=" "))
        filename <- paste("FILTERED_ST_DATA_", f,"_PENURY_LEVELS_",windowStr, ".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
      }
      i <- i + 1
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
}


##### RESEARCH ANALYSIS

getDepTree <- function(start, prereq_table){
  relevant_prereq <- unique(prereq_table[prereq_table$r_name %in% start & order(prereq_table$r_name), ])
  #time to reconstruct the whole tree
  next_prereq <- unique(prereq_table[(prereq_table$r_name %in% relevant_prereq$prereq_r_name | prereq_table$r_name %in% relevant_prereq$r_name) & order(prereq_table$r_name), ])
  while(nrow(relevant_prereq) != nrow(next_prereq)){
    relevant_prereq <- next_prereq
    
    next_prereq <- unique(prereq_table[(prereq_table$r_name %in% relevant_prereq$prereq_r_name | prereq_table$r_name %in% relevant_prereq$r_name) & order(prereq_table$r_name), ])
    
  }
  end_researches <- unique(relevant_prereq[ ! relevant_prereq$prereq_r_name %in% relevant_prereq$r_name,]$prereq_r_name)
  
  #adding the end researches to the relevant dataframe by reconstruction of the data frame:
  size_dt <- nrow(relevant_prereq)
  size_er <- length(end_researches)
  r_name <- c()
  pre_r <- c()
  for (i in 1:size_dt) {
    r_name <- c(r_name, relevant_prereq[i,]$r_name)
    pre_r <- c(pre_r, relevant_prereq[i,]$prereq_r_name)
  }
  for (i in 1:size_er){
    r_name <- c(r_name, end_researches[i])
    pre_r <- c(pre_r, "None")
  }
  r_name <- c(r_name, "None")
  pre_r <- c(pre_r, "None")
  results <- data.frame(r_name, pre_r)
  colnames(results) <- c("r_names", "prereq_r_names")  
  
  return(results)
}



generateResearchTree <- function(objectives){
  
  print("RESEARCH OBJECTIVES ANALYSIS GENERATION")
  flush.console()
  pb <- txtProgressBar(min=0, max=length(getAllForces()), style=3)
  i<-0
  for(f in getAllForces()){
    prereq <- getResearchesPrerequisites()
    all_researched <- getCompletedResearches()

    colnames(prereq) <- c("raw_id", "r_name", "prereq_r_name")
    all_researched<-all_researched[all_researched$data_name == "name",]
    colnames(all_researched) <- c("raw_id", "force", "id_date", "ig_tick", "data_name", "r_name")
    
    relevant_prereq <- getDepTree(objectives, prereq)
    researched <- c( "None", completed_researches[completed_researches$force == f,]$r_name)
    not_researchable <- relevant_prereq[!relevant_prereq$prereq_r_name %in% researched, ]$r_name
    not_researchable_yet <- relevant_prereq[relevant_prereq$prereq_r_name %in% researched & relevant_prereq$r_name %in% not_researchable, ]$r_name
    researchable <- relevant_prereq[! relevant_prereq$r_name %in% not_researchable,]$r_name
    
    
    
    researchObjectiveGraph(relevant_prereq, objectives, researched, not_researchable, not_researchable_yet, researchable, f)
    
    #####Calcul du nombre de dépendance unique de chaque objectif et du nombre d'entre eux de recherche's
    start_point <- c()
    data_type <- c()
    data_value <- c()
    
    for(p in objectives){
      relevant_prereq <- unique(getDepTree(c(p), prereq))$r_name
      researched_tech <- relevant_prereq[relevant_prereq %in% researched]
      
      start_point <- c(start_point, p, p)
      data_type <- c(data_type, "researches_completed", "researches_needed")
      data_value <- c(data_value, length(researched_tech)-1, length(relevant_prereq)-1-length(researched_tech))
    }
    
    tech_progress <- data.frame(start_point, data_type, data_value)
    colnames(tech_progress) <- c("objective", "data_type", "data_value")
    tech_progress <- tech_progress[order(tech_progress$data_type),]
  
    plot <- researchObjectiveCompletionPercentageBars(tech_progress, f)
    filename <- paste("RESEARCH_", f,"_GOALS_COMPLETION_PERC.png", sep="" )
    ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    
    plot <- researchObjectiveCompletionBars(tech_progress, f)
    filename <- paste("RESEARCH_", f,"_GOALS_COMPLETION_BARS.png", sep="" )
    ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    
    i<-i+1
    setTxtProgressBar(pb, i)
  }
  close(pb)
}

##### analysis functions


slideFunct <- function(data, window, step){
  total <- length(data)
  spots <- seq(from=1, to=(total-window), by=step)
  result <- vector(length = length(spots))
  for(i in 1:length(spots)){
    result[i] <- mean(data[spots[i]:(spots[i]+window)])
  }
  return(result)
}

##### SCRIPT CONFIGURATION

gamename <- "default"
working_dir <- paste(script.dir, "out", sep="\\")


##### DB CONFIGURATION

dbpath <- "C:/Users/Chris/PycharmProjects/FactoLog Data Miner/resources/"
dbfilename <- "testmap.db"

##### PLOTLY CONFIGURATION

Sys.setenv("plotly_username"="helldragger")
Sys.setenv("plotly_api_key"="5VZ8a6OdUNPjLflaHHHP")
plotly_output <- F


##### ANALYSIS CONFIGURATION

# Nb of ticks analyzed to figure out maximums and minimums, etc. -1 = all
analysis_relevant_timespan <- -1

# Nb of ticks represented on outputted graphs, three scopes for three kind of overview graphs
## close means the smallest ticks window: default = latest 10 mins (36 000 ticks)
analysis_output_close_timespan <- 36000
## medium means the intermediate window : default = latest 30 mins (108 000 ticks)
analysis_output_medium_timespan <- 108000
## large means the largest window : default = latest 1 h (216 000 ticks)
analysis_output_large_timespan <- 216000

##### SCRIPT INIT

dir.create(file.path(working_dir, gamename))
setwd(file.path(working_dir, gamename))

##### FILTERS

penuryFilter <- c("iron-ore","iron-plate","copper-plate","steel-plate","copper-ore","coal","stone", "crude-oil", "light-oil", "heavy-oil", "petroleum-gas", "lubricant", "water", "steam")
killsFilter <- c("player", "behemoth-spitter", "behemoth-biter", "big-spitter", "big-biter", "big-worm-biter", "medium-spitter", "medium-biter", "medium-worm-biter", "small-spitter", "small-biter", "small-worm-biter", "biter-spawner")
researchObjectives <- c("construction-robotics", "logistic-system", "automated-rail-transportation", "oil-processing", "atomic-bomb", "artillery", "tanks", "nuclear-power", "modules", "rocket-silo")

researchObjectives2 <- c("automated-rail-transportation", "oil-processing", "modules", "rocket-silo")

researchObjectives3 <- c("modules")

##### 

generateProdStats(filter=penuryFilter)
generateEventChronograms()
generatePenuryStats(filter=penuryFilter)

generateResearchTree(researchObjectives2)
