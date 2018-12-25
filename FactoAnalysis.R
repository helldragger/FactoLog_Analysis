library("RSQLite")
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


#setup analysis functions
insertAnalysisData <- function(raw_ids, 
                               dates, 
                               game_names, 
                               forces, 
                               ig_dates,
                               ig_ticks,
                               data_type,
                               data_subtype,
                               data_names,
                               analysis_type,
                               analysis_subtype,
                               analysis_name,
                               analysis_values){
  n <- length(raw_ids)
  if(n > 0)
    {
    
    analysis_types <- rep.int(analysis_type, n)
    analysis_subtypes <- rep.int(analysis_subtype, n)
    analysis_names <- rep.int(analysis_name, n)
    data_types <- rep.int(data_type, n )
    data_subtypes <- rep.int(data_subtype, n )
    
    queries <- data.frame(raw_ids, dates, game_names, forces, ig_dates, ig_ticks, data_types, data_subtypes, data_names, analysis_types, analysis_subtypes, analysis_names, analysis_values)
    colnames(queries) <- NULL
    con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
    
    rs <- dbSendStatement(
      con, 
      "WITH new (raw_id, date, game_name, force, ig_date, ig_tick, data_type, data_subtype, data_name, analysis_type, analysis_subtype, analysis_name, value) 
      AS ( VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) )
      INSERT OR REPLACE INTO analyzed_data (analysis_id, raw_id, date, game_name, force, ig_date, ig_tick, data_type, data_subtype, data_name, analysis_type, analysis_subtype, analysis_name, value)
      SELECT old.analysis_id, old.raw_id, old.date, old.game_name, old.force, old.ig_date, old.ig_tick, old.data_type, old.data_subtype, old.data_name, old.analysis_type, old.analysis_subtype, old.analysis_name, new.value
      FROM analyzed_data AS new LEFT JOIN analyzed_data AS old ON  new.raw_id=old.raw_id AND new.analysis_type=old.analysis_type AND new.analysis_subtype=old.analysis_subtype AND new.analysis_name=old.analysis_name;")
    dbBind(rs, queries)
    dbClearResult(rs)
    dbDisconnect(con)
  }
}

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
  
  pb <- txtProgressBar(min=0, max=6*length(stat_tables$view_name)*length(forces$force), style=3)
  i <- 0
  for(view_name in stat_tables$view_name){
    con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
    all_table <- dbReadTable(con, view_name)
    dbDisconnect(con)
    all_table$input <- as.numeric(all_table$input)
    all_table$output <- as.numeric(all_table$output)
    all_table$stock <- as.numeric(all_table$stock)
    
    for(f in forces$force){
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

ticksChronogramPlot<- function(table, tablename, f){
  return(ggplot(table[with(table, order(-ig_tick)),]) + 
           geom_label(aes(x=ig_tick, y=0, label=value))  +
           geom_rug(aes(x=ig_tick, y=0))  +
           labs(x="TIME (ticks)", y=tablename, title=paste("TEAM", f, tablename,":", sep=" ")))
}

timestampChronogramPlot<- function(table, tablename, f){
  return(ggplot(table[with(table, order(-ig_tick)),]) + 
           geom_label(aes(x=ig_date, y=0, label=value))  +
           geom_rug(aes(x=ig_date, y=0))  +
           labs(x="DATE", y=tablename, title=paste("TEAM", f, tablename,":", sep=" ")))
}

generateEventChronograms <- function(window=-1){
  #GENERATING EVENTS CHRONOGRAMS
  
  print("COMPLETED RESEARCHES CHRONOGRAMS GENERATION")
  flush.console()
  
  tablename <- "completed_researches"
  
  con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
  table <- dbReadTable(con, tablename)
  dbDisconnect(con)
  
  
  table <- table[table$data_name == "name", ]
  
  
  pb <- txtProgressBar(min = 0, max= length(unique(table$force)), style=3)
  i <- 0
  if(window > 0){
    windowStr <- paste("OF_LAST_", as.character(window), "_TICKS", sep="")
    
    table <- table[order(table$ig_tick) & table$ig_tick >= max(table$ig_tick) - window,]
  }else{
    windowStr <- paste("OF_ALL_TIME", sep="")
    
    table <- table[order(table$ig_tick),]
  }
  
  
  for (f in unique(table$force)){
    n <- table[table$data_name == "name" & table$force == f,]

    plot <- ticksChronogramPlot(n, tablename, f)
    filename <- paste("EV_ECARTS_", f,"_",tablename,"_", windowStr,".png", sep="" )
    ggsave(plot, file=filename, width=16, height = 9, dpi=300)

    plot <- timestampChronogramPlot(n, tablename, f)
    filename <- paste("EV_CHRONO_", f,"_",tablename,"_", windowStr,".png", sep="" )
    ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    i<- i+1
    setTxtProgressBar(pb, i)
  }
  close(pb)

}
#GENERATING PENURY DATA (concerns solid+fluid data)

penuryIndicatorPlot<- function(table, title){
  return(ggplot(table[with(table, order(table$ig_tick, -xtfrm(table$estimated_time))),]) + 
           geom_line(aes(x=ig_tick, y=-1,group="ETBC Never",  linetype="temoin"))  + 
           geom_line(aes(x=ig_tick, y=216000,group="ETBC < 1h", linetype="temoin"))  + 
           geom_line(aes(x=ig_tick, y=108000,group="ETBC < 30m", linetype="temoin"))  + 
           geom_line(aes(x=ig_tick, y=18000,group="ETBC < 5m", linetype="temoin"))  + 
           geom_line(aes(x=ig_tick, y=3600,group="ETBC < 1m", linetype="temoin"))  +
           geom_line(aes(x=ig_tick, y=estimated_time, color=data_name, group=data_name, linetype="donnée"))  + 
           labs(x="TIME (ticks)", y="ESTIMATED TIME BEFORE CATASTROPHE (ticks)", title=title))
}

generatePenuryStats <- function(window=-1, filter=c()){
  print("PENURY TIME ESTIMATIONS GENERATION")
  flush.console()
  pb <- txtProgressBar(min=0, max=length(forces$force)*length(stat_tables$view_name), style=3)
  i <- 0
  for (f in forces$force) {
    for (view in stat_tables$view_name) {
      
      con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
      stock <- dbReadTable(con, view)
      dbDisconnect(con)
      
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
  return(relevant_prereq)
}



generateResearchTree <- function(objectives){
  
  print("RESEARCH OBJECTIVES ANALYSIS GENERATION")
  flush.console()
  pb <- txtProgressBar(min=0, max=length(forces$force), style=3)
  i<-0
  for(f in forces$force){
    con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
    prereq <- dbReadTable(con, "researches_prerequisites")
    all_researched <-dbReadTable(con, "completed_researches")
    dbDisconnect(con)
    
    colnames(prereq) <- c("raw_id", "r_name", "prereq_r_name")
    all_researched<-all_researched[all_researched$data_name == "name",]
    colnames(all_researched) <- c("raw_id", "force", "id_date", "ig_tick", "data_name", "r_name")
    
    relevant_prereq <- getDepTree(objectives, prereq)
    
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
    relevant_prereq <-results
    
    net <- graph_from_data_frame(d=results,  directed=T)

    researched <- c( "None", all_researched[all_researched$force == f,]$r_name)
    not_researchable <- relevant_prereq[!relevant_prereq$prereq_r_name %in% researched, ]$r_name
    not_researchable_yet <- relevant_prereq[relevant_prereq$prereq_r_name %in% researched & relevant_prereq$r_name %in% not_researchable, ]$r_name
    researchable <- relevant_prereq[! relevant_prereq$r_name %in% not_researchable,]$r_name
    
    
    if( length(unique(relevant_prereq$r_name)) < 50 ){
      c_unresearchable <- "gray75"
      c_researchable <- "darkred"
      c_researched <- "gray25"
      
      c_important <- "grey65"
      c_notimportant <- "grey75"
      
      c_researched_edges <- "orange"
      c_researchable_edges <- "green"
      c_not_researchable_yet_edges <- "red"
      c_other_edges <- "grey80"
      
      wd_researched_edges <- 2
      wd_other_edges <- 1
      
      sz_important <- 10
      sz_notimportant <- 5
      
      sh_important <- "circle"
      sh_notimportant <- "circle"
      
      c_sh_researched <- "gold"
      c_sh_researchable <- "orange"
      c_sh_important <- "grey35"
      c_sh_not_important <- "grey45"
      
      
      V(net)$label.color <- with(as_data_frame(net, what="vertices"), 
                            ifelse(name %in% researched, c_researched, 
                                ifelse(name %in% researchable,c_researchable, c_unresearchable)))
      V(net)[name=="None"]$label.color=c_researched
      
      V(net)$size <-ifelse(!V(net)$name %in% objectives, sz_notimportant, sz_important)
      
      V(net)$shape <-ifelse(!V(net)$name %in% objectives, sh_notimportant, sh_important)
      
      V(net)$color <- with(as_data_frame(net, what="vertices"),
                           ifelse( name %in% researched,
                                   c_sh_researched,
                                   ifelse( name %in% researchable, 
                                           c_sh_researchable,
                                           ifelse( name %in% objectives,
                                                    c_sh_important,
                                                    c_sh_not_important))))
      
    
      V(net)$frame.color <-ifelse(!V(net)$name %in% objectives, c_notimportant, c_important)
      
      ## coloring the graph outing paths of researched techs 
      c_edges <- rep(c_other_edges, ecount(net))
      wd_edges <- rep(wd_other_edges, ecount(net))
      
      for (r_inc.edge in incident_edges(net, V(net)[name %in% researched], mode="all")) {
        c_edges[r_inc.edge] <- c_researched_edges
        wd_edges[r_inc.edge] <- wd_researched_edges
      }
      for(r_inc.edge in incident_edges(net, V(net)[name %in% researchable & ! name %in% researched ], mode="out")){
        c_edges[r_inc.edge] <- c_researchable_edges
      }
      filename <- paste("RESEARCH_", f,"_GOALS_COMPLETION_GRAPH.svg", sep="" )
      
      
      svg(filename, width=16, height = 9)
      
      
      l <- layout_with_lgl(net)
      l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
      plot( net, 
           asp=1, 
           rescale=F, 
           edge.color=c_edges,
           label=results$r_name,
           edge.width=wd_edges,
           layout=l,
           vertex.label.cex=.7,
           vertex.label.font=2,
           edge.arrow.mode=0)
      legend(x=1.5,
             y=0,
             c("Researched",
               "Researchable",
               "Unresearchable and not important",
               "Unresearchable and important"),
             pch=21,
             col="#777777",
             pt.bg=c(c_sh_researched,
                     c_sh_researchable,
                     c_sh_not_important,
                     c_sh_important),
             pt.cex=2, 
             cex=.8,
             bty="n",
             ncol=1)
      
      dev.off()
    }
    #####Calcul du nombre de dépendance unique de chaque objectif et du nombre d'entre eux de recherche's
    start_point <- c()
    data_type <- c()
    data_value <- c()
    
    for(p in objectives){
      relevant_prereq <- unique(getDepTree(c(p), prereq))$r_name
      researched_tech <- relevant_prereq[relevant_prereq %in% researched]
      
      start_point <- c(start_point, p, p)
      data_type <- c(data_type, "researches_completed", "researches_needed")
      data_value <- c(data_value, length(researched_tech), length(relevant_prereq)-length(researched_tech))
    }
    tech_progress <- data.frame(start_point, data_type, data_value)
    colnames(tech_progress) <- c("objective", "data_type", "data_value")
    tech_progress <- tech_progress[order(tech_progress$data_type),]

    plot <- ggplot(melt(tech_progress), aes( x=objective, y=value, color=data_type, fill=data_type, group=objective))+
      geom_bar(position="fill", stat="identity")+
      labs(x="RESEARCH OBJECTIVE", y="COMPLETION PERCENTAGE", title=paste("TEAM", f, "RESEARCH GOALS:" ))
    filename <- paste("RESEARCH_", f,"_GOALS_COMPLETION_PERC.png", sep="" )
    ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    
    plot <- ggplot(melt(tech_progress), aes( x=objective, y=value, color=data_type, fill=data_type, group=objective))+
      geom_bar(position="stack", stat="identity")+
      labs(x="RESEARCH OBJECTIVE", y="COMPLETION BARS", title=paste("TEAM", f, "RESEARCH GOALS:" ))
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
working_dir <- "D:\\FactoLogAnalysis"


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

##### DB LOADING

con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))

dbListTables(con)
stat_tables <- dbReadTable(con, "stat_views")
raw_data <- dbReadTable(con, "raw_data")
forces <- dbReadTable(con, "forces")
completed_researches <- dbReadTable(con, "completed_researches")
researches_prerequisites <- dbReadTable(con, "researches_prerequisites")
unlocked_all <- dbReadTable(con, "unlocked_all")
solid <- dbReadTable(con, "BUILD")
analyzed_data <- dbReadTable(con, "analyzed_data")
dbDisconnect(con)


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
