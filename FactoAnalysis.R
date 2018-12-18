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
getValues <- function (table, data_type){
  return(table[table$type==data_type,]$value)
}

getLastValue <- function (table, data_type){
  return(getValues[order(-table$ticks), 1, ]$value)
}

getFirstValue <- function (table, data_type){
  return(getValues[order(table$ticks), 1, ]$value)
}

getTypeFilteredData <- function (table, filter_table){
  return(table[table$type %in% filter_table, ])
}

getTendanceTable <- function (t){
  tendTable <- t
  tendTable <- tendTable[order(tendTable$ticks), ]
  tendTable$value <- diff(c(0, tendTable$value))
  return(tendTable)
}

dataPerTickPlot <- function(table, title){
  return(ggplot(table) +
           geom_line(aes(ticks, value, color=type, group=type))+
           labs(x="TIME (ticks)", title=title))
}


generateProdStats <- function(timespan=-1, plotly_output=FALSE){
  #Generating separate INPUT, OUTPUT, STOCK AND SUBSEQUENT TENDANCIES GRAPHS
  for (tablename in stat_tables$tablename) {
  
    table <-dbReadTable(con, tablename)
    if(timespan > 0){
      windowStr <- paste("OF_LAST_", as.character(timespan), "_TICKS", sep="")
      
      table <- table[order(table$ticks) & table$ticks >= max(table$ticks) - timespan,]
    }else{
      windowStr <- paste("OF_ALL_TIME", sep="")
      
      table <- table[order(table$ticks),]
    }
    for(f in forces$force){
      
      print(paste(f, tablename, "ANALYSIS, TIMESPAN :", windowStr))
      flush.console() 
      n <- table[table$force == f,]
      if( nrow(n) > 0){
        plot <- dataPerTickPlot(n, paste("TEAM", f, tablename," DATA:", sep=" "))
        filename <-paste("ST_DATA_", f,"_",tablename,"_", windowStr,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
        if(plotly_output){
          api_create(plot, filename = filename)
        }        
        plot <- dataPerTickPlot(getTypeFilteredData(n, penuryFilter), paste("TEAM", f, tablename," FILTERED DATA:", sep=" "))
        filename <-paste("FILTERED_ST_DATA_", f,"_",tablename,"_", windowStr,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
        
        plot <- dataPerTickPlot(getTendanceTable(n), paste("TEAM", f, tablename," TENDANCES:", sep=" "))
        filename <-paste("ST_TENDANCES_", f,"_",tablename,"_", windowStr,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
        
        plot <- dataPerTickPlot(getTypeFilteredData(getTendanceTable(n), penuryFilter), paste("TEAM", f, tablename," FILTERED TENDANCES:", sep=" "))
        filename <-paste("FILTERED_ST_TENDANCES_", f,"_",tablename,"_", windowStr,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
      }
    }
  }
}

ticksChronogramPlot<- function(table, tablename, f){
  return(ggplot(table[with(table, order(-xtfrm(insertionDate))),]) + 
           geom_label(aes(x=ticks, y=0, label=value))  +
           geom_rug(aes(x=ticks, y=0))  +
           labs(x="TIME (ticks)", y=tablename, title=paste("TEAM", f, tablename,":", sep=" ")))
}

timestampChronogramPlot<- function(table, tablename, f){
  return(ggplot(table[with(table, order(-xtfrm(insertionDate))),]) + 
           geom_label(aes(x=timestamp, y=0, label=value))  +
           geom_rug(aes(x=timestamp, y=0))  +
           labs(x="DATE", y=tablename, title=paste("TEAM", f, tablename,":", sep=" ")))
}

generateEventChronograms <- function(timespan=-1){
  #GENERATING EVENTS CHRONOGRAMS
  for (tablename in event_tables$tablename) {
    table <-dbReadTable(con, tablename)
    
    if(timespan > 0){
      windowStr <- paste("OF_LAST_", as.character(timespan), "_TICKS", sep="")
      
      table <- table[order(table$ticks) & table$ticks >= max(table$ticks) - timespan,]
    }else{
      windowStr <- paste("OF_ALL_TIME", sep="")
      
      table <- table[order(table$ticks),]
    }
    
    
    for (f in unique(table$force)){
      n <- table[table$type == "name" & table$force == f,]
      #name dependent values
      
      print(paste(f, tablename, "ANALYSIS, TIMESPAN :", windowStr))
      flush.console()             
      
      plot <- ticksChronogramPlot(n, tablename, f)
      filename <- paste("EV_ECARTS_", f,"_",tablename,"_", windowStr,".png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
  
      plot <- timestampChronogramPlot(n, tablename, f)
      filename <- paste("EV_CHRONO_", f,"_",tablename,"_", windowStr,".png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    }
  }
}
#GENERATING PENURY DATA (concerns solid+fluid data)

penuryIndicatorPlot<- function(levels, title){
  return(ggplot(levels[with(levels, order(levels$ticks, -xtfrm(levels$importance))),]) + 
           geom_line(aes(x=ticks, y="0. AUCUN RISQUE", linetype="temoin"))  + 
           geom_line(aes(x=ticks, y="1. POTENTIELLE >= 1h", linetype="temoin"))  + 
           geom_line(aes(x=ticks, y="2. POSSIBLE < 1h", linetype="temoin"))  + 
           geom_line(aes(x=ticks, y="3. BIENTOT < 30m", linetype="temoin"))  + 
           geom_line(aes(x=ticks, y="4. IMMINENT < 5m", linetype="temoin"))  + 
           geom_line(aes(x=ticks, y="5. PANNE TOTALE < 1m", linetype="temoin"))  +
           geom_line(aes(x=ticks, y=importance, color=type, group=type, linetype="donnée"))  + 
           labs(x="TIME (ticks)", y="ESTIMATED TIME BEFORE CATASTROPHE", title=title))
}

generatePenuryStats <- function(timespan=-1){
  ##TODO generate it per force
  for (f in dbReadTable(con, "forces")$force) {
    #data type
    dt <- c()
    #time in ticks
    x  <- c()
    #estimated time
    et <- c()
    #importance
    im <- c()
    levels<-c()
    
    for (data_type in unique(stat_tables$datatype)) {
      stock <- dbReadTable(con, stat_tables[stat_tables$datatype == data_type& stat_tables$measure == "STOCK", ]$tablename)
      input <-  dbReadTable(con, stat_tables[stat_tables$datatype == data_type& stat_tables$measure == "INPUT", ]$tablename)
      output <-  dbReadTable(con, stat_tables[stat_tables$datatype == data_type& stat_tables$measure == "OUTPUT", ]$tablename)
      
      
      if(timespan > 0){
        windowStr <- paste("OF_LAST_", as.character(timespan), "_TICKS", sep="")
        stock <- stock[stock$force==f & stock$ticks >= max(stock$ticks) - timespan,]
        input <- input[input$force==f & input$ticks >= max(input$ticks) - timespan,]
        output <- output[output$force==f & output$ticks >= max(output$ticks) - timespan,]
        
      }else{
        
        windowStr <- paste("OF_ALL_TIME", sep="")
        stock <- stock[stock$force==f,]
        input <- input[input$force==f,]
        output <- output[output$force==f,]
      }
      print(paste(force, data_type, "PENURY ANALYSIS, TIMESPAN :", windowStr))
      flush.console()             
      
      stock <- getTypeFilteredData(stock, penuryFilter)
      input <- getTypeFilteredData(input, penuryFilter)
      output <- getTypeFilteredData(output, penuryFilter)
      
      stock_t <- getTendanceTable(stock)
      
      for (ti in unique(stock$ticks)) {
        for (ty in unique(stock[stock$ticks==ti,]$type)) {
          x <- c(x, ti)
          dt <- c(dt, ty)
          
          #big penury =not enough stock+input for 1 minute (60**3 ticks)
          #fatal penury= not enough stock+input for 1 output cycle (1  tick)
          #time in ticks before zero = -current stock /(current production - max consumption recorded)
          curr_st <- stock[stock$ticks==ti & stock$type == ty,]$value
          curr_st_t <- stock_t[stock_t$ticks==ti & stock_t$type == ty,]$value
          curr_in <- input[input$ticks==ti & input$type == ty,]$value
          max_cons <- max(output[output$type==ty,]$value)
          #we avoid any division par zero et on ne s occupe que du t > 0, qui veut dire que les stocks diminuent
          if (curr_st_t < 0){
            t <- -curr_st/curr_st_t
            et <- c(et, t)
            if(t < 3600){#1 minute
              im <- c(im, "5. PANNE TOTALE < 1m")
            }
            else if(t < 18000){#5 minutes
              im <- c(im, "4. IMMINENT < 5m")
            }
            else if (t < 108000){#30 min -
              im <- c(im, "3. BIENTOT < 30m")
            }
            else if (t < 216000) {# 1 heure et -
              im <- c(im, "2. POSSIBLE < 1h")
            }
            else{
              im <- c(im, "1. POTENTIELLE >= 1h")
            }
          }
          else{
            et <- c(et, 0)
            im <- c(im, "0. AUCUN RISQUE")
          }
  
        }
      }
      
    }
    
    if (length(x) > 0){
      
      
      
      levels<-data.frame(x,dt,im,et)
      colnames(levels) <- c("ticks", "type", "importance","estimatedTime")
      
      plot <- penuryIndicatorPlot(levels, paste("TEAM", f," FILTERED PENURY LEVELS ",windowStr," :", sep=" "))
      filename <- paste("FILTERED_ST_DATA_", f,"_PENURY_LEVELS_",windowStr, ".png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    }
  }
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
                          
  
  
  prereq <- dbReadTable(con, "research_prereq")
  researches <-dbReadTable(con, "researches")
  
  for(f in forces$force){
    print(paste(f, "RESEARCH ANALYSIS: ",as.character(length(objectives))," OBJECTIVES"))
    flush.console()  
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
    colnames(results) <- c("r_names", "pre_r_names")
    
    
    
    net <- graph_from_data_frame(d=results,  directed=T)

    researched <- dbReadTable(con, "RESEARCHED")
    researched <- researched[researched$force == f & researched$type == "name", ]$value
    not_researchable <- relevant_prereq[!relevant_prereq$prereq_r_name %in% researched, ]$r_name
    not_researchable_yet <- relevant_prereq[relevant_prereq$prereq_r_name %in% researched & relevant_prereq$r_name %in% not_researchable, ]$r_name
    researchable <- relevant_prereq[! relevant_prereq$r_name %in% not_researchable,]$r_name
    
    if( length(unique(relevant_prereq$r_name)) < 50 ){
      print(paste(f, "RESEARCH ANALYSIS: ",as.character(length(unique(relevant_prereq$r_name)))," ELEMENTS TECH TREE COMPLETION"))
      flush.console()  
    
    c_unresearchable <- "gray75"
    c_researchable <- "darkred"
    c_researched <- "gray25"
    
    c_important <- "grey75"
    c_notimportant <- "grey85"
    
    c_researched_edges <- "orange"
    c_researchable_edges <- "green"
    c_not_researchable_yet_edges <- "red"
    c_other_edges <- "grey70"
    
    wd_researched_edges <- 2
    wd_other_edges <- 1
    
    sz_important <- 10
    sz_notimportant <- 5
    
    sh_important <- "circle"
    sh_notimportant <- "circle"
    
    c_sh_researched <- "gold"
    c_sh_researchable <- "orange"
    c_sh_important <- "grey70"
    c_sh_not_important <- "grey80"
    
    
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
    
    
    l <- layout_with_lgl(net)
    l <- norm_coords(l, ymin=-1, ymax=1, xmin=-1, xmax=1)
    png(width=16, height = 9, dpi=300)
    plot<- plot( net, 
         asp=16/9, 
         rescale=F, 
         edge.color=c_edges,
         label=results$r_name,
         edge.width=wd_edges,
         layout=l*1.3,
         vertex.label.cex=.7,
         vertex.label.font=2,
         edge.arrow.mode=0)
    legend(x=1.5,
           y=-1,
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
    filename <- paste("RESEARCH_", f,"_GOALS_COMPLETION_GRAPH.png", sep="" )
    save(plot, file=filename )
    dev.off()
    }
    else{
      print(paste(f, "RESEARCH ANALYSIS: ",as.character(length(unique(relevant_prereq$r_name)))," ELEMENTS TECH TREE COMPLETION SKIPPED (>50)"))
      flush.console()  
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
      geom_bar(position="dodge", stat="identity")+
      labs(x="RESEARCH OBJECTIVE", y="COMPLETION BARS", title=paste("TEAM", f, "RESEARCH GOALS:" ))
    filename <- paste("RESEARCH_", f,"_GOALS_COMPLETION_BARS.png", sep="" )
    ggsave(plot, file=filename, width=16, height = 9, dpi=300)
  }
}

##### SCRIPT CONFIGURATION

working_dir <- "D:\\FactoLogAnalysis"


##### DB CONFIGURATION

dbpath <- "C:/Users/Chris/PycharmProjects/FactoLog Data Miner/resources/"
dbfilename <- "testmap.db"

##### PLOTLY CONFIGURATION

Sys.setenv("plotly_username"="helldragger")
Sys.setenv("plotly_api_key"="5VZ8a6OdUNPjLflaHHHP")
plotly_output <- FALSE


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

setwd(working_dir)

##### DB LOADING

con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))

dbListTables(con)
stat_tables <- dbReadTable(con, "stat_tables")
stock_tables <- stat_tables[stat_tables$measure == "STOCK", ]
input_tables <- stat_tables[stat_tables$measure == "INPUT", ]
output_tables <- stat_tables[stat_tables$measure == "OUTPUT", ]
forces <- dbReadTable(con, "forces")
event_tables <- dbReadTable(con, "event_tables")
researches <- dbReadTable(con, "researches")


##### FILTERS

penuryFilter <- c("iron-ore","iron-plate","copper-plate","steel-plate","copper-ore","coal","stone", "crude-oil", "light-oil", "heavy-oil", "petroleum-gas", "lubricant", "water", "steam")
researchFilter <- c("construction-robotics", "logistic-system", "automated-rail-transportation", "oil-processing", "atomic-bomb", "artillery", "tanks", "nuclear-power", "modules", "rocket-silo")

researchFilter2 <- c("automated-rail-transportation", "oil-processing", "modules", "rocket-silo")

researchFilter3 <- c("modules")

##### 

generateProdStats(timespan=analysis_output_close_timespan)
generateEventChronograms(timespan=analysis_output_close_timespan)
generatePenuryStats(timespan=analysis_output_close_timespan)

generateProdStats(timespan=analysis_output_medium_timespan)
generateEventChronograms(timespan=analysis_output_medium_timespan)
generatePenuryStats(timespan=analysis_output_medium_timespan)

generateProdStats(timespan=analysis_output_large_timespan)
generateEventChronograms(timespan=analysis_output_large_timespan)
generatePenuryStats(timespan=analysis_output_large_timespan)

generateProdStats()
generateEventChronograms()
generatePenuryStats()

generateResearchTree(researchFilter)

