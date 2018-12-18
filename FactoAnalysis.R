library("RSQLite")
library("plotly")
library("SciViews")
library("stats")
library("glogis")
library("ggplot2")

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


generateProdStats <- function(timespan=-1){

  #Generating separate INPUT, OUTPUT, STOCK AND SUBSEQUENT TENDANCIES GRAPHS
  for (tablename in stat_tables$tablename) {
  
    table <-dbReadTable(con, tablename)
    table <- table[order(table$ticks),]
    for(f in forces$force){
      n <- table[table$force == f,]
      if( nrow(n) > 0){
        plot <- dataPerTickPlot(n, paste("TEAM", f, tablename," DATA:", sep=" "))
        filename <-paste("ST_DATA_", f,"_",tablename,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
        api_create(plot, filename = filename)
        
        plot <- dataPerTickPlot(getTypeFilteredData(n, penuryFilter), paste("TEAM", f, tablename," FILTERED DATA:", sep=" "))
        filename <-paste("FILTERED_ST_DATA_", f,"_",tablename,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
        
        plot <- dataPerTickPlot(getTendanceTable(n), paste("TEAM", f, tablename," TENDANCES:", sep=" "))
        filename <-paste("ST_TENDANCES_", f,"_",tablename,".png", sep="" )
        ggsave(plot, file=filename, width=16, height = 9, dpi=300)
        
        plot <- dataPerTickPlot(getTypeFilteredData(getTendanceTable(n), penuryFilter), paste("TEAM", f, tablename," FILTERED TENDANCES:", sep=" "))
        filename <-paste("FILTERED_ST_TENDANCES_", f,"_",tablename,".png", sep="" )
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
    for (f in unique(table$force)){
      n <- table[table$type == "name" & table$force == f,]
      #name dependent values
      
      plot <- ticksChronogramPlot(n, tablename, f)
      filename <- paste("EV_ECARTS_", f,"_",tablename,".png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
  
      plot <- timestampChronogramPlot(n, tablename, f)
      filename <- paste("EV_CHRONO_", f,"_",tablename,".png", sep="" )
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
      
      stock <- stock[stock$force==f,]
      input <- input[input$force==f,]
      output <- output[output$force==f,]
          
      stock <- getTypeFilteredData(stock, penuryFilter)
      input <- getTypeFilteredData(input, penuryFilter)
      output <- getTypeFilteredData(output, penuryFilter)
      stock_t <- getTendanceTable(stock)
      
      for (ti in unique(stock$ticks)) {
        for (ty in unique(stock[stock$ticks==ti,]$type)) {
          x <- c(x, ti)
          dt <- c(dt, ty)
          #
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
    levels<-data.frame(x,dt,im,et)
    colnames(levels) <- c("ticks", "type", "importance","estimatedTime")
    
    if (nrow(levels) > 0){
      
      plot <- penuryIndicatorPlot(levels, paste("TEAM", f," FILTERED PENURY LEVELS:", sep=" "))
      filename <- paste("FILTERED_ST_DATA_", f,"_PENURY_LEVELS.png", sep="" )
      ggsave(plot, file=filename, width=16, height = 9, dpi=300)
    }
  }
}

cropResults<- function(table, timespan){
  if(timespan == -1)
    return(table)
  return(table[table$ticks >= max(table$ticks)-timespan,])
}

##### DB CONFIGURATION

dbpath <- "C:/Users/Chris/PycharmProjects/FactoLog Data Miner/resources/"
dbfilename <- "testmap.db"

##### PLOTLY CONFIGURATION

Sys.setenv("plotly_username"="helldragger")
Sys.setenv("plotly_api_key"="5VZ8a6OdUNPjLflaHHHP")


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

##### DB LOADING

con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))

dbListTables(con)
stat_tables <- dbReadTable(con, "stat_tables")
stock_tables <- stat_tables[stat_tables$measure == "STOCK", ]
input_tables <- stat_tables[stat_tables$measure == "INPUT", ]
output_tables <- stat_tables[stat_tables$measure == "OUTPUT", ]
forces <- dbReadTable(con, "forces")
event_tables <- dbReadTable(con, "event_tables")

##### FILTERS

penuryFilter <- c("iron-ore","iron-plate","copper-plate","steel-plate","copper-ore","coal","stone", "crude-oil", "light-oil", "heavy-oil", "petroleum-gas", "lubricant", "water")



##### 

generateProdStats()
generateEventChronograms()
generatePenuryStats()


