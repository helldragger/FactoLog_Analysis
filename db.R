library("RSQLite")
dbpath <- "C:/Users/Chris/PycharmProjects/FactoLog Data Miner/resources/"
dbfilename <- "testmap.db"

con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
table_list <- dbListTables(con)
analyzed_data <- dbReadTable(con, "analyzed_data")
completed_researches <- dbReadTable(con, "completed_researches")
forces <- dbReadTable(con, "forces")
last_data <- dbReadTable(con, "last_data")
last_update <- dbReadTable(con, "last_update")
raw_data <- dbReadTable(con, "raw_data")
researches_prerequisites <- dbReadTable(con, "researches_prerequisites")
stat_views <- dbReadTable(con, "stat_views")
unlocked_all <- dbReadTable(con, "unlocked_all")
unlocked_fluids <- dbReadTable(con, "unlocked_fluids")
unlocked_gifts <- dbReadTable(con, "unlocked_gifts")
unlocked_gun_speed <- dbReadTable(con, "unlocked_gun_speed")
unlocked_items <- dbReadTable(con, "unlocked_items")
unlocked_others <- dbReadTable(con, "unlocked_others")
unlocked_recipes <- dbReadTable(con, "unlocked_recipes")
unlocked_turret_attack <- dbReadTable(con, "unlocked_turret_attack")

dbDisconnect(con)


getView <- function(view_name){
  con <- dbConnect(RSQLite::SQLite(), paste(dbpath,dbfilename, sep=""))
  view <- dbReadTable(con, view_name)
  dbDisconnect(con)
  return(view)
}

getTableList <- function(){
  return(table_list)
}

getAllForces <- function(){
  return(unique(forces$force))
}

getForcesFromTable <- function(table){
  return(unique(table$force))
}


getCompletedResearches <- function(){
  return(completed_researches)
}

getResearchesPrerequisites <- function(){
  return(researches_prerequisites)
}

getRawData <- function(){
  return(raw_data)
}

getAnalyzedData <- function(){
  return(analyzed_data)
}

getStatsViewNames <- function(){
  return(stat_views$view_name)
}

getLastUpdate <- function(){
  return(last_update)
}

getUnlockedAll <- function(){
  return(unlocked_all)
}

getUnlockedItems <- function(){
  return(unlocked_items)
}

getUnlockedFluids <- function(){
  return(unlocked_fluids)
}

getUnlockedGifts <- function(){
  return(unlocked_gifts)
}

getUnlockedGunSpeed <- function(){
  return(unlocked_gun_speed)
}

getUnlockedOthers <- function(){
  return(unlocked_others)
}

getUnlockedRecipes <- function(){
  return(unlocked_recipes)
}

getUnlockedTurretAttack <- function(){
  return(unlocked_turret_attack)
}

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