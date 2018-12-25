library("ggplot2")
library("reshape2")

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

researchObjectiveGraph <- function(relevant_prereq, objectives, researched, not_researchable, not_researchable_yet, researchable, f){
  net <- graph_from_data_frame(d=relevant_prereq,  directed=T)
  
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
        label=relevant_prereq$r_name,
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

researchObjectiveCompletionPercentageBars <- function(tech_progress, f){
  return(ggplot(melt(tech_progress), aes( x=objective, y=value, color=data_type, fill=data_type, group=objective))+
    geom_bar(position="fill", stat="identity")+
    labs(x="RESEARCH OBJECTIVE", y="COMPLETION PERCENTAGE", title=paste("TEAM", f, "RESEARCH GOALS:" )))
}

researchObjectiveCompletionBars <- function(tech_progress, f){
  return(ggplot(melt(tech_progress), aes( x=objective, y=value, color=data_type, fill=data_type, group=objective))+
           geom_bar(position="stack", stat="identity")+
           labs(x="RESEARCH OBJECTIVE", y="COMPLETION BARS", title=paste("TEAM", f, "RESEARCH GOALS:" )))
}