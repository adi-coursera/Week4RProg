library(data.table)
source("rankhospital.R")
rankall <- function(outcome,rank="best"){
      states <- fread("outcome-of-care-measures.csv")$State
      states <- unique(states)
      states <- sort(states)
      list <- list()
      for(c in states){
            list <- rbind(list,list(hospital=rankhospital(c,outcome,rank),state=c))
      }
      list
}