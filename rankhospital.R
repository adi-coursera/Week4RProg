library(data.table)
source("best.R")
minlist <- function(frame, state, outcome){
      name <- bestany(frame, state, outcome)
      sort(name)
      interlist <- data.table()
      for(c in name){
            data <- frame[frame$`Hospital Name`==c,]
            if(outcome=="heart attack"){
                  min <- min(data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`)
                  data <- data[`Hospital Name`==c & `Hospital 30-Day Death (Mortality) Rates from Heart Attack`==min]
            }
            if(outcome=="heart failure"){
                  min <- min(data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`)
                  data<- data[data$`Hospital Name`==c & data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`==min]
            }
            if(outcome=="pneumonia"){
                  min<- min(data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`)
                  data <- data[data$`Hospital Name`==c & data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`==min]
            }
                  
            
            interlist <- rbind(interlist, data)
      }
      invisible(interlist)
}
minremove <- function(frame, state, outcome){
      name <- bestany(frame, state, outcome)
      sort(name)
      interlist <- data.table()
      for(c in name){
            data <- frame[frame$`Hospital Name`==c,]
            if(outcome=="heart attack"){
                  min <- min(data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`)
                  frame <- frame[frame$`Hospital Name`!=c | frame$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`!=min]
            }
            if(outcome=="heart failure"){
                  min <- min(data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`)
                  frame<- frame[frame$`Hospital Name`!=c | frame$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`!=min]
            }
            if(outcome=="pneumonia"){
                  min<- min(data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`)
                  frame <- frame[frame$`Hospital Name`!=c | frame$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`!=min]
            }
            
      }
      invisible(frame)
}
rankhospitallist <- function(state, outcome){
      data <- fread("outcome-of-care-measures.csv")[State==state,]
      list <- data.table()
      if(outcome=="heart attack"){
            data <- data[!is.na(as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`)), ]
      }
      if(outcome=="heart failure"){
            data <- data[!is.na(as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`)),]
      }
      if(outcome=="pneumonia"){
            data <- data[!is.na(as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`)),]
      }
      while(dim(data)[1]!=0){
            list <- rbind(list,minlist(data,state,outcome))
            data <- minremove(data,state,outcome)
      }
      invisible(list)
}
rankhospital <- function(state, outcome, num="best"){
      list<- rankhospitallist(state,outcome)

      if(num=="best"){
            list[1,]$`Hospital Name`
      }
      else if(num=="worst"){
            list[dim(list)[1],]$`Hospital Name`
      }
      else if(1<=num & num<=dim(list)[1] & is.numeric(num) & num - floor(num)==0 ){
            list[num,]$`Hospital Name`
      }
      else NA
      
}

