library(data.table)
bestany <- function(DT, state, outcome) {
      outcomedata <- DT
      if (!(state %in% outcomedata$State)) {
            stop("invalid state")
            
      }   ## Stops code running if wrong state
      if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
            stop("invalid outcome")
      }   ## Stops code running if wring outcome
      data <-
            outcomedata[outcomedata$State == state,]#Keep relevant state data
      if (outcome == "heart attack") {
            data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack` <-
                  as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`)
            ##Convert heart attack mortality data to numeric
            minval <-
                  min(na.omit(
                        data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`
                  )) ##find the minimum of the heart attack mortality
            data <-
                  data[`Hospital 30-Day Death (Mortality) Rates from Heart Attack` ==
                             minval, ]#Keep only those rows which are minimum
            names <-
                  sort(data$`Hospital Name`)#sorted list of hospital names
            invisible(names)
            
      }   ## Heart attack outcome
      else if (outcome == "heart failure") {
            data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure` <-
                  as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`)
            ## Convert heart failure mortality data to numeric
            minval <-
                  min(
                        na.omit(
                              data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`
                        )
                  ) ##find the minimum of the heart failure mortality
            data <-
                  data[`Hospital 30-Day Death (Mortality) Rates from Heart Failure` ==
                             minval, ]#Keep only those rows which are minimum
            names <-
                  sort(data$`Hospital Name`)#sorted list of hospital names
            
            invisible(names)
            
      }   ## Heart Failure outcome
      if (outcome == "pneumonia") {
            data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia` <-
                  as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`)
            ## Convert pnuemonic mortality data to numeric
            minval <-
                  min(na.omit(
                        data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`
                  )) ##find the minimum of the heart attack mortality
            data <-
                  data[`Hospital 30-Day Death (Mortality) Rates from Pneumonia` ==
                             minval, ]#Keep only those rows which are minimum
            names <-
                  sort(data$`Hospital Name`)#sorted list of hospital names
            invisible(names)
            
      }## Heart attack outcome
      invisible(names)
}
best <- function(state, outcome){
      bestany(fread("outcome-of-care-measures.csv"),state,outcome)[[1]]
}