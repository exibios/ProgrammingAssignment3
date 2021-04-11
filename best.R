#outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
#head(outcome)
#ncol(outcome)
#names(outcome)
#nrow(outcome)
#outcome[,11]<-as.numeric(outcome[,11])
#hist(outcome[,11])
get_data<-function(){
  
  outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
  #outcome[,11]<-as.numeric(outcome[,11])
  outcome
}

standarize_income_char<-function(var){
  var <- toupper(var)
  var <- trimws(var)
  var
}
# Check if is a valid outcome
outcome_check<-function(var){
  colName<-FALSE
  if(var=="HEART ATTACK"){ #11
    colName<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if(var=="HEART FAILURE"){ #17
    colName<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if(var=="PNEUMONIA"){ #23
    colName<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } 
  colName
}
#Check if is valid state
state_check<-function(var){
  data<-get("data",parent.frame(n=1))
  if (is.element(var,unique(data[['State']]))){
   return(TRUE)
  } else {
    return(FALSE)
  }
    
}
best<-function(state,outcome){
  # Read Data
  data<-get_data()
  # make outcome trimmed and UPPERCASE
  outcome<-standarize_income_char(outcome)
  # make state trimmed and UPPERCASE
  state<-standarize_income_char(state)
  # get column number of the outcome 
  ## TODO automatically check which column should return, by regexp name is not possible 
  ## because there is more than one column with the outcome name alike
  outcome_col<-outcome_check(outcome)
  if (outcome_col==FALSE){
    #return a personalized string of error
    #return (cat(paste("Error in best(\"",state,"\",\"",outcome,"\") : invalid outcome",collapse = NULL,sep="")))
    stop("invalid outcome")
  }
  if (!state_check(state)){
    #return a personalized string of error
    #return (cat(paste("Error in best(\"",state,"\",\"",outcome,"\") : invalid state",collapse = NULL,sep="")))
    stop("invalid state")
  }
  #only the necessary columns 
  data_filtered <- data[data$State==state,c(outcome_col,"Hospital.Name")]
  #transform the column into numeric value
  #Using suppressWarnings to get a cleaner output when coercing data
  data_filtered[[outcome_col]]<-suppressWarnings(as.numeric(data_filtered[[outcome_col]]))
  #sort the data.frame by the column needed and only returning the first row
  #(less mortality rate) so the column returned is "Hospital.Name"
  data_filtered[order(data_filtered[outcome_col]),][1,"Hospital.Name"]
}

#Falta Agregar al cheat sheet
#suppressWarnings
#cat
#is.element
#get("x",parent.frame(n=1))
#as.numeric
#order


