get_data<-function(){
  
  outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
  outcome
}

standarize_income_char<-function(var){
  var <- toupper(var)
  var <- trimws(var)
  var
}

num_check<-function(var){
  rank<-NULL
  if (var=='BEST'){
    rank<-1
  } else if(var=='WORST'){
    rank<-NULL
  } else if(is.numeric(as.numeric(var))){
    rank<-as.numeric(var) #validar que no sea flotante
  } else rank<- -1
  rank
}

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

rankhospital<-function(state,outcome,num="best"){
  # get data
  data<-get_data()
  # make outcome trimmed and UPPERCASE
  outcome<-standarize_income_char(outcome)
  # make num trimmed and UPPERCASE
  num<-standarize_income_char(num)
  # make state trimmed and UPPERCASE
  state<-standarize_income_char(state)
  # get the column to analyse
  outcome_col<-outcome_check(outcome)
  # get the rank to get
  num_id<-num_check(num)
  if (outcome_col==FALSE){
    #return a personalized string of error
    stop("invalid outcome")
  }
  if (!state_check(state)){
    #return a personalized string of error
    stop("invalid state")
  }
  # coerse column into numeric
  data[[outcome_col]]<-suppressWarnings(as.numeric(data[[outcome_col]]))
  # only keeping not null columns
  data<-data[,c("Hospital.Name",outcome_col,"State")]
  data<-data[complete.cases(data),]
  # only keeping data of the desired state, possible error if state exists and values are null
  data<-data[data["State"]==state,]
  # order data to rank later
  data<-data[order(data["Hospital.Name"]),]
  # rank column based on the outcome column
  data[['rank']]<-rank(data[[outcome_col]],ties.method = "first")

  ## return the name of the hospital with the ranking specified
  if (is.numeric(num_id)){
    hn<- data[data$rank==num_id,c("Hospital.Name")]
    if (length(hn)==0){
      return (NA)
    } else {
      return (hn) 
    }
  }else if(is.null(num_id)){
    return (data[order(data["rank"],decreasing=TRUE) ,][1,"Hospital.Name"])
  }
}


#Falta Agregar al cheat sheet
#stop
#rank



