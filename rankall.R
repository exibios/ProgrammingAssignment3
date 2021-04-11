##read the data
get_data<-function(){
  
  outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
  outcome
}
## function to rank a dataframe previously ordered by the "col" of choise
ranker <- function(x=data.frame(),col){
  x[['rank']]<-rank(x[[col]],ties.method = "first")
  x
}

# to lower and trim a variable
standarize_income_char<-function(var){
  var <- toupper(var)
  var <- trimws(var)
  var
}

# to return an interpretable varaible of the rank needed
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

# to validate the outcome column
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

# principal function
rankall<-function(outcome,num='best'){
  # get data
  data<-get_data()
  # make outcome trimmed and UPPERCASE
  outcome<-standarize_income_char(outcome)
  # make num trimmed and UPPERCASE
  num<-standarize_income_char(num)
  # get the column to analyse
  outcome_col<-outcome_check(outcome)
  # get the rank to get
  num_id<-num_check(num)
  if (outcome_col==FALSE){
    #return a personalized string of error
    stop("invalid outcome")
  }
  #keep only the necessary columns
  imdf <- data[c("State","Hospital.Name",outcome_col)]
  #transform the outcome column into numeric
  imdf[[outcome_col]] <- suppressWarnings(as.numeric(data[[outcome_col]]))
  #order the dataframe by name
  imdf<-imdf[order(imdf["Hospital.Name"]),]
  #only columns with data on outcome
  imdf_useful <- imdf[complete.cases(imdf),]
  #split to divide the dataframe (into a list of dataframes) by State factors
  imdf_list <- split( imdf_useful , imdf_useful$State )
  #process the list of dataframes by the ranker function and send it the column 
  #to rank every element (the list of dataframes)
  final <- lapply( imdf_list , ranker , col=outcome_col )
  #prepare the final dataframe with the final columns
  final_df<-data.frame(State=NA,Hospital.Name=NA)
  #iterate every element of States in the csv data
  for (x in unique(imdf$State)){
    # temporary data.frame to later concatenate
    tmp_df<-data.frame(State=NA,Hospital.Name=NA)
    # the state analyzed is assigned to the temporary dataframe
    tmp_df[["State"]]<-x
    # filtering the info of the State in the list of dataframes
    tmp<-final[[x]]
    # getting the rank number sent and returning the Hospital name
    if (is.numeric(num_id)){
      # best is 1 always
      tmp_df["Hospital.Name"]=tmp[tmp['rank']==num_id,][['Hospital.Name']]
    } else if (is.null(num_id)){
      # NULL is for the worst
      tmp_df["Hospital.Name"]=tmp[order(tmp['rank'],decreasing = TRUE),][1,'Hospital.Name']#[['Hospital.Name']]
    } 
    
    # concatenate into the final dataframe the temporal dataframe
    final_df <- rbind(final_df,tmp_df)
  }
  #final_df[order(final_df["State"]),]
  final_df<-final_df[complete.cases(final_df),]
  final_df[order(final_df["State"]),]
  colnames(final_df)<-c("state","hospital")
  final_df
}
