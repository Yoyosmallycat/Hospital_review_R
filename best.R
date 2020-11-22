best <- function(state, outcome){
  mydf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  sub_df <- mydf[, c(2, 7, 11, 17, 23)]
  sub_state <- sub_df[sub_df$State == state, ]
  specific = gsub(" ", ".", outcome)
  sel_col <- sub_state[,grep(specific, colnames(sub_state), ignore.case=TRUE, value=TRUE)]
  sel_df <- data.frame(State = sub_state$State, Hospital_Name = sub_state$Hospital.Name, outcome = as.numeric(sel_col))
  sorted_df <- sel_df[with(sel_df, order((sel_df$outcome),sel_df$Hospital_Name)),]
sorted_df$Hospital_Name[1]
  
}


best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
