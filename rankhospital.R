rankhospital <- function(state, outcome, num = "best"){
  mydf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  sub_df <- mydf[, c(2, 7, 11, 17, 23)]
  sub_state <- sub_df[sub_df$State == state, ]
  specific = gsub(" ", ".", outcome)
  sel_col <- sub_state[,grep(specific, colnames(sub_state), ignore.case=TRUE, value=TRUE)]
  sel_df <- data.frame(State = sub_state$State, Hospital_Name = sub_state$Hospital.Name, outcome = as.numeric(sel_col))
  #remove rows containing NA
  final_df <- sel_df[complete.cases(sel_df), ]
  sorted_df <- final_df[with(final_df, order((final_df$outcome),final_df$Hospital_Name)),]
  if (num == "best"){
    sorted_df$Hospital_Name[1]}
    else if (num >=1 && num <= nrow(sorted_df)){
      sorted_df$Hospital_Name[num]
    }
    else if (num == "worst"){
      sorted_df$Hospital_Name[nrow(sorted_df)]
    }
    else{
      "NA"
    }
}
 
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("WA", "heart attack", 7)
rankhospital("NY", "heart attack", 7)
