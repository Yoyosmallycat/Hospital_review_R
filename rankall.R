rankall <- function(outcome, num = "best"){
  rank_df <- data.frame(matrix(ncol = 2, nrow = 0))
  col_name <- c("hospital", "state")
  colnames(rank_df) <- col_name
  mydf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # subset columns(2,7,11,17,23), save as sub-df
  sub_df <- mydf[, c(2, 7, 11, 17, 23)]
  
  na_remove <- sub_df[complete.cases(sub_df), ]
  #replace the "." in goutcone with " "
  specific = gsub(" ", ".", outcome)
  
  # subset column comtaining the string of specific, igore the case
  sel_col <- sub_df[,grep(specific, colnames(sub_df), ignore.case=TRUE, value=TRUE)]
  sel_df <- data.frame(hospital = sub_df$Hospital.Name, state = sub_df$State,  outcome = as.numeric(sel_col))

  #remove rows containing NA
  final_df <- sel_df[complete.cases(sel_df), ]
  print(final_df)
  sorted_df <- final_df[with(final_df, order(final_df$state,final_df$outcome, final_df$hospital)),]
  
  state_vec <- unique(sorted_df[ , 2] )
  sort(state_vec)
  len <- length(state_vec)
  
  for ( i in 1 : len) {
    state_df <- sorted_df[sorted_df$state == state_vec[i], ]
    if (num == "best"){
      rank_df[nrow(rank_df) + 1, ] = state_df[1,(1:2)]
      }
    else if (num >=1 && num <= nrow(state_df)){
      rank_df[nrow(rank_df) + 1, ] = state_df[num, (1:2)]
    }
    else if (num == "worst"){
      rank_df[nrow(rank_df) + 1, ] = state_df[nrow(state_df), (1:2)]
    }
    else{
      rank_df[nrow(rank_df) + 1, ] = list("NA", state_vec[i])
    }
  }
 rank_df 
}

head(rankall("heart attack", 20), 10)
rankall("heart attack", 50)
head(rankall("heart attack", "best"))

tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

r <- rankall("heart attack", 7)
r
as.character(subset(r, state == "WA")$hospital)
