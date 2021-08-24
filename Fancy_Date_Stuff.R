if(file.exists(".Rdata")) load(".Rdata")



# leap year detection function

is_leapyear <- function(year){
  if(year %% 4 == 0){
    if(year %% 100 == 0)
    {
      if(year %% 400 == 0)
      {
        return(TRUE)
      }
      else{
        return(FALSE)
      }}
    else{
      return(TRUE)
    }}  
  else{
    return(FALSE)
  }  
}

ordinal_index <- function(numstring){
# first digit of two digit number

  imp_num <- str_sub(numstring, start = 1, end = 1) %>% as.numeric()
  
  
# second digit of two digit number  
  imp_num2 <- str_sub(numstring, start = 2, end = 2) %>% as.numeric() 
  ind <- if(imp_num2 %in% c(4:9)){("th") 
  } else if(imp_num2 == 1) {("st")
  } else if(imp_num2 == 2) {("nd")
  } else if(imp_num2 == 3) {("rd")
  } else {("error, number problem")
    stop()}
# Remove zero when writing day as ordinal value
# if day is single digit value  
  day <- if(imp_num == 0) {(as.character(imp_num2))
  } else {(as.character(numstring))}
  day <- paste0(day, ind)
  return(day)
}


month_detect <- function(month){
  case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March",
    month == 4 ~ "April",
    month == 5 ~ "May",
    month == 6 ~ "June",
    month == 7 ~ "July",
    month == 8 ~ "August",
    month == 9 ~ "September",
    month == 10 ~ "October",
    month == 11 ~ "November",
    month == 12 ~ "December",
    TRUE ~ "Something's Wrong."
  )
  
  
}



dateImp <- function(){
  
  
format <- readline("Specify a Format: \n 1 For DD/MM/YYYY \n 2 for MM/DD/YYYY \n 3 for YYYY/MM/DD \n 4 for YYYY/DD/MM")   
if(!(format %in% c(1:4))){
  print("Error: Please input a value between 1 to 4")
  stop() 
  
}
date <- readline("Enter a Date:\n")
hist <- readline("Do you want to know how many times you have run this function? (y/n): ")
if(hist == "y"){
  print(paste0(history))
}
  
if(format == "1"){  
  pattern_with_groups <- "^(\\d{2})/(\\d{2})/(\\d{4})$"
  foo <- str_match(date, pattern_with_groups) 
  month <- foo[1,3] %>% as.numeric()
  day <- foo[1,2]
  day2 <- foo[1,2] %>% as.numeric()
  year <- foo[1,4] %>% as.numeric()
  if(
    (month == 2 && !(day2 %in% c(1:28)) && !is_leapyear(year)) || (month == 2 && !(day2 %in% c(1:29)) && is_leapyear(year))  || ((month %in% c(4,6,9,11)) && !(day2 %in% c(1:30))) ||((month %in% c(1,3,5,7,8,10,12)) && !(day2 %in% c(1:31))) || (!(month %in% 1:12)))
  {print("Error: Date does not exist, please input date in DD/MM/YYYY")
    stop()}
  else{
    month <- month_detect(month)
    day <- ordinal_index(day)
    noquote(" ")
    return(paste0(day, " of ", month, ", ", year))
  }}
else if(format == "2"){
  pattern_with_groups <- "^(\\d{2})/(\\d{2})/(\\d{4})$"
  foo <- str_match(date, pattern_with_groups) 
  month <- foo[1,2] %>% as.numeric()
  day <- foo[1,3]
  day2 <- foo[1,3] %>% as.numeric()
  year <- foo[1,4] %>% as.numeric()
  if(
    (month == 2 && !(day2 %in% c(1:28)) && !is_leapyear(year)) || (month == 2 && !(day2 %in% c(1:29)) && is_leapyear(year))  || ((month %in% c(4,6,9,11)) && !(day2 %in% c(1:30))) ||((month %in% c(1,3,5,7,8,10,12)) && !(day2 %in% c(1:31))) || (!(month %in% 1:12)))
  {print("Error: Date does not exist, please input date in MM/DD/YYYY")
    stop()}
  else{
    month <- month_detect(month)
    day <- ordinal_index(day)
    noquote(" ")
    return(paste0(day, " of ", month, ", ", year))
  }}
else if(format == "3"){
  pattern_with_groups <- "^(\\d{4})/(\\d{2})/(\\d{2})$"
  foo <- str_match(date, pattern_with_groups) 
  month <- foo[1,3] %>% as.numeric()
  day <- foo[1,4]
  day2 <- foo[1,4] %>% as.numeric()
  year <- foo[1,2] %>% as.numeric()
  if(
    (month == 2 && !(day2 %in% c(1:28)) && !is_leapyear(year)) || (month == 2 && !(day2 %in% c(1:29)) && is_leapyear(year))  || ((month %in% c(4,6,9,11)) && !(day2 %in% c(1:30))) ||((month %in% c(1,3,5,7,8,10,12)) && !(day2 %in% c(1:31))) || (!(month %in% 1:12)))
  {print("Error: Date does not exist, please input date in YYYY/MM/DD")
    stop()}
  else{
    month <- month_detect(month)
    day <- ordinal_index(day)
    noquote(" ")
    return(paste0(day, " of ", month, ", ", year))
  }}
else if(format == "4"){
  pattern_with_groups <- "^(\\d{4})/(\\d{2})/(\\d{2})$"
  foo <- str_match(date, pattern_with_groups) 
  month <- foo[1,4] %>% as.numeric()
  day <- foo[1,3]
  day2 <- foo[1,3] %>% as.numeric()
  year <- foo[1,2] %>% as.numeric()
 
  else{
    month <- month_detect(month)
    day <- ordinal_index(day)
    noquote(" ")
    return(paste0(day, " of ", month, ", ", year))
  }  
  
}

  

}  





# loop script
userinp <- "y"
# initialize history
history <- 0
while(userinp == "y"){

  print(dateImp())
  userinp <- readline("Do you want to try again? (y/n): ")
history <- history + 1  
}
# remove everything except history
list1 <- as.character(ls())
list1 <- list[which(list != "history")]
remove(list = list1)
save.image()
