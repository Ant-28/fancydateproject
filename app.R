if(!require("tidyverse")) install.packages("tidyverse")

if(!require("stringr")) install.packages("stringr")

if(!require("shiny")) install.packages("shiny")

library(tidyverse)

library(stringr)

library(shiny)

# Prereq functions
# Leap year detection
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


# find ordinal index
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
  if(month <= 1 && month >= 12) stop("Error: Bad Month")
  else{
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
  )}
  
  
}

# date formats

formats <- c("DD/MM/YYYY", "MM/DD/YYYY", "YYYY/MM/DD", "YYYY/DD/MM")

dateRegexSelector <- function(format){
  if(format == "DD/MM/YYYY" || format == "MM/DD/YYYY"){      return("^(\\d{2})/(\\d{2})/(\\d{4})$")
  }
 else if(format == "YYYY/MM/DD" || format == "YYYY/DD/MM"){ return("^(\\d{4})/(\\d{2})/(\\d{2})$")
 }


}

index_selector <- function(format){
  if(format == "DD/MM/YYYY"){
    return(c(3,2,2,4))
  }
  else if(format == "MM/DD/YYYY"){
    return(c(2,3,3,4))
  }
  else if(format == "YYYY/MM/DD"){
    return(c(3,4,4,2))
  }
  else if(format == "YYYY/DD/MM"){
    return(c(4,3,3,2))
  }
}


fancy_date <- function(format, date){
  if(!(str_detect(date, dateRegexSelector(format)))){
    return("Please enter a valid date")
  }
  else{
  foo <- str_match(date, pattern = dateRegexSelector(format))
  indices <- index_selector(format)
  month <- foo[1,indices[1]] %>% as.numeric()
  day <- foo[1, indices[2]]
  day2 <- foo[1, indices[3]] %>% as.numeric()
  year <- foo[1, indices[4]] %>% as.numeric()
  # the worst if statement I've ever made just to check if dates are ok
  if((month == 2 && !(day2 %in% c(1:28)) && !is_leapyear(year)) || (month == 2 && !(day2 %in% c(1:29)) && is_leapyear(year))  || ((month %in% c(4,6,9,11)) && !(day2 %in% c(1:30))) ||((month %in% c(1,3,5,7,8,10,12)) && !(day2 %in% c(1:31))) || (!(month %in% 1:12)))
  {return(paste("Error: Date does not exist, please input date in ", format, sep=""))}
  else{
    month <- month_detect(month)
    day <- ordinal_index(day)
    return(paste0(day, " of ", month, ", ", year))
  }}

}






# Shiny Stuff Here




  
  ui <- fluidPage(
  selectInput(inputId = "format", label = "Select a date format: ",choices = formats),
  textInput("date", label = "Enter a date: ", value = "01/01/1990"),
  textOutput(outputId = "fancydate")
)

server <- function(input, output, session) {
  
  
  
  
  
  output$fancydate <- renderText({
    fancy_date(input$format, input$date)
  })
  
  
  
  
}

shinyApp(ui, server)
