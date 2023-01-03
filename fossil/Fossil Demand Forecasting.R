library(forecast)
library(tidyverse)
library(zeallot)
library(glue)
getwd()
train = read.csv("./R/fossil/Train.csv")
test = read.csv("./R/fossil/Test.csv")

head(test)

train <- train %>% arrange(year, month)

head(train)

test <- test %>% arrange(year, month)

head(test)

print(colnames(train))
sellin_channels <- train%>%
  select(starts_with("sellin_channel")) %>%
   colnames()

print(sellin_channels)

df <- train[c("sku_name", "year", "month", "sellin")]
head(df)  
dim(df)

next_month <- function(year, month){
  
  if(month + 1 <= 12){
    return(c(year, month+1))
  }
  
  if(year == 2020){
    year = year + 1
  }
  
  return(c(year, 1))
  
}

filler <- function(frame){
  
  vals <- list()
  year <- 2016
  month  <- 1
  
  for(i in 1:nrow(frame)){
     val <- frame[i, "sellin"]
     yr <- frame[i, "year"]
     mon <- frame[i, "month"]
  }

  while(1){
    
    if(yr == year & mon == month){
      c(year, month) %<-% next_month(year, month)
      break
    }
    
    c(year, month) %<-% next_month(year, month)
    vals <- as.numeric(append(vals, 0))
  }
  
  vals <- as.numeric(append(val))
  
  while (1){
    if (year == 2021 & month > 10){
      break
      vals <- as.numeric(append(vals, 0))
      c(year, month) %<-% next_month(year, month)
    }
  }
  # print(vals)
  return(vals)
}

to_df_main <- function(series){
  ds <- list()
  y <- list()
  year <- 2016
  month <- 1
  for(i in 1:length(series)){
    month <- fapply(month, "%02d")
    ds.append(glue("{year}-{month}"))
    y <- append(y, series[i])
    c(year, month) %<-% next_month(year, month)
  }
  
  return(data.frame(ds, y))
}

to_df <- function(dframe){
  dframe$month <- sapply("%02d", sprintf, dframe$month)
  dframe <- dframe %>% 
    mutate(
      ds = glue("{year}-{month}"),
      y = sellin
    )
  dframe <- dframe[c("ds", "y")]
  
  return(dframe)
}


  
sku_names <- unique(test$sku_name)

for(sku in sku_names){
  data <- df[df$sku_name == sku,]
  data <- data[c("sellin", "month", "year")] 
  
}

data <- to_df(data)



head(data)



















  