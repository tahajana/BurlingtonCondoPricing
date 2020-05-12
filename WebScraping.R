rm(list = ls())
library(httr)
#install.packages("jsonlite")
library(jsonlite)
library(rvest)
library(plyr)



############################
# ALDERSHOT SOUTH #183
############################
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=aldershot-south-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
df = data.frame(NULL)
for (i in 1:8){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  #df3 = data.frame(NULL)
  #get_prices_df = cbind(get_prices_df,df3)
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Aldershot South",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'aldershot-south.csv')

############################
# ALDERSHOT WEST #1
############################
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=aldershot-west-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
df = data.frame(NULL)
for (i in 1:1){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Aldershot West",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'aldershot-west.csv')

############################
# ALTON NORTH #93
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=alton-north-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:4){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$data.attributes.open.houses =  as.character(df$data.attributes.open.houses)
df$Neighbourhood = rep("Alton North",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'alton-north.csv')


############################
# ALDERSHOT CENTRAL #62
#########################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=aldershot-central-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:3){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Aldershot Central",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'aldershot-central.csv')

############################
# Brant Hills #104
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=brant-hills-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:5){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Brant Hills",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'brant-hills.csv')

############################
# Central #157
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=central-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:7){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Central",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'central.csv')

############################
# CORPORATE #264
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=corporate-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:11){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Corporate",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'corporate.csv')


############################
# DYNES #113
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=dynes-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:5){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Dynes",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'dynes.csv')

############################
# ELIZABETH GARDEN #178
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=elizabeth-gardens-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:8){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Elizabeth Garden",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'elizabeth-garden.csv')


############################
# HEADON FOREST #222
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=headon-forest-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:10){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$data.attributes.open.houses =  as.character(df$data.attributes.open.houses)
df$Neighbourhood = rep("Headon Forest",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'headon-forest.csv')

############################
# LONGMOOR #31
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=longmoor-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:2){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Longmoor",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'longmoor.csv')

############################
# MAPLE #307
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=maple-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:13){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Maple",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'maple.csv')

############################
# MILLCROFT #190
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=millcroft-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:8){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Millcroft",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'millcroft.csv')

############################
# MOUNTAINSIDE #137
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=mountainside-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:6){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Mountainside",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'mountainside.csv')




############################
# ORCHARD #210
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=orchard-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:24){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Orchard",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'orchard.csv')

############################
# PALMER #121
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=palmer-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:6){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Palmer",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'palmer.csv')

############################
# PINEDALE #132
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=pinedale-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:18){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Pinedale",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'pinedale.csv')

############################
# PLAINS
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=plains-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:3){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Plains",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'plains.csv')

############################
# ROSELAND
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=roseland-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:1){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Roseland",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'roseland.csv')



############################
# TANSLEY
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=tansley-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:9){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Tansley",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'tansley.csv')

############################
# TYANDAGA
############################
df <- data.frame(NULL)
base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=tyandaga-burlington-on&page%5Bnumber%5D="
base2 = "&page%5Bsize%5D=24&sort=-date"
for (i in 1:3){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
df$Neighbourhood = rep("Tyandaga",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
write.csv(df,'tyandaga.csv')

base1 = "https://www.zoocasa.com/services/api/v3/listings?filter%5Brental%5D=false&filter%5Bstatus%5D=not-available-sold&filter%5Blatitude%5D=43.653226&filter%5Blongitude%5D=-79.3831843&filter%5Bzoom%5D=14&filter%5Bhome-type%5D%5Bhouse-detached%5D=false&filter%5Bhome-type%5D%5Bhouse-semidetached%5D=false&filter%5Bhome-type%5D%5Bhouse-attached%5D=false&filter%5Bhome-type%5D%5Btownhouse%5D=false&filter%5Bhome-type%5D%5Bcondo%5D=true&filter%5Bprice-min%5D=&filter%5Bprice-max%5D=&filter%5Bbedrooms%5D=0%2B&filter%5Bsqft-min%5D=&filter%5Bsqft-max%5D=&filter%5Blisted-since%5D=&filter%5Bbathrooms%5D=1%2B&filter%5Bparking-spaces%5D=0%2B&filter%5Bopen-house%5D=false&filter%5Bgarage%5D=false&filter%5Bpool%5D=false&filter%5Bfireplace%5D=false&filter%5Bwaterfront%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bsingle-family%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bbasement-apartment%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bduplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Btriplex%5D=false&filter%5Badditional%5D%5Bhouse%5D%5Bfourplex%2B%5D=false&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Blocker%5D=any&filter%5Badditional%5D%5Bcondo-or-townhouse%5D%5Bmaintenance-fee%5D=&filter%5Bslug%5D=mississauga-on&page%5Bnumber%5D="
base2 ="&page%5Bsize%5D=24&sort=-date"
df <- data.frame(NULL)
for (i in 1:68){
  base = paste(base1,i,base2,sep ="")
  get_houses <-GET(base)
  get_houses_text <- content(get_houses,"text")
  get_prices_json <- fromJSON(get_houses_text, flatten = TRUE)
  get_prices_df <- as.data.frame(get_prices_json[1])
  df = rbind(df, get_prices_df)
}
df = df[,-c(31,32)]
#df$Neighbourhood = rep("Tyandaga",dim(df)[1])
lst <- which(is.na(df$data.attributes.street.name))
df = df[-lst,]
df3 = data.frame(NULL)
for (j in df[1201:1626,]$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  links = as.character(links)
  start = "<span>details</span>"
  finish = "<span>Room Layout</span>" 
  s <- (which(links==start) + 1)
  f <- ( which(links==finish) - 1)
  pattern2 =  "<span>([^<]*)</span>"
  k=grep(pattern2,links[s:f],value = TRUE )
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern2,k)
  matches = mapply(getexpr,k,gg)
  result1 = gsub(pattern2,'\\1',matches)
  result1 = as.character(result1)
  rem <- which((result1 == "utilities" )| (result1 == "building")| (result1 == "include in maintenance fee") | (result1 == "building amenities") |(result1 == "unit"))
  result1 = result1[-rem]
  split1 = split(result1, 1:2)
  df2 = as.data.frame(matrix(split1$`2`,ncol=(length(split1$`2`)),byrow=TRUE)) 
  names(df2) = split1$`1`
  df3 = rbind.fill(df3,df2)
}
df = cbind(df,df3)
df3 = df3[,-(36:52)]
df3 = df3[,-(20:25)]
for (i in 1:dim(df3)[2]) {
  print(paste(names(df3)[i],sum(is.na(df3[,i])),sep = " : "))
}
df = cbind(df,df3)
write.csv(df,'771DataSet.csv')

df4 = data.frame(Neighbourhood =NULL)
for (j in df[1001:1626,]$data.attributes.path) {
  link <- paste("https://www.zoocasa.com",j,sep = "")
  page <- read_html(link)
  links <- page %>% html_nodes("span")
  page = as.character(links)
  start = grep('<meta itemprop="position" content="3">',page)
  pattern1 = '<span itemprop="name">([^<]*)</span>'
  neigh =grep(pattern1,page[start + 3], value = TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(pattern1,neigh)
  matches = mapply(getexpr,neigh,gg)
  neighbour = gsub(pattern1,'\\1',matches)
  neighbour = as.character(neighbour)
  df4 = rbind(df4, data.frame(Neighbourhood = neighbour))
}

df = cbind(df,df4)
df = df[,-1]

