library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(gridExtra)
library(leaps)
library(randomForest)
library(glmnet)
rm(list = ls())
setwd("Data Files")

# Combining the datasets into one dataset
temp = list.files(pattern="*.csv")
mydata = lapply(temp, read.csv,header = TRUE
                ,na.strings=c("","NA"))
df <- do.call(rbind.fill , mydata) 




# Removing the unimportant variables, and renaming the columns
df = df[,-c(1,2,3,4,5,7,12,13,14,17,18,20,22,23,24,26,29,30,31)]
df = df[,-c(31:44)]


names2 <-colnames(df[13:51]) 
colnames(df)<- c("Listed.Price","Bedrooms","Bathrooms","Extra.Bedrooms","Extra.Bathrooms","Street#","StreetName","Postal.Code"
                              ,"Type1","Date.Added","Price.Sold","Date.Sold",names2)


# MLS is sunique, So if we see two rows with the same MLS, that means its been considered twice
df = df[-which(duplicated(df$MLS..Number)),]

df$Date.Added <- as.Date(df$Date.Added)
df$Date.Sold <- as.Date(df$Date.Sold)
df$Days.In.Market <- difftime(df$Date.Sold,df$Date.Added, units = "days" )
lst10 <- which(df$Days.In.Market < 0)
df<- df[-lst10,] # Remove all data that have Days in Market less than 0,because that does not make sense
# Removing NULL Columns
df = select(df, -Days.In.Market)


df = df[,-(33:51)]
df = df[,-c(23,19)]


##########################
# Exploring Response Variable
###########################
#df = df[-which(df$Price.Sold > 1000000),]
(gy = ggplot(data=df[!is.na(df$Price.Sold),], aes(x=Price.Sold)) +
   geom_histogram(fill="steelblue", binwidth = 10000) +
   scale_x_continuous(breaks= seq(0, 3000000, by=500000), labels = scales::comma)+
   xlab("Price Sold (in CAD)"))

df = df[,-1] #remove listed price


# Checking for Null values
NAcol <- which(colSums(is.na(df)) > 0)
sort(colSums(sapply(df[NAcol], is.na)), decreasing = TRUE)

# Dealing With NA Values 
df$Extra.Bedrooms[which(is.na(df$Extra.Bedrooms))] = 0
df$Total.Bedrooms = df$Extra.Bedrooms + df$Bedrooms
#summary(factor(df$Bedrooms))
#summary(factor(df$Bathrooms))
#summary(factor(df$Total.Bedrooms))
df = df[,-c(3,4)]

###################################################
df$Bedrooms[which(df$Bedrooms >= 4 )] = "4+"
df$Total.Bedrooms[which(df$Total.Bedrooms >= 4 )] = "4+"
df$Bathrooms[which(df$Bathrooms >= 4)] = "4+"
df$Bedrooms[which(df$Bedrooms <= 1)] = "1 or less"
df$Total.Bedrooms[which(df$Total.Bedrooms <= 1 )] = "1 or less"
df$Bedrooms = as.factor(df$Bedrooms)
df$Total.Bedrooms = as.factor(df$Total.Bedrooms)
df$Bathrooms = as.factor(df$Bathrooms)
##############################################################
(gBed <- ggplot(df, aes(x = Bedrooms, y=Price.Sold)) 
  + geom_boxplot(color = "steelblue") 
  + labs(x='Number of Bedrooms', y="Price Sold (in CAD)")
  + scale_x_discrete(breaks = c("1 or less", 2, 3,"4+"))
  + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma)
  +geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)
  + geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred")
)

(gTBed <- ggplot(df, aes(x = Total.Bedrooms, y=Price.Sold)) 
  + geom_boxplot(color = "steelblue") 
  + labs(x='Number of Total Bedrooms', y="Price Sold (in CAD)")
  + scale_x_discrete(breaks = c("1 or less", 2, 3,"4+"))
  + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma)
  +geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)
  + geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred")
)


(gBath <- ggplot(df, aes(x = Bathrooms, y=Price.Sold)) 
  + geom_boxplot(color = "steelblue") 
  + labs(x='Number of Bathrooms', y="Price Sold (in CAD)")
  + scale_x_discrete(breaks = c(1, 2, 3,"4+"))
  + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma)
  +geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)
  + geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred")
)
df[!is.na(df$Price.Sold),] %>% group_by(Bedrooms) %>% summarise(mean = mean(Price.Sold), counts=n())

grid.arrange(gBed, gTBed, gBath, ncol=2)

Beds = c("1 or less" = 1, "4+" = 4)
df$Total.Bedrooms<-as.integer(revalue(df$Total.Bedrooms, Beds))
Bath = c("4+" = 4)
df$Bathrooms<-as.integer(revalue(df$Bathrooms, Bath))


summary(df$Approx..Age)
df$Approx..Age[which(df$Approx..Age == "New years")] = "0-5 years"
df$Approx..Age[which((df$Approx..Age == "11-15 years")| (df$Approx..Age == "6-10 years"))] = "6-15 years"
df$Approx..Age <- droplevels(df$Approx..Age)

df$FullAddress = paste(df$`Street#`,df$StreetName, sep = " ")
lst = levels(factor(df$FullAddress[which(is.na(df$Approx..Age))]))
for (k in lst) {
  st = df$Approx..Age[which((!is.na(df$Approx..Age)) & (df$FullAddress == k))]
  df$Approx..Age[df$FullAddress == k] = as.character(st)[1]
  }
summary(df$Approx..Age)
df = df[-which(is.na(df$Approx..Age)),]

df$Approx..Age = reorder(df$Approx..Age, df$Price.Sold, FUN=median)
(gAge <- ggplot(df, aes(x = Approx..Age, y=Price.Sold)) 
  + geom_boxplot(color = "steelblue") 
  + labs(x='Condo Approx. age', y="Price Sold (in CAD)")
  + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma)
  +geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)
  + geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred")
)
df[!is.na(df$Price.Sold),] %>% group_by(Approx..Age) %>% summarise(mean = mean(Price.Sold), counts=n())
Age = c("0-5 years" = 1,"6-15 years" = 2, "16-30 years" = 3, "31-50 years" = 4
         ,"51-99 years" = 5)
df$Approx..Age<-as.integer(revalue(df$Approx..Age, Age))

vec = df[!is.na(df$Laundry.Level) & df$Ensuite.Laundry == "No", c('Ensuite.Laundry', 'Laundry.Level')]
dim(vec)[1]
head(vec, n = 10)
df = select(df, -Ensuite.Laundry)
df = select(df, -Laundry.Level)


summary(factor(df$Stories)) #This does not amke sense as it sometimes consideres the stories
# of the Whole apartemts, and sometimes the unit it self
# I decide not to use it, because we already have levels which is similar
df = select(df, -Stories)

# levels
df$Levels = reorder(df$Levels, df$Price.Sold, FUN=median)
df[!is.na(df$Price.Sold),] %>% group_by(Levels) %>% summarise(median = median(Price.Sold), counts=n())
df$Levels = as.character(df$Levels)
df = df[-which((df$Levels == "Multi-Level") | (df$Levels == "Other")),]
df$Levels[which((df$Levels == "Bachelor/Studio") | (df$Levels == "Loft") | (df$Levels == "Apartment") )] = "1-Storey"
df$Levels[which(df$Levels == "Stacked Townhse" )] = "2-Storey"
df$Levels[which((df$Levels == "Bungaloft") | (df$Levels == "Bungalow") )] = "1 1/2 - Storey"
df$Levels = as.factor(df$Levels)
df$Levels = reorder(df$Levels, df$Price.Sold, FUN=median)
(gLevels <- ggplot(df, aes(x = Levels, y=Price.Sold)) 
  + geom_boxplot(color = "steelblue") 
  + labs(x='Levels of Condo', y="Price Sold (in CAD)")
  + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma)
  +geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)
  + geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred")
)
Level = c("1-Storey" = 1,"1 1/2 - Storey" = 2, "2-Storey" = 3, "3-Storey" = 4)
df$Levels<-as.integer(revalue(df$Levels, Level))
# Removing the 8 houses with no information about maintenance fees
df = df[-which(is.na(df$Maintenance.Fees)),]
df$Maintenance.Fees = as.numeric(str_remove_all(df$Maintenance.Fees, "[$,]"))
(gMaint <- ggplot(data=df, aes(x=Maintenance.Fees, y=Price.Sold))
  + geom_point(col='steelblue') + geom_smooth(method = "lm", se=FALSE, color="darkred", aes(group=1)) 
  + labs(x="Maintenance Fees (in CAD)", y="Price Sold (in CAD)")
  + scale_y_continuous(breaks= seq(0, 2250000, by=250000), labels = scales::comma)
  + scale_x_continuous(labels = scales::comma)
)
df$Postal.Code = str_sub(df$Postal.Code,1,3) #Consider only the first 3 characters, 
summary(factor(df$Postal.Code))



summary(df$Neighbourhood)


df = df[-which((df$Neighbourhood == "Aldershot West" ) | (df$Neighbourhood == "Roseland" ) ),]
df$Neighbourhood = reorder(df$Neighbourhood, df$Price.Sold, FUN=median)
(nb1 <- ggplot(df, aes(x= Neighbourhood, y=Price.Sold)) 
    + geom_bar(stat='summary', fun.y = "median", fill="steelblue") 
    + labs(x='Neighbourhood', y="Median Price Sold ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)
df$Neighbourhood = reorder(df$Neighbourhood, df$Price.Sold, FUN=mean)
(nb2 <- ggplot(df, aes(x= Neighbourhood, y=Price.Sold)) +
    geom_bar(stat='summary', fun.y = "mean", fill="steelblue") + labs(x='Neighbourhood', y="Mean Price Sold ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=mean(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)
grid.arrange(nb1, nb2)

df = select(df, -Postal.Code)
# we do this as a Neighbourhood#2 and we see which one actully gives better result




########################
# Visualizing the Other Variables
#########################

df$month = rep(0,dim(df)[1])
df$month = paste(str_sub(df$Date.Sold,1,4),str_sub(df$Date.Sold,6,7),sep = "-")
lst50 = levels(factor(df$month))
df$median = rep(0,dim(df)[1])
for (i in lst50) {
  #y = strep_data$Exp[(strep_data$group == i)]
  df$median[(df$month == i)] = median(df$Price.Sold[(df$month == i)])
}


k = 1
df$monthNum = rep(0,dim(df)[1])
for (j in lst50) {
  #y = strep_data$Exp[(strep_data$group == i)]
  df$monthNum[(df$month == j)] = k
  k = k + 1
}

(gPrices <- ggplot(data = df, mapping = aes(x=month,y=Price.Sold)) 
  +geom_jitter(aes(color="#F3D9B1"),alpha=0.4, width = 0.2) 
  +geom_point(aes(y = median, color = "#C29979")) 
  +geom_smooth(aes(x = monthNum, y = median,color = "#6494AA"),span=0.5) 
  +guides(color=FALSE)
  + scale_y_continuous(trans='log10', labels = scales::comma)
  + scale_x_discrete(breaks = c("2017-01", "2018-01","2019-01","2020-01"))
  +scale_color_manual(values = c("#6494AA","#C29979","#F3D9B1"))
  +ylab("Price Sold (in CAD)") 
  +xlab("Date Sold")
)

summary(df$Size)
df$Size = reorder(df$Size, df$Price.Sold, FUN=median)
df[!is.na(df$Price.Sold),] %>% group_by(Size) %>% summarise(median = median(Price.Sold), counts=n())
# We plot a boxplot to show the range of prices for the different size categoties
df$Size = as.character(df$Size)
lst7 = which((df$Size == "0-499 sq. ft.") | (df$Size == "500–599 sq. ft."))
df$Size[lst7] = "0–599 sq. ft."

lst8 = which((df$Size == "2500–2749 sq. ft.") | (df$Size == "2750–2999 sq. ft.")
             | (df$Size == "3000–3249 sq. ft.") | (df$Size == "3250–3499 sq. ft.")
             | (df$Size == "4000–4249 sq. ft.") | (df$Size == "2000–2249 sq. ft.")
             | (df$Size == "2250–2499 sq. ft."))
df$Size[lst8] = "2000+ sq. ft."

df$Size = factor(df$Size)
df$Size = reorder(df$Size, df$Price.Sold, FUN=median)
(gSize <- ggplot(df, aes(x= Size, y=Price.Sold))
    +  geom_boxplot(col = "steelblue")
    + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma)+
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)

#we are going to create an ordinal variable
Size = c("0–599 sq. ft." = 1,"600–699 sq. ft." = 2, "700–799 sq. ft." = 3, "800–899 sq. ft." = 4
         ,"900–999 sq. ft." = 5, "1000–1199 sq. ft." = 6, "1200–1399 sq. ft." = 7, "1400–1599 sq. ft." = 8,
         "1600–1799 sq. ft." = 9, "1800–1999 sq. ft." = 10, "2000+ sq. ft." = 11)
df$Size<-as.integer(revalue(df$Size, Size))

# ###########################################################################
summary(df$Type)
 df = df[-(which((df$Type == "Co-Op Apt")| (df$Type == "Co-Ownership Apt") | (df$Type == "Comm Element Condo"))),]
 df$Type = as.character(df$Type)
 df$Type[which(df$Type != "Condo Apt")] = "Townhouse/Semi-Det/Det Condo"
 df$Type = as.factor(df$Type)
 df$Type = reorder(df$Type, df$Price.Sold, FUN=median)
 (gType <- ggplot(df, aes(x= Type, y=Price.Sold)) +
     geom_boxplot(col="steelblue") + 
     labs(x='Type of Condo', y="Price Sold ($)")  
   + scale_y_continuous(breaks= seq(0, 2500000, by=500000), labels = scales::comma) +
     geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
     geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)
#############################################


df$Locker = reorder(df$Locker, df$Price.Sold, FUN=median)
df[!is.na(df$Price.Sold),] %>% group_by(Locker) %>% summarise(median = median(Price.Sold), counts=n())
df$Locker = as.character(df$Locker)
df$Locker[which((df$Locker == "Ensuite+Owned")| (df$Locker == "Ensuite+Common") | (df$Locker == "Ensuite+Exclusive"))] = "Ensuite+"
df$Locker = as.factor(df$Locker)
df$Locker = reorder(df$Locker, df$Price.Sold, FUN=mean)
(gLocker <- ggplot(df, aes(x= Locker, y=Price.Sold)) +
    geom_boxplot(color = "steelblue") + 
    labs(x='Locker', y="Price Sold ($)")  +
    scale_y_continuous(breaks= seq(0, 3000000, by=500000),labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)
df = df[-which(df$Locker == "Ensuite"),]
#Exposure
df$Exposure = reorder(df$Exposure, df$Price.Sold, FUN=mean)
(gExposure <- ggplot(df, aes(x= Exposure, y=Price.Sold)) +
    geom_boxplot(color = "steelblue") + 
    labs(x='Exposure', y="Price Sold ($)") +
    scale_y_continuous(breaks= seq(0, 3000000, by=500000),labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)


df[!is.na(df$Price.Sold),] %>% group_by(Garage) %>% summarise(median = median(Price.Sold), counts=n())
df$Garage[which((df$Garage == "Carport") | (df$Garage == "Surface") | (df$Garage == "Detached"))] = "Other"
df$Garage = reorder(df$Garage, df$Price.Sold, FUN=mean)
(gGarage <- ggplot(df, aes(x= Garage, y=Price.Sold)) +
    geom_boxplot(color = "steelblue") + 
    labs(x='Type of Garage', y="Price Sold ($)")  +
    scale_y_continuous(breaks= seq(0, 3000000, by=500000),labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)

#Balcony
df$Balcony = reorder(df$Balcony, df$Price.Sold, FUN=mean)
df[!is.na(df$Price.Sold),] %>% group_by(Balcony) %>% summarise(median = median(Price.Sold), counts=n())
df = df[-which(df$Balcony == "Jlte"),]
(gBalcony <- ggplot(df, aes(x= Balcony, y=Price.Sold)) +
    geom_boxplot(color = "steelblue") + 
    labs(x='Type of Balcony', y="Price Sold ($)")  +
    scale_y_continuous(breaks= seq(0, 3000000, by=500000),labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)

#Exterior
df$Exterior = reorder(df$Exterior, df$Price.Sold, FUN=mean)
summary(factor(df$Exterior))
(gExt <- ggplot(df, aes(x= Exterior, y=Price.Sold)) +
    geom_bar(stat='summary', fun.y = "median", fill="steelblue") + labs(x='Neighbourhood', y="Mean Price Sold ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)

df$Exterior = as.character(df$Exterior)
df$Exterior[which((df$Exterior == "Board/Batten and Brick") | (df$Exterior == "Brick Front and Stucco/Plaster") | (df$Exterior == "Brick Front and Metal/Side") |
                    (df$Exterior == "Brick Front and Wood") | (df$Exterior == "Brick and Wood") | (df$Exterior == "Brick and Metal/Side") | (df$Exterior == "Brick and Shingle") |
                                        (df$Exterior == "Brick Front"))] = "Brick and Other" 

df$Exterior[which((df$Exterior == "Brick Front and Concrete"))] = "Brick and Concrete"
df$Exterior[which((df$Exterior == "Brick Front and Vinyl Siding"))] = "Brick and Vinyl Siding"
df$Exterior[which((df$Exterior == "Brick and Brick Front"))] = "Brick"
df$Exterior[which((df$Exterior == "Brick Front and Stone"))] = "Brick and Stone"
df$Exterior[which((df$Exterior == "Alum Siding and Brick Front"))] = "Alum Siding and Brick"
df$Exterior[which((df$Exterior == "Concrete and Stone") | (df$Exterior =="Stone and Stucco/Plaster") | 
                    (df$Exterior == "Other and Stone") |  (df$Exterior == "Stone") | (df$Exterior == "Alum Siding and Stone")
                  | (df$Exterior == "Stone and Vinyl Siding")  | (df$Exterior == "Metal/Side and Stone"))] = "Stone or (Stone & Other)"
df$Exterior[which((df$Exterior == "Concrete and Stucco/Plaster") | (df$Exterior =="Stucco/Plaster") |
                    (df$Exterior == "Other and Stucco/Plaster") | (df$Exterior == "Stucco/Plaster and Vinyl Siding") |
                    (df$Exterior == "Metal/Side and Stucco/Plaster"))] = "Stucco/Plaster or (Stucco/Plaster & Other)"

df$Exterior[which((df$Exterior != "Stucco/Plaster or (Stucco/Plaster & Other)") & (df$Exterior !="Stone or (Stone & Other)") &
                    (df$Exterior != "Alum Siding and Brick") & (df$Exterior != "Brick and Stone") &
                    (df$Exterior != "Brick") & (df$Exterior != "Brick and Vinyl Siding") & (df$Exterior !="Brick and Concrete")  & (df$Exterior !="Brick and Other")
                  & (df$Exterior !="Brick and Stucco/Plaster")  & (df$Exterior !="Concrete"))] = "Other"

df$Exterior = as.factor(df$Exterior)
df$Exterior = reorder(df$Exterior, df$Price.Sold, FUN=median)
(gExt <- ggplot(df, aes(x= Exterior, y=Price.Sold)) +
    geom_bar(stat='summary', fun.y = "median", fill="steelblue") + labs(x='Condo Exterior', y="Mean Price Sold ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=median(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)

df$Exterior = reorder(df$Exterior, df$Price.Sold, FUN=mean)
(gExt <- ggplot(df, aes(x= Exterior, y=Price.Sold)) +
    geom_bar(stat='summary', fun.y = "mean", fill="steelblue") + labs(x='Condo Exterior', y="Mean Price Sold ($)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::comma) +
    geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
    geom_hline(yintercept=mean(df$Price.Sold), linetype="dashed", color = "darkred") #dashed line is median SalePrice
)

names(df)
summary(df$Pets)
df[!is.na(df$Price.Sold),] %>% group_by(Pets) %>% summarise(median = median(Price.Sold), counts=n())
df = select(df, -Pets)

summary(df$A.C)
df$A.C[which((df$A.C == "Wall Unit") | (df$A.C == "Window Unit"))] = "Other"
df[!is.na(df$Price.Sold),] %>% group_by(A.C) %>% summarise(median = median(Price.Sold), counts=n())
summary(df$Exterior)
summary(df$Heating.Fuel)
df$Heating.Fuel[ which(df$Heating.Fuel != "Gas") ] = "Other"
df$Heating.Fuel = reorder(df$Heating.Fuel, df$Price.Sold, FUN=median)
df[!is.na(df$Price.Sold),] %>% group_by(Heating.Fuel) %>% summarise(median = median(Price.Sold), counts=n())


write.csv(df,'BurlingtonHousingData.csv')
#########################################################
# MODELING
#########################################################

df2 = read.csv("BurlingtonHousingData.csv")
df = df2[,c(3,10,12,13,14,15,16,17,18,19,20,21,22,23,24,8)]

set.seed(705)
train_index <- sample(1:nrow(df), 0.7 * nrow(df))
test_index <- setdiff(1:nrow(df), train_index)

Train <- df[train_index, ]
Test <- df[test_index, ]

for (i in names(Train)) {
  print(summary(Train[,i]))
  print(summary(Test[,i]))
}

regfit.fwd=regsubsets(Price.Sold~.,data=df ,nvmax =15,
                         )
mod1 <- lm(Price.Sold ~., Train)
summary(mod1)
yhat = predict(mod1,newdata=Test)
y.test = Test[,16]
sqrt(mean((yhat-y.test)^2))
res = data.frame(yhat, y.test)
head(res, n= 10)


set.seed(720)
M<-(1:20)*50
error<-M
for(m in M){
  bag.hitters=randomForest(Price.Sold~.,data=df,subset=train_index,mtry=15,importance=TRUE,ntree=m)
  yhat.bag = predict(bag.hitters,newdata=df[-train_index,])
  error[m/50]<-mean((yhat.bag-y.test)^2)
}
bag<-error
for(m in M){
  bag.hitters=randomForest(Price.Sold~.,data=df,subset=train_index,mtry=2,importance=TRUE,ntree=m)
  yhat.bag = predict(bag.hitters,newdata=df[-train_index,])
  error[m/50]<-mean((yhat.bag-y.test)^2)
}
rf2<-error
for(m in M){
  bag.hitters=randomForest(Price.Sold~.,data=df,subset=train_index,mtry=3,importance=TRUE,ntree=m)
  yhat.bag = predict(bag.hitters,newdata=df[-train_index,])
  error[m/50]<-mean((yhat.bag-y.test)^2)
}
rf3<-error
for(m in M){
  bag.hitters=randomForest(Price.Sold~.,data=df,subset=train_index,mtry=4,importance=TRUE,ntree=m)
  yhat.bag = predict(bag.hitters,newdata=df[-train_index,])
  error[m/50]<-mean((yhat.bag-y.test)^2)
}
rf4<-error
plot(M,bag,typ="l",col=2,xlab= "number of trees", xlim=c(0,1000),ylab="MSE",ylim=c(1500000000,550000000))
lines(M,rf2,typ="l",col=1)
lines(M,rf3,typ="l",col=4)
lines(M,rf4,typ="l",col=5)
legend(x=1,y=3500000000,c("m=15 (bagging)","m=2","m=3","m=4"),col=c(2,1,4,5),pch=c(2,3,4,5))



x=model.matrix(Price.Sold~.,df)[,-1]
y=df$Price.Sold
grid =10^seq(10,-2, length =100)
ridge.mod =glmnet (x,y,alpha =0, lambda =grid)
cv.out =cv.glmnet(x[train_index ,],y[train_index],alpha =0)
bestlam =cv.out$lambda.min
bestlam
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test_index ,])
mean((ridge.pred -y.test)^2)

lasso.mod =glmnet (x[train_index ,],y[train_index],alpha =1, lambda =grid)
set.seed(720)
cv.out =cv.glmnet(x[train_index,],y[train_index],alpha =1)
bestlam =cv.out$lambda.min
lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test_index ,])
mean((lasso.pred -y.test)^2)

# remove all the data that are more than 1 million dollars,and house sold in 2017

df2 = df2[-which((df2$month == "2017-06") | (df2$month == "2017-07")),]
df2 = df2[-which(df2$Price.Sold > 1000000),]



