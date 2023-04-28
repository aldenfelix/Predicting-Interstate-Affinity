library(tidyverse)
library(openxlsx)
library(reshape2)
library(countrycode)
library(VIM)

# Loading, wrangling, and merging ICEWS coded event data----
# slightly different formats between yearly data
# coercing variables to correct class
# saving as .rds

# load("data/events.2015.20180710092545.RData")
# x15 <- x
# x15$Event.Date <- as.Date(as.character(x15$Event.Date))
# saveRDS(x15,"data/events_2015.rds")
# load("data/events.2016.20180710092843.RData")
# x16 <- x
# x16$Event.Date <- as.Date(as.character(x16$Event.Date))
# saveRDS(x16,"data/events_2016.rds")
# rm(x)
# x17 <- read_delim("data/Events.2017.20201119.tab")
# saveRDS(x17,"data/events_2017.rds")
# x18 <- read_delim("data/events.2018.20200427084805.tab")
# saveRDS(x18,"data/events_2018.rds")
# x19 <- read_delim("data/events.2019.20200427085336.tab")
# saveRDS(x19,"data/events_2019.rds")
# colnames(x15) <- gsub(".", " ", colnames(x15), fixed = TRUE)
# colnames(x16) <- gsub(".", " ", colnames(x15), fixed = TRUE)
# events <- rbind(x15, x16, x17, x18, x19)
# colnames(events) <- gsub(" ", ".", colnames(events), fixed = TRUE)
# events <- events %>% subset(select = c("Event.Date", "Source.Country", "Event.Text", "CAMEO.Code", "Intensity", "Target.Country"))
# events <- events %>% subset(Target.Country == "United States")
# events <- na.omit(events)
# events$Event.Date <- format(events$Event.Date, "%Y")
# saveRDS(events,"data/events.rds")



# Loading, wrangling World Bank variables, Calculating Affinity----

# loading, renaming, and saving as .rds
# vars <- read.xlsx("data/P_Affinity Data.xlsx", sheet = 1, colNames = TRUE)
# vars <- vars[, -5]
# saveRDS(vars, "data/variables.rds")
vars <- readRDS("data/variables.rds")
events <- readRDS("data/events.rds")

# only keeping events from countries that variables are available for
events <- events %>% subset(Source.Country %in% vars$Country.Name)

# removing all events/countries from countries that have less than 10 events for any year
counts <- events %>% group_by(Source.Country, Event.Date) %>% 
  count(Source.Country)
exclude <- counts %>% subset(n <= 10, select = "Source.Country")
events <- events %>% subset(!Source.Country %in% exclude$Source.Country)

# calculating affinity for countries per year
affinity <- events %>% group_by(Source.Country, Event.Date) %>% 
  summarise(affinity = mean(Intensity))
affinity <- full_join(affinity, counts, by = c("Source.Country", "Event.Date"))
affinity <- na.omit(affinity)

# not necessary, just cleaning variables to match kept data
vars <- vars %>% subset(Country.Name %in% affinity$Source.Country)



# Loading and wrangling trade data----

# loading, renaming, saving as .rds
# trade <- read_csv("data/DOT_02-28-2023 04-56-02-14_timeSeries.csv")
# saveRDS(trade, "data/trade_2015-2019.rds")
trade <- readRDS("data/trade_2015-2019.rds")

# keep trade balance variable with U.S. as counterpart country
trade <- trade[, c(-(8:74), -(80:82))]
trade <- trade %>% subset(`Counterpart Country Name` == "United States")
trade <- trade[grep("Trade", trade$`Indicator Name`), ]

# transform existing IMF country code to country name to merge with rest of data
trade$Source.Country <- countrycode(trade$`Country Code`, 
                                    origin = "imf", destination = "country.name")

# cleaning data, transforming to long format
trade <- trade[, c(13, 1:12)]
trade <- na.omit(trade)
trade <- trade[, c(-2, -3, -(5:8))]
trade <- melt(trade, id.vars = 1:2)
names(trade)[3] <- "Time"



# Merging affinity, variables, trade, freedom scores data----

# merging affinity and variables
df <- full_join(affinity, vars, 
                by = c("Source.Country" = "Country.Name", "Event.Date" = "Time"))

df <- full_join(df, trade, by = c("Source.Country", "Event.Date" = "Time"))
colnames(df)[which(names(df) == "value")] <- "Goods.Value.of.Trade.Balance.US.Dollars"
df$Goods.Value.of.Trade.Balance.US.Dollars <- as.numeric(df$Goods.Value.of.Trade.Balance.US.Dollars)
df <- df[, -28]

# removing countries that don't have data for all 5 years
# df <- na.omit(df)
# exclude2 <- count(df, Source.Country) %>% subset(n < 5)
# df <- df %>% subset(!Source.Country %in% exclude2$Source.Country)

# loading, wrangling freedom data
freedom <- read_csv("data/freedom_scores.csv")
freedom <- freedom[, -1]
freedom$`Country  Sort descending` <- gsub("*", "", freedom$`Country  Sort descending`, fixed = TRUE)
# removing categorical description from total score variable, now fully numeric
freedom$`Total Score and Status` <- gsub("[^0-9]", "", freedom$`Total Score and Status`)
freedom$`Total Score and Status` <- as.numeric(freedom$`Total Score and Status`)
colnames(freedom) <- c("Source.Country", "Total.Score", "Political.Rights", "Civil.Liberties")
freedom$Total.Score <- as.numeric(freedom$Total.Score)

# merging freedom scores with rest of data
df <- left_join(df, freedom)
df <- df[, -(4:6)]

#keep the obs that have affinity data
df <- df[1:424,]

#remove the row corresponding to "Andorra"
df <- df[df$Source.Country != "Andorra", ]


## kNN imputation
dfknnImpute1 <- kNN(df, k=5) # takes forever
#summary(dfknnImpute1)
df <- dfknnImpute1[, 1:28]



# Turning affinity into categorical----

df$affinity <- round(df$affinity, 0)

for (i in 1:length(df$affinity)) {
  if (df$affinity[i] < 0){
    df$affinity[i] <- -1
  }
  else if (df$affinity[i] == 0){
    df$affinity[i] <- 0
  }
  else if (df$affinity[i] == 1){
    df$affinity[i] <- 1
  }
  if (df$affinity[i] == 2){
    df$affinity[i] <- 2
  }
  else if (df$affinity[i] >= 3){
    df$affinity[i] <- 3
  }
}

df$affinity <- as.factor(df$affinity)

# Regression Random Forest----

df15 <- df %>% subset(Event.Date == 2015)
df15 <- df15[, -2]
df15 <- as_tibble(df15)
df15 <- column_to_rownames(df15, "Source.Country")
colnames(df15) <- c("affinity", "gdp_growth_annual", "gdp_cap_growth", "health_exp_cap", "health_exp_gdp",
                    "gdp_cap_ppp", "gdp_ppp", "edu_exp", "nat_resc_rent", "women_seats", "women_bus_law_score",
                    "life_exp", "mort", "rd_exp", "hi_tech_export", "internet", "ict_good_exp", "ict_good_imp",
                    "ict_ser_exp", "gini", "ease_bus_score", "milt_exp", "trade_balance", "total_score",
                    "pol_rights", "civil_lib")
# df15$affinity <- round(df15$affinity, 1)
# df15 <- df15[, c(-2, -3, -5, -7, -15, -17, -21)]
# df15 <- df15[, c(-2, -15)]

library(randomForest)
library(caret)

#testing range of mtry values
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(affinity~., data=df15, method="rf", 
                       metric="Rsquared", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

#using best mtry value
set.seed(1)
rf <- randomForest(affinity ~ ., data = df15, ntree = 5000, 
                   importance = TRUE, mtry = 4)
rf
varImpPlot(rf)



# Classification Random Forest----

df15 <- df %>% subset(Event.Date == 2015)
df15 <- df15[, -2]
df15 <- as_tibble(df15)
df15 <- column_to_rownames(df15, "Source.Country")
colnames(df15) <- c("affinity", "gdp_growth_annual", "gdp_cap_growth", "health_exp_cap", "health_exp_gdp",
                    "gdp_cap_ppp", "gdp_ppp", "edu_exp", "nat_resc_rent", "women_seats", "women_bus_law_score",
                    "life_exp", "mort", "rd_exp", "hi_tech_export", "internet", "ict_good_exp", "ict_good_imp",
                    "ict_ser_exp", "gini", "ease_bus_score", "milt_exp", "trade_balance", "total_score",
                    "pol_rights", "civil_lib")
df15 <- df15[, c(-15, -16, -18, -21, -20, -22)]

library(randomForest)
library(caret)

#testing range of mtry values
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(affinity~., data=df15, method="rf", 
                       metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

#using best mtry value
set.seed(1)
rf <- randomForest(affinity ~ ., data = df15, ntree = 5000, 
                   importance = TRUE, mtry = 4)
rf
varImpPlot(rf)


#---------------------------16
df16 <- df %>% subset(Event.Date == 2016)
df16 <- df16[, -2]
df16 <- as_tibble(df16)
df16 <- column_to_rownames(df16, "Source.Country")
colnames(df16) <- c("affinity", "gdp_growth_annual", "gdp_cap_growth", "health_exp_cap", "health_exp_gdp",
                    "gdp_cap_ppp", "gdp_ppp", "edu_exp", "nat_resc_rent", "women_seats", "women_bus_law_score",
                    "life_exp", "mort", "rd_exp", "hi_tech_export", "internet", "ict_good_exp", "ict_good_imp",
                    "ict_ser_exp", "gini", "ease_bus_score", "milt_exp", "trade_balance", "total_score",
                    "pol_rights", "civil_lib")
df16 <- df16[, c(-15, -16, -18, -21, -20, -22)]

#using best mtry value
set.seed(1)
rf16 <- randomForest(affinity ~ ., data = df16, ntree = 5000, 
                   importance = TRUE, mtry = 4)
rf16
#---------------------------17
df17 <- df %>% subset(Event.Date == 2017)
df17 <- df17[, -2]
df17 <- as_tibble(df17)
df17 <- column_to_rownames(df17, "Source.Country")
colnames(df17) <- c("affinity", "gdp_growth_annual", "gdp_cap_growth", "health_exp_cap", "health_exp_gdp",
                    "gdp_cap_ppp", "gdp_ppp", "edu_exp", "nat_resc_rent", "women_seats", "women_bus_law_score",
                    "life_exp", "mort", "rd_exp", "hi_tech_export", "internet", "ict_good_exp", "ict_good_imp",
                    "ict_ser_exp", "gini", "ease_bus_score", "milt_exp", "trade_balance", "total_score",
                    "pol_rights", "civil_lib")
df17 <- df17[, c(-15, -16, -18, -21, -20, -22)]

#using best mtry value
set.seed(1)
rf17 <- randomForest(affinity ~ ., data = df17, ntree = 5000, 
                     importance = TRUE, mtry = 4)
rf17
#---------------------------18
df18 <- df %>% subset(Event.Date == 2018)
df18 <- df18[, -2]
df18 <- as_tibble(df18)
df18 <- column_to_rownames(df18, "Source.Country")
colnames(df18) <- c("affinity", "gdp_growth_annual", "gdp_cap_growth", "health_exp_cap", "health_exp_gdp",
                    "gdp_cap_ppp", "gdp_ppp", "edu_exp", "nat_resc_rent", "women_seats", "women_bus_law_score",
                    "life_exp", "mort", "rd_exp", "hi_tech_export", "internet", "ict_good_exp", "ict_good_imp",
                    "ict_ser_exp", "gini", "ease_bus_score", "milt_exp", "trade_balance", "total_score",
                    "pol_rights", "civil_lib")
df18 <- df18[, c(-15, -16, -18, -21, -20, -22)]

#using best mtry value
set.seed(1)
rf18 <- randomForest(affinity ~ ., data = df18, ntree = 5000, 
                     importance = TRUE, mtry = 4)
rf18
#---------------------------19
df19 <- df %>% subset(Event.Date == 2019)
df19 <- df19[, -2]
df19 <- as_tibble(df19)
df19 <- column_to_rownames(df19, "Source.Country")
colnames(df19) <- c("affinity", "gdp_growth_annual", "gdp_cap_growth", "health_exp_cap", "health_exp_gdp",
                    "gdp_cap_ppp", "gdp_ppp", "edu_exp", "nat_resc_rent", "women_seats", "women_bus_law_score",
                    "life_exp", "mort", "rd_exp", "hi_tech_export", "internet", "ict_good_exp", "ict_good_imp",
                    "ict_ser_exp", "gini", "ease_bus_score", "milt_exp", "trade_balance", "total_score",
                    "pol_rights", "civil_lib")
df19 <- df19[, c(-15, -16, -18, -21, -20, -22)]

#using best mtry value
set.seed(1)
rf19 <- randomForest(affinity ~ ., data = df19, ntree = 5000, 
                     importance = TRUE, mtry = 4)
rf19




### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x= reorder(Var.Names, -MeanDecreaseAccuracy), y=`MeanDecreaseAccuracy`)) +
  geom_segment( aes(x= reorder(Var.Names, -MeanDecreaseAccuracy), xend=Var.Names, y=-5, yend=`MeanDecreaseAccuracy`), color="skyblue") +
  geom_point(aes(size = MeanDecreaseGini), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dotted", size = 1, color = "red") +
  annotate("text", x = 10, y = -.4, label = "0 Mean Decrease Accuracy", 
           angle = 90, size = 5, color = "red") +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
