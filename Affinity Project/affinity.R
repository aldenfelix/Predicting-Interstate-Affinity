library(tidyverse)
library(openxlsx)
library(reshape2)
library(countrycode)

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

# vars <- read.xlsx("data/P_Affinity Data.xlsx", sheet = 1, colNames = TRUE)
# vars <- vars[, -5]
# saveRDS(vars, "data/variables.rds")
vars <- readRDS("data/variables.rds")
events <- readRDS("data/events.rds")

events <- events %>% subset(Source.Country %in% vars$Country.Name)
counts <- events %>% group_by(Source.Country, Event.Date) %>% 
  count(Source.Country)
exclude <- counts %>% subset(n <= 10, select = "Source.Country")
events <- events %>% subset(!Source.Country %in% exclude$Source.Country)

affinity <- events %>% group_by(Source.Country, Event.Date) %>% 
  summarise(affinity = mean(Intensity))
affinity <- full_join(affinity, counts, by = c("Source.Country", "Event.Date"))
affinity <- na.omit(affinity)

vars <- vars %>% subset(Country.Name %in% affinity$Source.Country)


# trade <- read_csv("data/DOT_02-28-2023 04-56-02-14_timeSeries.csv")
# saveRDS(trade, "data/trade_2015-2019.rds")
trade <- readRDS("data/trade_2015-2019.rds")
trade <- trade[, c(-(8:74), -(80:82))]
trade <- trade %>% subset(`Counterpart Country Name` == "United States")
trade <- trade[grep("Trade", trade$`Indicator Name`), ]
trade$Source.Country <- countrycode(trade$`Country Code`, 
                                    origin = "imf", destination = "country.name")
trade <- trade[, c(13, 1:12)]
trade <- na.omit(trade)
trade <- trade[, c(-2, -3, -(5:8))]
trade <- melt(trade, id.vars = 1:2)
names(trade)[3] <- "Time"

df <- full_join(affinity, vars, 
                by = c("Source.Country" = "Country.Name", "Event.Date" = "Time"))

df <- full_join(df, trade, by = c("Source.Country", "Event.Date" = "Time"))
colnames(df)[which(names(df) == "value")] <- "Goods.Value.of.Trade.Balance.US.Dollars"
df <- df[, -28]

df <- na.omit(df)
exclude2 <- count(df, Source.Country) %>% subset(n < 5)
df <- df %>% subset(!Source.Country %in% exclude2$Source.Country)

freedom <- read_csv("data/freedom_scores.csv")
freedom <- freedom[, -1]
freedom$`Country  Sort descending` <- gsub("*", "", freedom$`Country  Sort descending`, fixed = TRUE)
freedom$`Total Score and Status` <- gsub("[^0-9]", "", freedom$`Total Score and Status`)
freedom$`Total Score and Status` <- as.numeric(freedom$`Total Score and Status`)
colnames(freedom) <- c("Source.Country", "Total.Score", "Political.Rights", "Civil.Liberties")

df <- left_join(df, freedom)

# impute missing values?
# find literacy data and merge