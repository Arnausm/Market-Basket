#### Practise 2.4 Discover Associations Between Products ####

#### Packages ####

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(arules,arulesViz,tidyverse,readxml,knitr,lubridate,plyr)


#### Data ####

# df <- read.transactions("C:/Users/Arnau/Documents/Task 2-4/ElectronidexTransactions2017.csv",
#                        sep = ",",
#                       format = "basket")

# df_retail <- read.transactions("C:/Users/Arnau/Documents/Task 2-4/RetailElectronidexDataset.csv",
#                               sep = ",",
#                               format = "basket")

# df_business <- read.transactions("C:/Users/Arnau/Documents/Task 2-4/BusinessElectronidexDataset.csv",
#                                sep = ",",
#                               format = "basket")

df <- read.transactions("C:/Users/User/Desktop/Practiques bones/Task 2-4/ElectronidexTransactions2017.csv",
                        sep = ",",
                        format = "basket")

df_retail <- read.transactions("C:/Users/User/Desktop/Practiques bones/Task 2-4/Retail_data.csv",
                              sep = ",",
                              format = "basket")

df_business <- read.transactions("C:/Users/User/Desktop/Practiques bones/Task 2-4/Business_data.csv",
                                sep = ";",
                                format = "basket")

# The conditions used to split the data between retail and business.
# Business:
# - One or more laptop and one or more desktop bought in the same transaction
# - Two or more monitors bought in the same transaction
# - One or more keyboard, Mice or Combo in the same transaction.


#### Item frequency plots ####

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
  install.packages("RColorBrewer")
  #include library RColorBrewer
  library(RColorBrewer)
}

## Absolute

itemFrequencyPlot(df_business,topN=8,
                  type="absolute",
                  col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(df_retail,topN=10,
                  type="absolute",
                  col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")
itemFrequencyPlot(df,topN=10,
                  type="absolute",
                  col=brewer.pal(8,'Pastel2'),
                  main="Absolute Item Frequency Plot")

## Relative

itemFrequencyPlot(df_business,topN=8,
                  type="relative",
                  col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")
itemFrequencyPlot(df_retail,topN=10,
                  type="relative",
                  col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")
itemFrequencyPlot(df,topN=10,
                  type="relative",
                  col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")

#### Simple contingency table ####

# crossTable() function allows showing and sorting items and pairs of items by:
  # - count
  # - support
  # - lift
  # - chiSquared

tbl_df <- crossTable(df, sort=TRUE)
tbl_df[1:5,1:5]

tbl_retail <- crossTable(df_retail, sort=TRUE)
tbl_retail[1:5,1:5]

tbl_business <- crossTable(df_business, sort=TRUE)
tbl_business[1:5,1:5]

## Lift

  # How more often the rule under questions happens than if it did simply happen by chance.
  # In general, we prefer higher lift over lower lift.

crossTable(df, measure='lift',sort=T)[1:5,1:5]
crossTable(df_retail, measure='lift',sort=T)[1:4,1:4]  # Only 4 because the name of the last item is so large
crossTable(df_business, measure='lift',sort=T)[1:5,1:5]
  
## Chi

  # If lift is close to 1, but counts are large
  # Or lift is meaningfully different from 1, but counts are low
  # We may need to turn to statistical chiSquared test to prove that events A and B are statistically dependent (i.e. we did not run into spurious correlation)

  # chi-square test for independence compares two variables to see if they are related.
  # - A very small chi square test statistic means that your observed data fits your expected data extremely well. In other words, there is a relationship.
  # - A very large chi square test statistic means that the data does not fit very well. In other words, there isn't a relationship.

crossTable(df, measure='chi')['iMac', 'Apple MacBook Air']
crossTable(df_retail, measure='chi')['AppleEarpods', 'AppleMacBookAir']
crossTable(df_business, measure='chi')['iMac','HPLaptop']

  # Chi values obtained are very low so there's a relationship.

#### Min Support as 0.001, confidence as 0.9 ####

rules_df <- apriori(df,parameter = list(supp=0.001,conf=0.8,maxlen=10))
rules_df_retail <- apriori(df_retail,parameter = list (supp=0.001,conf=0.8,maxlen= 10))
rules_df_business <- apriori(df_business,parameter = list (supp=0.001,conf=0.8,maxlen= 6))

## Remove duplicated rules

subset.rules_df <- which(colSums(is.subset(rules_df,rules_df)) > 1) # get subset rules in vector
length(subset.rules_df) # 36 rules

subset.rules_retail <- which(colSums(is.subset(rules_df_retail,rules_df_retail)) > 1)
length(subset.rules_retail) # 1 rules

subset.rules_business <- which(colSums(is.subset(rules_df_business,rules_df_business)) > 1)
length(subset.rules_business) # 3060 rules

## Rules

inspect(head(df))
inspect(head(df_retail))
inspect(head(df_business))

## Filter rules with confidence greater than 0.4 or 40%

subRules<-rules_df[quality(rules_df)$confidence>0.4]

#Plot SubRules
plot(subRules)

plot(subRules,method="two-key plot")

#### Rules per confidence ####

# All data

rules_df_conf <- sort(rules_df, by = "confidence", decreasing = TRUE)
inspect(head(rules_df_conf))

inspect(head(rules_df, n = 10, by = "confidence")) # same return

# Business

rules_df_business_conf <- sort(rules_df_business, by = 'confidence', decreasing = T)
inspect(head(rules_df_business_conf))

inspect(head(rules_df_business, n = 10, by = "confidence")) # same return

# Retail

rules_df_retail_conf <- sort(rules_df_retail, by = 'confidence', decreasing = T)
inspect(head(rules_df_retail_conf))

inspect(head(rules_df_retail, n = 10, by = "confidence")) # same return


#### Rules per lift ####

# All data

rules_df_lift <- sort(rules_df, by = "lift", decreasing = TRUE)
inspect(head(rules_df_lift))

inspect(head(rules_df, n = 10, by = "lift")) # same return

# Business

rules_df_business_lift <- sort(rules_df_business, by = 'lift', decreasing = T)
inspect(head(rules_df_business_lift))

inspect(head(rules_df_business, n = 10, by = "lift")) # same return

# Retail

rules_df_retail_lift <- sort(rules_df_retail, by = 'lift', decreasing = T)
inspect(head(rules_df_retail_lift))

inspect(head(rules_df_retail, n = 10, by = "lift")) # same return


#### Graph of rules ####

# All data

top_rules_df<- head(rules_df, n = 10, by = "confidence")

plot(top_rules_df, method = "graph",  engine = "htmlwidget")

# Retail

top_rules_retail<- head(rules_df_retail, n = 10, by = "confidence")

plot(top_rules_retail, method = "graph",  engine = "htmlwidget")

# Business

top_rules_business<- head(rules_df_business, n = 10, by = "confidence")

plot(top_rules_business, method = "graph",  engine = "htmlwidget")


