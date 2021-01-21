########################################
# Clean data for left side of FIGURE 2
########################################
### Load data into new variables to compute infection and death rates
coronaCol <- covidByCounty
deathCol <- deathsByCounty
infectRate <- covidByCounty
deathRate <- deathsByCounty
numCols <- ncol(coronaCol)-1

# Compute infection and death rates
for (row in seq(5,numCols,1)) {
  row2 <- as.numeric(row) + 1
  infectRate[,row2] <- coronaCol[,row2] - coronaCol[,row]
  deathRate[,row2] <- deathCol[,row2] - deathCol[,row]
}
# Change unallocated names to NA
infectRate <- infectRate %>% mutate(County.Name = replace(County.Name, County.Name == "Statewide Unallocated", NA))
infectRate <- infectRate %>% mutate(County.Name = replace(County.Name, County.Name == "New York City Unallocated", NA))
deathRate <- deathRate %>% mutate(County.Name = replace(County.Name, County.Name == "Statewide Unallocated", NA))
deathRate <- deathRate %>% mutate(County.Name = replace(County.Name, County.Name == "New York City Unallocated", NA))

# Create infection and death rate timeline dataframes
infectRate_timeline <- na.omit(infectRate)
deathRate_timeline <- na.omit(deathRate)

### Save data to preserve timeline dataframes
infectRate_timeline2 <- infectRate_timeline
deathRate_timeline2 <- deathRate_timeline 

### Create data frame comparing correlation between infections and deaths with different delays 
infectRate_dates <- gather(infectRate_timeline2,covidDate,covidCount,X1.22.20:X8.20.20,
                           factor_key=TRUE)
day0 <- gather(deathRate_timeline2,day0,deathCount,X1.22.20:X8.20.20,factor_key=TRUE);
day0<-day0$deathCount;n<-length(day0)
day1 <- gather(deathRate_timeline2,day1,deathCount,X1.23.20:X8.20.20,factor_key=TRUE);
day1<-day1$deathCount;length(day1)<-n
day2 <- gather(deathRate_timeline2,day2,deathCount,X1.24.20:X8.20.20,factor_key=TRUE);
day2<-day2$deathCount;length(day2)<-n
day3 <- gather(deathRate_timeline2,day3,deathCount,X1.25.20:X8.20.20,factor_key=TRUE);
day3<-day3$deathCount;length(day3)<-n
day4 <- gather(deathRate_timeline2,day4,deathCount,X1.26.20:X8.20.20,factor_key=TRUE);
day4<-day4$deathCount;length(day4)<-n
day5 <- gather(deathRate_timeline2,day5,deathCount,X1.27.20:X8.20.20,factor_key=TRUE);
day5<-day5$deathCount;length(day5)<-n
day6 <- gather(deathRate_timeline2,day6,deathCount,X1.28.20:X8.20.20,factor_key=TRUE);
day6<-day6$deathCount;length(day6)<-n
day7 <- gather(deathRate_timeline2,day7,deathCount,X1.29.20:X8.20.20,factor_key=TRUE);
day7<-day7$deathCount;length(day7)<-n
day8 <- gather(deathRate_timeline2,day8,deathCount,X1.30.20:X8.20.20,factor_key=TRUE);
day8<-day8$deathCount;length(day8)<-n
day9 <- gather(deathRate_timeline2,day9,deathCount,X1.31.20:X8.20.20,factor_key=TRUE);
day9<-day9$deathCount;length(day9)<-n
day10 <- gather(deathRate_timeline2,day10,deathCount,X2.1.20:X8.20.20,factor_key=TRUE);
day10<-day10$deathCount;length(day10)<-n
day11 <- gather(deathRate_timeline2,day11,deathCount,X2.2.20:X8.20.20,factor_key=TRUE);
day11<-day11$deathCount;length(day11)<-n
day12 <- gather(deathRate_timeline2,day12,deathCount,X2.3.20:X8.20.20,factor_key=TRUE);
day12<-day12$deathCount;length(day12)<-n
day13 <- gather(deathRate_timeline2,day13,deathCount,X2.4.20:X8.20.20,factor_key=TRUE);
day13<-day13$deathCount;length(day13)<-n
day14 <- gather(deathRate_timeline2,day14,deathCount,X2.5.20:X8.20.20,factor_key=TRUE);
day14<-day14$deathCount;length(day14)<-n

### Change lists back to dataframes
day0<-data.frame(day0);
day1<-data.frame(day1);
day2<-data.frame(day2);
day3<-data.frame(day3);
day4<-data.frame(day4);
day5<-data.frame(day5);
day6<-data.frame(day6);
day7<-data.frame(day7);
day8<-data.frame(day8);
day9<-data.frame(day9);
day10<-data.frame(day10);
day11<-data.frame(day11);
day12<-data.frame(day12);
day13<-data.frame(day13);
day14<-data.frame(day14)

### Combine, change covidDate to date type, create long form dataframe, remove zeroies, 
### and extract population data from finalMatrix
covid_deathRate <- cbind(infectRate_dates,day0,day1,day2,day3,day4,day5,day6,day7,day8,day9,
                         day10,day11,day12,day13,day14)
# Change to type date
covid_deathRate$covidDate <- mdy(substring(covid_deathRate$covidDate, 2))
# Create long form data frame
covid_deathRate_long <- gather(covid_deathRate,dayDiff,deathCount,day0:day14,factor_key=TRUE)
covid_deathRate_long$dayDiff <- as.numeric(substring(covid_deathRate_long$dayDiff, 4))
# Remove zeroes
covid_deathRate_long <- subset(covid_deathRate_long, covidCount!=0)
covid_deathRate_long <- subset(covid_deathRate_long, deathCount!=0)
covid_deathRate_long <- subset(covid_deathRate_long,select=c(countyFIPS:covidDate,
                                                             dayDiff,covidCount,deathCount))
# Extract population
covid_deathRate_long$population <-
  finalMatrix$population[match(covid_deathRate_long$countyFIPS, finalMatrix$FIPS)]
covid_deathRate_long <- na.omit(covid_deathRate_long)

### Select data for FIGURE 2
my.formula <- y ~ x
dates <- unique(subset(covid_deathRate_long,covidDate >= as.Date("2020-03-22"))$covidDate)
countDate <- as.Date("2020-04-13")

covid_deathRate_long2 <- subset(covid_deathRate_long,dayDiff>=1 & dayDiff<=14 &
                                  covidDate == countDate &
                                  covidCount >= 10 &
                                  deathCount >= 2 &
                                  population >= 500000 
)
########################################
# Clean data for right side of FIGURE 2
########################################
dailyCasesDeaths_clean <- dailyCasesDeaths
dailyCasesDeaths_clean$Date <- as.Date(dailyCasesDeaths_clean$Date)
names(dailyCasesDeaths_clean)[names(dailyCasesDeaths_clean) ==
                                "Daily.confirmed.cases..cases."]<-"cases"
names(dailyCasesDeaths_clean)[names(dailyCasesDeaths_clean) ==
                                "Daily.confirmed.deaths..deaths."]<-"deaths"
dailyCasesDeaths_clean <- subset(dailyCasesDeaths_clean,
                                 Code=="USA" &
                                   Date >= as.Date("2020-03-01") &
                                   Date <= as.Date("2020-06-19"))
