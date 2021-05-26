# This script explores the BLS api. In this example, I pull CA county employment statistics from the BLS
# and import vaccination data from the CDC and make scatterplots and estimate a did. 
# I also document racial and gender unemployment gaps over time

library(tidycensus)
library(dplyr)
library(tidyr)
library(blsAPI)
library(ggplot2)
library(fixest)


# set working directory
setwd("/Users/petermannino/Documents/Data Manipulation Training/bls_api")

# get fips codes for counties
data(fips_codes)

##### CA County Unemployment and vaccinations #####

# create series IDs for the datasets I want
ca_county<-fips_codes %>%
  filter(state=="CA") %>%
  mutate(u_rate_series=paste("LAUCN",state_code,county_code,"0000000003", sep=''), # the ids for county unemployment rates
         employment_series=paste("LAUCN",state_code,county_code,"0000000005", sep=''),
         labor_force_series=paste("LAUCN",state_code,county_code,"0000000006", sep=''),
         geoid=paste(state_code,county_code,sep='')) # create county geoid

# put the series ids in a vector
series_id<-as.vector(ca_county$u_rate_series)

# create the payload for the bls api call
payload<-list('seriesid' = series_id,
              'startyear' = 2019,
              'endyear' = 2021,
              'registrationKey' = 'd71553aae6e0461ea407e08d772e26ed')

u_rate<-blsAPI(payload = payload, api_version = 2, return_data_frame = TRUE) # bls api call, return df

u_rate$geoid<- substr(u_rate$seriesID,6,10) # generate county geoids
u_rate$month_year<-lubridate::my(paste(substr(u_rate$period,2,3), "/", u_rate$year, sep="")) # generate month-year var


# calculate the percent change in the u-rate between Dec 2020 and Mar 2021, and Aug 2020 and Nov 2020
u_change<- u_rate %>%
  filter((year == 2021 & period == "M03") | 
           (year == 2020 & period == "M12") |
           (year == 2020 & period == "M08") |
           (year == 2020 & period == "M11")) %>% #limit data to Dec, Mar, Aug, and Nov
  select(periodName, value, geoid) %>%
  mutate(value=as.numeric(value)) %>% # convert u-rate to numeric
  tidyr::spread(key=periodName,value=value) %>% # reshape df
  mutate(u_change=((March-December)/December)*100,
         pre_u_rate=((November-August)/August)*100) # calc percent change over qs

# import vaccination rate data, clean and transform data - only keep through march for consistency with U-rate data
county_vacc<-read.csv("county_vacc_data.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) %>% # import
  filter(demographic_category=="Race/Ethnicity",
         administered_date %in% c('3/31/21', '2/28/21','1/31/21','12/31/20')) %>% # filter and keep only end of month totals
  group_by(county, administered_date) %>%
  summarize(first_dose=sum(replace_na(cumulative_at_least_one_dose,0)), 
            pop=sum(replace_na(est_population,0))) %>% # summarize across all race/ethnicities to get totals
  mutate(county_name=paste(county, "County"),
         pct_first_dose=(first_dose/pop)*100, # generate pct vacc'd
         month_year=lubridate::floor_date(lubridate::mdy(administered_date),'month')) %>% # add month-year
  left_join(ca_county[c("county","geoid")],by=c("county_name" = "county")) # add geoid by joining on county name


# merge unemployment changes to vaccination pcts
u_change_vacc<- u_change %>%
  left_join(county_vacc %>%
              filter(administered_date=='3/31/21') %>%
              select(geoid,pct_first_dose))


# create scatterplot of vaccination through march and unemployment rate change Dec-March
ggplot(data = u_change_vacc,mapping = aes(x=pct_first_dose, y=u_change)) + 
  geom_point() +
  geom_smooth(method='lm') +
  xlab(label = "Percent of Total Pop That Received First Dose") + 
  ylab("Percent Change in Unemployment Rate, December - March") +
  labs(title = "Unemployment Rate and Vaccinations",
       subtitle = "Counties in California - December 2020 to March 2021") +
  ggpubr::stat_regline_equation(label.x = 45, label.y = 10,size=3.5) +
  ggpubr::stat_cor(aes(label=..rr.label..), label.x = 45, label.y = 8,size=3.5) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8))

# fairly weak correlation. Every pp increase in pop with at least first dose
# assoc with an unemployment rate decreases of 0.26%. 

# save
ggsave(filename = "vaccination_employment.png", device="png",
       units='in', height = 4, width=6)
  
# create scatterplot of vaccination and unemployment rate change before vaccinations started (Aug-Nov 2020)
ggplot(data = u_change_vacc,mapping = aes(x=pct_first_dose, y=pre_u_rate)) + 
  geom_point() +
  geom_smooth(method='lm') +
  xlab(label = "Percent of Total Pop that Received First Dose") + 
  ylab("Percent Change in Unemployment Rate, August - November") +
  labs(title = "Unemployment Rate and Vaccinations",
       subtitle = "Counties in California - August 2020 to November 2020") +
  ggpubr::stat_regline_equation(label.x = 45, label.y = -10,size=3.5) +
  ggpubr::stat_cor(aes(label=..rr.label..), label.x = 45, label.y = -12,size=3.5) +
  theme(axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8))

# save
ggsave(filename = "pre_vaccination_employment.png", device="png",
       units='in', height = 2, width=4)

# actually a stronger relationship between change in unemployment rate and vaccinations through march
# for time period before vaccinations started. 


# join vaccination data to unemployment data
county_u_vac<-u_rate %>%
  left_join(county_vacc[c("geoid","month_year", "first_dose","pop","pct_first_dose")], 
            by=c("geoid" = "geoid","month_year"="month_year")) %>%
  mutate(pct_first_dose=replace_na(pct_first_dose,0),
         value=as.numeric(value)) 

# A simple twfe model of pct first dose on unemployment 

model<-feols(value~pct_first_dose | geoid + month_year, data = county_u_vac)
summary(model)

# result is very small and very insignificant. vaccination prob hasn't improved the labor market yet.

'OLS estimation, Dep. Var.: value
Observations: 1,351 
Fixed-effects: geoid: 50,  month_year: 28
Standard-errors: Clustered (geoid) 
Estimate Std. Error  t value Pr(>|t|) 
pct_first_dose -0.01489   0.029438 -0.50581 0.615258 
---
  Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1
RMSE: 1.49     Adj. R2: 0.876898
Within R2: 3.481e-4'''



##### Racial unemployment gaps #####

# series IDs and names
series<-c("LNS14000007","LNS14000008","LNS14000006","LNS14000003","LNS14000004","LNS14000005")
names<-c("u_black_men","u_black_women","u_black_total","u_white_total","u_white_men","u_white_women")

# api is limited to 20 years, so generate three payload lists for the years
payload<-list('seriesid'=series,
              'startyear'="1972",
              'endyear'="1991",
              'registrationKey' = 'd71553aae6e0461ea407e08d772e26ed')

payload2<-list('seriesid'=series,
              'startyear'="1992",
              'endyear'="2011",
              'registrationKey' = 'd71553aae6e0461ea407e08d772e26ed')

payload3<-list('seriesid'=series,
               'startyear'="2012",
               'endyear'="2021",
               'registrationKey' = 'd71553aae6e0461ea407e08d772e26ed')


# get data from the bls api and add clean names to the variable names
r_urate<-rbind(blsAPI(payload, api_version = 2, return_data_frame = TRUE),
               blsAPI(payload2, api_version = 2, return_data_frame = TRUE),
               blsAPI(payload3, api_version = 2, return_data_frame = TRUE)) %>%
  left_join(as.data.frame(list('series' = series, 'names' = names)), by=c("seriesID"="series")) %>%
  mutate(value=as.numeric(value)) 

# create a month-year field
r_urate$month_year<-lubridate::my(paste(substr(r_urate$period,2,3), "/", r_urate$year, sep=""))

# create clean df with unemployment by gender, race, and month/year
r_g_urate<- r_urate %>% 
  filter(names %in% c("u_black_men","u_black_women","u_white_men","u_white_women")) 

# plot line graph of unemployment rates 1972-2021
ggplot(data=r_g_urate, mapping = aes(x=month_year, y=value, group=names, color=names)) + 
  geom_line() +
  labs(color="Race and Gender",title = "Unemployment Rate by Race and Gender") +
  xlab(label = "Month-Year") + ylab("Unemployment Rate") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size=6),
        legend.title = element_text(size=8),
        axis.title.x = element_text(size=8),
        axis.title.y=element_text(size=8))

# save
ggsave(filename = "unemployment-by-race_and_gender.png", device="png",
       units='in', height = 3, width=5)

# create df for the different between total white and black unemployment by month-year
r_urate_diff<- r_urate %>%
  select(month_year, names, value) %>%
  filter(names %in% c("u_black_total",'u_white_total')) %>%
  tidyr::spread(key=names, value=value) %>%
  mutate(u_diff=u_black_total-u_white_total)

# plot line graph
ggplot(data=r_urate_diff,mapping = aes(x=month_year, y=u_diff)) +
  geom_line() +
  labs(title = "Black-White Unemployment Rate Difference") +
  xlab(label = "Month-Year") + ylab("Unemployment Rate Difference") +
  theme(axis.title.x = element_text(size=8),
        axis.title.y=element_text(size=8))

# save
ggsave(filename = "b-w-unemployment-diff.png", device="png",
       units='in', height = 3, width=5)
