# This script explores the BLS api. In this example, I pull CA county employment statistics from the BLS
# and import vaccination data from the CDC and make a scatterplot.

library(tidycensus)
library(blsAPI)
library(ggplot2)

# set working directory
setwd("/Users/petermannino/Documents/bls_api")

# get fips codes for counties
data(fips_codes)

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
              'registrationKey' = 'api key here')

u_rate<-blsAPI(payload = payload, return_data_frame = TRUE) # bls api call, return df

u_rate$geoid<- substr(response$seriesID,6,10) # generate county geoids

# calculate the percent change in the u-rate between December 2020 and March 2021
u_change<- u_rate %>%
  filter((year == 2021 & period == "M03") | (year == 2020 & period == "M12")) %>% #limit data to Dec and Mar
  select(periodName, value, geoid) %>%
  mutate(value=as.numeric(value)) %>% # convert u-rate to numeric
  tidyr::spread(key=periodName,value=value) %>% # reshape df
  mutate(u_change=((March-December)/December)*100) # calc percent change over first q

# import vaccination rate data - data doesn't have a geoid :(
county_vacc<-read.csv("county_vacc.csv", header=TRUE, sep=",",stringsAsFactors = FALSE) %>%
  mutate(county_name=paste(County, "County")) %>%
  left_join(ca_county[c("county","geoid")],by=c("county_name" = "county")) # join on county name (works good enough)

# join vaccination data to unemployment change data
county_u_vac<-u_change %>%
  left_join(county_vacc[c("geoid","pct_total_pop")]) %>%
  mutate(pct_total_pop=as.numeric(pct_total_pop))

# create scatterplot of vaccination and unemployment rate change
ggplot(data = county_u_vac,mapping = aes(x=pct_total_pop, y=u_change)) + 
  geom_point() +
  geom_smooth(method='lm') +
  xlab(label = "Percent of Total Population Fully Vaccinated") + 
  ylab("Percent Change in Unemployment Rate, December - March") +
  labs(title = "Unemployment Rate and Vaccinations",
       subtitle = "Counties in California") +
  ggpubr::stat_regline_equation(label.x = 45, label.y = 1,size=3.5) +
  ggpubr::stat_cor(aes(label=..rr.label..), label.x = 45, label.y = .8,size=3.5) +
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))

# relatively strong negative correlation. R2 is .33. Every pp increase in pop vaccinated
# assoc with an unemployment rate decreases of 0.35%. Vaccination might be a decent stimulus

# save
ggsave(filename = "vaccination_employment.png", device="png")

  


