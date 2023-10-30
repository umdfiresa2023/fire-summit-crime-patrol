Crime Patrol's FIRE Summit Presentation
================
Advik, Alan, Brian Mark, Esha, Kush, Sreeja

## Research Question

How does the air pollution rate impact the violent crime rate in Baltimore City?

![](pollutioncrime.webp)

## Data Wrangling

**Outcome variable**

Our outcome variable are the crime rates in the specific counties.

This data was obtained from Baltimore City Crime Data at a neighborhood level.

We categorized each of the crime entries into property crimes and violent crimes, and then organized them by date, as shown in the code below.

``` 
install.packages("tidyverse")
library("tidyverse")

#Read crime data
crime <- read.csv("DATA/crime_14_22.csv")
crime2<-crime %>%
  
  mutate(Violent= ifelse(Description== "HOMICIDE", 1, 0)) %>%
  
  mutate(Violent= ifelse(Description== "AGG. ASSAULT", 1, Violent)) %>%
  
  mutate(Property= ifelse(Description== "ROBBERY - CARJACKING", 1, 0)) %>%
  
  mutate(Property= ifelse(Description== "ARSON", 1, Property)) %>%
  
  crime_by_date <- crime2 %>%
  group_by(Neighborhood, date) %>%
  summarize(total_violent_crime = sum(Violent), total_property_crime = sum(Property))
```

**Treatment variable**

The treatment variable is an indicator of whether there is wind in each county. We distinguished this by separating each date with a 0 or 1. A 0 meant there was no wind flowing in the direction of that county, ergo no pollution that day. Similarly, a 1 meant there was wind flowing in that direction, ergo pollution was present.

This data was obtained by NASA MERRA Data.

In this code, we first found the angle of each county using the coordinate points from our data. Then, we converted the angles to degrees. Finally, we categorized whether or not the county had wind flowing in its direction with a zero or one using a if else statement.
``` 
install.packages("terra")
install.packages("tidyterra")
library("terra")
library("tidyterra")

all<-read.csv("full_merge.csv") %>%
  rename(Name=Neighborhood) %>%
  mutate(wind_dir=atan2(ULML, VLML)*(180/pi))

all2<-merge(df2, all, by="Name")%>%
  mutate(treatment = ifelse(wind_dir > min_ang & wind_dir< max_ang, 1, 0)) %>%
  filter(!is.na(treatment)) %>%
  select(-X.x, -X.y, -ang1, -ang2, -n1x, -n2x, -n1y, -n2y, -ULML, -VLML)

write.csv(all2, "finaldata.csv", row.names = F)

test<-all2 %>%
  select(min_ang, max_ang, wind_dir) %>%
  mutate(in_dir = ifelse(wind_dir > min_ang & wind_dir< max_ang, 1, 0))
``` 
**Control variables**

The control variables we have taken into account are certain neighborhood characteristics such as the majority race and gender of the neighborhood.

The code below demonstrates the merging between the data sets of crime by race, neighborhood, and date.

``` 
library("tidyverse")
library("dplyr")

intersect<-read.csv("road_neighborhood_intersect.csv")
race<-read.csv("crime_by_race.csv")
race2 = subset(race, select = -c(X))
intersect2<-intersect %>%
  mutate(Neighborhood = toupper(ndf))
colnames(intersect2)[3] <- "intersects_with_road"
intersect3 = subset(intersect2, select = -c(X, ndf))
crime_by_race_date_intersect <- full_join(race2, intersect3, by = "Neighborhood")
``` 
## Preliminary Results

Display a figure showing how the treatment variable impacted the outcome
variable.
```
data <- read.csv("finaldata.csv")
wind_graph_data <- data %>%
  select (Name, total_violent_crime, treatment) %>%
  filter(total_violent_crime >= 1, treatment == 1)

no_wind_graph_data <- data %>%
  select(Name, total_violent_crime, treatment) %>%
  filter(total_violent_crime >= 1, treatment == 0)

wind_data <- wind_graph_data %>%
  group_by(Name) %>%
  summarize(crime = sum(total_violent_crime) / n()) %>%
  mutate(group="Wind")

nowind_data <- no_wind_graph_data %>%
  group_by(Name) %>%
  summarize(crime = sum(total_violent_crime) / n()) %>%
  mutate(group="No Wind")

n<-wind_data$Name[1:20]

combined_data <- rbind(wind_data, nowind_data)%>%
  filter(Name %in% n) %>%
  arrange(desc(crime))

ggplot(combined_data, aes(x = Name, y=crime, fill=group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Crime by County (Wind Data vs. No Wind Data)",
       x = "County", y = "Average Number of Violent Crimes Per Day") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  scale_y_continuous(labels = scales::comma)
```
