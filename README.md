# Team Name’s FIRE Summit Presentation
Team Members

## Research Question

How does the air pollution rate impact the violent crime rate in
Baltimore City? 

<img src="pollutioncrime.webp" width="237" />

## Data Wrangling

**Outcome variable**

Our outcome variable are the crime rates in the specific counties.

This data was obtained from Baltimore City Crime Data at a neighborhood
level.

We categorized each of the crime entries into property crimes and
violent crimes, and then organized them by date, as shown in the code
below.

``` r
#install.packages("tidyverse")
library("tidyverse")
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ✔ purrr     1.0.2     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
#Read crime data
crime <- read.csv("crime_14_22.csv")
crime2<-crime %>%
  
  mutate(Violent= ifelse(Description== "HOMICIDE", 1, 0)) %>%
  
  mutate(Violent= ifelse(Description== "AGG. ASSAULT", 1, Violent)) %>%
  
  mutate(Violent= ifelse(Description== "COMMON ASSAULT", 1, Violent)) %>% 
  
  mutate(Violent= ifelse(Description== "RAPE", 1, Violent)) %>% 
  
  mutate(Violent= ifelse(Description== "SHOOTING", 1, Violent)) %>% 
  
  mutate(Property= ifelse(Description== "ROBBERY - CARJACKING", 1, 0)) %>%
  
  mutate(Violent= ifelse(Description== "ROBBERY - COMMERCIAL", 1, Violent)) %>%
  
  mutate(Violent= ifelse(Description== "ROBBERY - STREET", 1,Violent)) %>%
  
  mutate(Violent= ifelse(Description== "ROBBERY - RESIDENCE", 1, Violent)) %>%
  
  mutate(Property= ifelse(Description== "ARSON", 1, Property)) %>%
  
  mutate(Property= ifelse(Description== "AUTO THEFT", 1, Property)) %>%
  
  mutate(Property= ifelse(Description== "BURGLARY", 1, Property)) %>%
  
  mutate(Property= ifelse(Description== "LARCENY", 1, Property)) %>%
  
  mutate(Property= ifelse(Description== "LARCENY FROM AUTO", 1, Property)) %>%
  
  mutate(Race = ifelse(Race == "", "UNKNOWN", Race)) %>%
  
  mutate(Neighborhood = ifelse(Neighborhood == "", "UNKNOWN", Neighborhood))

crime_by_date <- crime2 %>%
  group_by(Neighborhood, date) %>%
  summarize(total_violent_crime = sum(Violent), total_property_crime = sum(Property))
```

    `summarise()` has grouped output by 'Neighborhood'. You can override using the
    `.groups` argument.

Then we visualized the violent and property crimes by neighborhood shown
on the graphs below.

``` r
#| warning: false
library("terra")
```

    terra 1.7.55


    Attaching package: 'terra'

    The following object is masked from 'package:tidyr':

        extract

``` r
#library("tidyverse")

n<-vect("Neighborhood/Neighborhood.shp")
```

    Warning: [vect] Z coordinates ignored

``` r
n_project<-project(n, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
n_project$Name <- toupper(n_project$Name)

df_sum <- read.csv("finaldata.csv") %>%
  filter(Name %in% n_project$Name) %>%
  group_by(Name) %>%
  summarise(all_violent = sum(total_violent_crime), 
            all_property = sum(total_property_crime))
n2<-merge(n_project, df_sum, by = "Name")

plot(n2, "all_violent", plg = list(title = "Violent Crimes by Neighborhood"))
```

![](README_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
plot(n2, "all_property", plg = list(title = "Property Crimes by Neighborhood"))
```

![](README_files/figure-commonmark/unnamed-chunk-2-2.png)

**Treatment variable**

The treatment variable is an indicator of whether there is wind in each
county. We distinguished this by separating each date with a 0 or 1. A 0
meant there was no wind flowing in the direction of that county, ergo no
pollution that day. Similarly, a 1 meant there was wind flowing in that
direction, ergo pollution was present.

This data was obtained by NASA MERRA Data.

In this code, we first found the angle of each county using the
coordinate points from our data. Then, we converted the angles to
degrees. Finally, we categorized whether or not the county had wind
flowing in its direction with a zero or one using a if else statement.

``` r
{r}

#| eval: false
#| echo: false

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

The control variables we have taken into account are certain
neighborhood characteristics such as the majority race and gender of the
neighborhood.

The code below demonstrates the merging between the data sets of crime
by race, neighborhood, and date.

``` r
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
variable

![](kush%20graph.png)

## Did Regression Models

This is a Did regression model with our treatment group, treatment
period, and there interaction.

``` r
library("lfe")
```

    Loading required package: Matrix


    Attaching package: 'Matrix'

    The following objects are masked from 'package:tidyr':

        expand, pack, unpack

``` r
panel <- read.csv("paneldata(1).csv")
summary(mode <- lm(total_violent_crime~treatment, data = panel))
```


    Call:
    lm(formula = total_violent_crime ~ treatment, data = panel)

    Residuals:
        Min      1Q  Median      3Q     Max 
    -0.2891 -0.2093 -0.2093 -0.2093 11.7907 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    (Intercept) 0.209295   0.001747  119.83   <2e-16 ***
    treatment   0.079850   0.005636   14.17   <2e-16 ***
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.5878 on 125291 degrees of freedom
    Multiple R-squared:  0.001599,  Adjusted R-squared:  0.001591 
    F-statistic: 200.7 on 1 and 125291 DF,  p-value: < 2.2e-16

This is a Did regression model that includes everything from the
previous model, plus all of out other numerical, binary, and categorical
variables.

``` r
Model4<- felm(total_violent_crime~treatment + TLML + PRECTOT + SPEED| Name + month + dayofweek + year, data = panel)
summary(Model4)
```


    Call:
       felm(formula = total_violent_crime ~ treatment + TLML + PRECTOT +      SPEED | Name + month + dayofweek + year, data = panel) 

    Residuals:
        Min      1Q  Median      3Q     Max 
    -0.9973 -0.2255 -0.1087 -0.0088 11.8029 

    Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
    treatment  2.778e-03  6.332e-03   0.439 0.660826    
    TLML       1.799e-03  3.599e-04   4.998  5.8e-07 ***
    PRECTOT   -5.516e+01  1.432e+01  -3.850 0.000118 ***
    SPEED     -1.778e-03  7.786e-04  -2.284 0.022401 *  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Residual standard error: 0.551 on 125217 degrees of freedom
    Multiple R-squared(full model): 0.1231   Adjusted R-squared: 0.1226 
    Multiple R-squared(proj model): 0.0004061   Adjusted R-squared: -0.0001926 
    F-statistic(full model):234.5 on 75 and 125217 DF, p-value: < 2.2e-16 
    F-statistic(proj model): 12.72 on 4 and 125217 DF, p-value: 2.383e-10 
    *** Standard errors may be too high due to more than 2 groups and exactDOF=FALSE

Both regressions in each model were statistically significant, but the
second model is more accurate as takes more variables into account.
