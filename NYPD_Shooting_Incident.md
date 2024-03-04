NYPD_Shooting_Incident_Analysis
================
2024-03-03

- Questions of interest:
  - Which boroughs were most affected by the shootings?
  - What were the number of shooting incidents by race and which race
    were affected the most and which the least?
  - What is the correlation between shootings by race and those that
    lead to deaths/murders?
- Description of NYPD_Shooting_Data
  - Each entity represents a single shooting incident for a single
    victim.
  - There are many fields included in the raw data many of which did not
    play a role in this analysis. Some of the fields that contribute
    valuable information for this analysis include: BORO (location of
    incident), STATISTICAL_MURDER_FLAG (whether the incident lead to
    murder), PERP_SEX, PERP_RACE, VIC_SEX, and VIC_RACE.

## Visualization \#1

- The following visualization compares the number of shooting incidents
  in boroughs in NYC.
- It also compares the number of cases which lead to murders by using
  the STATISTICAL_MURDER_FLAG attribute.
- ’True” represents the number of shootings leading to death.

``` r
##NYPD Shooting Data Import
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
url_in <- "https://data.cityofnewyork.us/api/views/833y-fsy8/rows.csv?accessType=DOWNLOAD"

nypd_shooting_data <- read_csv(url_in)
```

    ## Rows: 27312 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): OCCUR_DATE, BORO, LOC_OF_OCCUR_DESC, LOC_CLASSFCTN_DESC, LOCATION...
    ## dbl   (7): INCIDENT_KEY, PRECINCT, JURISDICTION_CODE, X_COORD_CD, Y_COORD_CD...
    ## lgl   (1): STATISTICAL_MURDER_FLAG
    ## time  (1): OCCUR_TIME
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
##Tidy up - delte columns not needed, change data type for OCCUR_DATE
library(lubridate)
nypd_shooting_data <- nypd_shooting_data %>%
  select(-c(INCIDENT_KEY, X_COORD_CD, Y_COORD_CD, Latitude, Longitude, Lon_Lat,LOC_OF_OCCUR_DESC, PRECINCT, JURISDICTION_CODE, LOC_CLASSFCTN_DESC))

nypd_shooting_data <- nypd_shooting_data %>%
  mutate(OCCUR_DATE = mdy(OCCUR_DATE))

  
##See summary and filter through data as needed
summary(nypd_shooting_data)
```

    ##    OCCUR_DATE          OCCUR_TIME           BORO           LOCATION_DESC     
    ##  Min.   :2006-01-01   Length:27312      Length:27312       Length:27312      
    ##  1st Qu.:2009-07-18   Class1:hms        Class :character   Class :character  
    ##  Median :2013-04-29   Class2:difftime   Mode  :character   Mode  :character  
    ##  Mean   :2014-01-06   Mode  :numeric                                         
    ##  3rd Qu.:2018-10-15                                                          
    ##  Max.   :2022-12-31                                                          
    ##  STATISTICAL_MURDER_FLAG PERP_AGE_GROUP       PERP_SEX        
    ##  Mode :logical           Length:27312       Length:27312      
    ##  FALSE:22046             Class :character   Class :character  
    ##  TRUE :5266              Mode  :character   Mode  :character  
    ##                                                               
    ##                                                               
    ##                                                               
    ##   PERP_RACE         VIC_AGE_GROUP        VIC_SEX            VIC_RACE        
    ##  Length:27312       Length:27312       Length:27312       Length:27312      
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ## 

``` r
##Visualization #1 Which borough has the most shootings and which has the least, and which of those
##lead to murders? Drop any rows with missing information on murder.
nypd_shooting_data %>%
drop_na(STATISTICAL_MURDER_FLAG) %>%
ggplot(aes(BORO, fill = STATISTICAL_MURDER_FLAG))+
geom_bar(alpha = 0.5)+
ggtitle("Shootings by Borough")+
theme(plot.title = element_text(hjust=0.5))
```

![](NYPD_Shooting_Incident_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Visualization \#2

- Next, we want to visualize the race of victims to see the race most
  frequently victimized.

``` r
nypd_shooting_data %>%
drop_na(VIC_RACE) %>%
ggplot(aes(VIC_RACE))+
geom_bar(alpha = 0.5)+
labs(x = "Race of Victim", y = "Count")+
ggtitle("Race of Victims")+
theme(axis.text.x = element_text(size=6, angle = 15), 
      plot.title = element_text(hjust=0.5))
```

![](NYPD_Shooting_Incident_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

- The following pie charts reveal additional intriguing details about
  the shooting data:
  - “Victims from Shootings by Race” compares the total number of
    victims of shootings by race
  - “Confirmed Deaths from Shootings by Race” compares the total number
    of confirmed deaths from those shootings by race
- As expected, the two charts look almost identical as the chance of
  death should be similar when shot no matter the race of victim.

``` r
slices <- c(sum(nypd_shooting_data$VIC_RACE == "AMERICAN INDIAN/ALASKAN NATIVE") 
                + sum(nypd_shooting_data$VIC_RACE == "UNKNOWN"),
            sum(nypd_shooting_data$VIC_RACE == "ASIAN / PACIFIC ISLANDER"),
            sum(nypd_shooting_data$VIC_RACE == "BLACK"),
            sum(nypd_shooting_data$VIC_RACE == "BLACK HISPANIC"),
            sum(nypd_shooting_data$VIC_RACE == "WHITE"),
            sum(nypd_shooting_data$VIC_RACE == "WHITE HISPANIC"))
lbls <- c("OTHER RACE", "ASIAN/P. ISLANDER", "BLACK", "BLK HISP.", "WHITE", "WHITE HISPANIC")
pie(slices, labels = lbls, main="Victims from Shootings by Race", cex=0.5)
```

![](NYPD_Shooting_Incident_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
slices <- c(sum(nypd_shooting_data$VIC_RACE == "AMERICAN INDIAN/ALASKAN NATIVE" & 
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE')
            +sum(nypd_shooting_data$VIC_RACE == "UNKNOWN" &
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE'),
            sum(nypd_shooting_data$VIC_RACE == "ASIAN / PACIFIC ISLANDER" &
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE'),
            sum(nypd_shooting_data$VIC_RACE == "BLACK" &
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE'),
            sum(nypd_shooting_data$VIC_RACE == "BLACK HISPANIC" &
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE'),
            sum(nypd_shooting_data$VIC_RACE == "WHITE" &
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE'),
            sum(nypd_shooting_data$VIC_RACE == "WHITE HISPANIC" &
                  nypd_shooting_data$STATISTICAL_MURDER_FLAG == 'TRUE'))
lbls <- c("OTHER RACE", "ASIAN/P. ISLANDER", "BLACK", "BLK HISP.", "WHITE", "WHITE HISPANIC")
pie(slices, labels = lbls, main="Confirmed Deaths from Shootings by Race", cex = 0.5)
```

![](NYPD_Shooting_Incident_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

- Bias Identification
  - The NYPD shooting data exposes a disturbing reality of
    disproportionate impacts on the black community, where both the
    number of victims and confirmed deaths are alarmingly high. This
    outcome aligns with the pervasive systemic issues deeply rooted in
    our society. White Hispanics, black Hispanics, and other racial
    groups also experience significant disparities.

  - It’s important to confront biases in data collection, including
    potential underreporting and limitations in racial categorization.
    As the researcher, I acknowledge the need for constant
    self-awareness and have strived to approach the analysis with a
    commitment to social justice.
