# Reproducible Research: Course Project 2 on NOAA storm database analysis

## Title: NOAA Storm Database explortion to answer some basic questions about severe weather events 

Synopsis: In this research paper I have explored/analysed the NOAA Storm Database for some basic questions about severe weather events. I have found answers for the questions below:

- Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
- Across the United States, which types of events have the greatest economic consequences? 


## Loading and preprocessing of the Storm data 

```r
sData <- read.csv("repdata-data-StormData.csv")

dim(sData)
```

```
## [1] 903870     40
```

```r
## head(sData)
```

I am keeping only relevant columns that are needed for my analysis. I will also convert column names to lowercase so that the data is easier to manipulate later on in my analysis

```r
sDataRelevant <- sData[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", 
    "CROPDMG", "CROPDMGEXP")]
names(sDataRelevant) <- tolower(names(sDataRelevant))
head(sDataRelevant)
```

```
##    evtype fatalities injuries propdmg propdmgexp cropdmg cropdmgexp
## 1 TORNADO          0       15      25          K       0           
## 2 TORNADO          0        0     2.5          K       0           
## 3 TORNADO          0        2      25          K       0           
## 4 TORNADO          0        2     2.5          K       0           
## 5 TORNADO          0        2     2.5          K       0           
## 6 TORNADO          0        6     2.5          K       0
```

## Results - Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

In terms of events affecting population health I will be mainly focussing on 'fatalities' and 'injuries' in my analysis. In this section I am arranging data such that I am aggregating 'fatalties' for different 'event' types. I am doing the same for the 'injuries' in relation to the different event types. 


```r
sDataRelevant$fatalities <- as.numeric(as.character(sDataRelevant$fatalities))
```

```
## Warning: NAs introduced by coercion
```

```r
sDataRelevant$injuries <- as.numeric(as.character(sDataRelevant$injuries))
```

```
## Warning: NAs introduced by coercion
```

```r
eventFatalities <- aggregate(fatalities ~ evtype, data = sDataRelevant, FUN = sum)
##table(eventFatalities$fatalities)
eventInjuries <- aggregate(injuries ~ evtype, data = sDataRelevant, FUN = sum)
##table(eventInjuries$injuries)
```

### Fatalities for different event types

```r
orderEventFatalities <- eventFatalities[order(eventFatalities$fatalities, decreasing = T), ]
orderEventInjuries <- eventInjuries[order(eventInjuries$injuries, decreasing = T), ]
library(ggplot2)
ggplot(orderEventFatalities[1:5, ], aes(evtype, fatalities)) + geom_bar(stat = "identity") + 
    ylab("Fatalities") + xlab("Event Type") + ggtitle("Deaths Across the U.S from Differents - Top Five")
```

![](RR_Course_Project2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Injuries for different event types

```r
ggplot(orderEventInjuries[1:5, ], aes(evtype, injuries)) + geom_bar(stat = "identity") + ylab("Injuries") + 
    xlab("Event Type") + ggtitle("Injuries Across the U.S from Different Events - Top Five")
```

![](RR_Course_Project2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


From the plots above you can see 'Tornado' is event that has caused a significant number of deaths (5,593 deaths) and injuries (90,671 injuries) to US polulation from 1950 to 2011

## Results - Across the United States, which types of events have the greatest economic consequences? 
In terms of events having greatest economic consequences I will be mainly focussing on 'PROPDMG',  'PROPDMGEXP',  'CROPDMG',  'CROPDMGEXP' in my analysis.

Below I am converting H, K, M, B units to calculate Property damage and Crop damage

```r
sDataRelevant$propdmg <- as.numeric(as.character(sDataRelevant$propdmg))
```

```
## Warning: NAs introduced by coercion
```

```r
sDataRelevant$cropdmg <- as.numeric(as.character(sDataRelevant$cropdmg))
```

```
## Warning: NAs introduced by coercion
```

```r
## table(sDataRelevant$propdmgexp)
## table(sDataRelevant$cropdmgexp)
prdmg <- sDataRelevant$propdmg
pdmgexp <- sDataRelevant$propdmgexp
crdmg <- sDataRelevant$cropdmg
crdmgeexp <- sDataRelevant$cropdmgexp

prdmg[pdmgexp %in% "B"] <- prdmg[pdmgexp %in% "B"] * 1000
prdmg[pdmgexp %in% c("M", "m")] <- prdmg[pdmgexp %in% c("M", "m")] * 1
prdmg[pdmgexp %in% c("K")] <- prdmg[pdmgexp %in% c("K")] * 0.001
prdmg[pdmgexp %in% c("H", "h")] <- prdmg[pdmgexp %in% c("H", "h")] * 1e-04
prdmg[!(pdmgexp %in% c("B", "M", "m", "K", "H", "h"))] <- prdmg[!(pdmgexp %in% c("B", "M", 
    "m", "K", "H", "h"))] * 1e-06

crdmg[crdmgeexp %in% "B"] <- crdmg[crdmgeexp %in% "B"] * 1000
crdmg[crdmgeexp %in% c("M", "m")] <- crdmg[crdmgeexp %in% c("M", "m")] * 1
crdmg[crdmgeexp %in% c("K", "k")] <- crdmg[crdmgeexp %in% c("K", "k")] * 0.001
crdmg[!(crdmgeexp %in% c("B", "M", "m", "K", "k"))] <- crdmg[!(crdmgeexp %in% c("B", "M", "m", 
    "K", "k"))] * 1e-06
```

In the below code, I am adding crop damage and property damage values for each event type, I then aggregate added damage values by the event type, and then plot the 2 variables


```r
economicdmg <- crdmg + prdmg
economicDamageType <- aggregate(economicdmg ~ sDataRelevant$evtype, FUN = sum)
economicDamageTypeOrdered <- economicDamageType[order(economicDamageType$economicdmg, decreasing = T), ]
names(economicDamageTypeOrdered)[1] <- "eventType"

ggplot(economicDamageTypeOrdered[1:7, ], aes(eventType, economicdmg)) + geom_bar(stat = "identity") + ylab("Economic Damages (in millions)") + 
    xlab("Event Type") + ggtitle("Events Causing Economic Damages Across the U.S - Top Seven")
```

![](RR_Course_Project2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


From the plots above you can see 'Flood' is event that has caused a significant economic damage ($150,000 million) from 1950 to 2011
