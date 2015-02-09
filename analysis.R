---
title: "Project 1"
author: "---------"
date: "2/10/2015"
output: html_document
---

## Reproducible Research Peer Assessment 1
## Downloaded Data Date "Feb. 3, 2015" 
## Data

We can now download large amounts of data about personal movement
using activity monitoring devices. Data remains under-utilized due to
many two factors - data is hard to obtain and lack of statistical methods
to process and interpret the data.


The data for this assignment was  downloaded from the course web site:
on date : "02/10/2015"
Dataset: Activity monitoring data [52K]
Variables for this dataset : steps , date, interval(identifier for 5 min. interval)
### Loading and preprocessing data:

```{r dlchunk}
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

# Download Data 
if (!file.exists("data")) 
    dir.create("data")

if (!file.exists("data/data.zip")) 
    download.file(url, destfile = "data/data.zip", mode="wb")     

if (!file.exists("data/activity.csv"))			 
    unzip("data/data.zip", exdir="data")
```
Now for some quick analysis to see if it matches what we are suppose to get:

```{r chunk2}


```






