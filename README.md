# README
Paola Leon, Eyal Greenberg and Kyle Killion  
July 24, 2016  


# Unit10CaseStudy2

## Introduction

The prime minister of Chulwalhar has asked us to help him in forecasting exports from his country. In order to do this we have been given as-is data, which is the original or observed data, and planned data, which is what Chulwalhar would like to export. We also have a list of indicators that may affect exports. Our job is to find out the best way to forecast Chulwalhar’s exports in 2014 based on data collected before this year. In other words, we want to create a credible statistical model.

The export data for Chulwalhar are in two CSV files. One contains the as-is data and the other one contains the planned data. These data sets are also composed of other data sets: monthly and yearly for both groups. Your task is to take all of these data sets, import them into R, and develop a model to forecast the exports of these particular products for the prime minister of Chulwalhar.

Original data sources can be found in the link below:

https://s3-us-west-2.amazonaws.com/smu-mds/prod/MSDS+6306+Doing+Data+Science/Week+10/ChulwalarCase.zip

<br><br>

##Instructions
1. Submit an R markdown document with the code necessary to download, clean and analyze the data. 
2. Interpretations of the code and analysis should be provided. 
3. There should also be at least one graphic that explains an important feature of the data.
4. This graphic should be interpreted in the text of the document.


## Recording the session info

```r
sessionInfo()
```

```
## R version 3.3.0 (2016-05-03)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] magrittr_1.5    tools_3.3.0     htmltools_0.3.5 yaml_2.1.13    
##  [5] Rcpp_0.12.5     stringi_1.0-1   rmarkdown_0.9.6 knitr_1.13     
##  [9] stringr_1.0.0   digest_0.6.9    evaluate_0.9
```
