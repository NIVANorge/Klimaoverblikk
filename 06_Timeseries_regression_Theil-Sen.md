---
title: "06 - Time series regressions using Mann-Kendall and Theil-Sen"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_depth: 3
    toc_float: true
    code_folding: hide
    df_print: paged   
---

Non-parametric tests for trends over time  
* Performed for seasons separately (note that hydrography + plankton seasons start in December)  
* Uses data from script 05  
* Test = Mann-Kendall trend test (for P) and Theil-Sen's slope estimator (for percent change per year)   
__NOTE: Have yet to include timing of spring flood (df_rivers_springflood) and DCA scores.__
  

DHJ: you borrowed much of this code from   'file:///C:/Data/Referanseelver_2018/Climate/05_Analysis_gridded_data_MK_TheilSen.html'    



## 0. Libraries

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.2     v dplyr   0.7.6
## v tidyr   0.8.1     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
```

```
## -- Conflicts ------------------------------------------------------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(readxl)
library(broom)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
# install.packages("rkt") 
library(rkt)        # rkt() - computes the Mann-Kendall tests and Theil-Sen's slope estimator

# library(pander)
```

## 1. Data (annual and partly seasonal)  
### a. Read data

```r
df_rivers_summ <- read.csv("Data_produced/05_df_rivers_summ.csv")
df_rivers_springflood <- read.csv("Data_produced/05_df_rivers_springflood.csv")
df_hydro_summ <- read.csv("Data_produced/05_df_hydro_summ.csv")
df_plank_summ <- read.csv("Data_produced/05_df_plank_summ.csv")
```

### b. Collect data  
* River data
    + discharge divided by 1 million, the rest divided by 1000

```r
df1 <- df_rivers_summ %>%
  mutate(DisTot = DisTot/1E3) %>%
  gather("Variable", "Value", TrspTot_TOTN:DisTot) %>%
  mutate(River_type = paste0("River_", River_type),
         Variable = sub("TrspTot_", "", Variable, fixed = TRUE)) %>%
  mutate(Variable = paste0(River_type, "_", Variable),
         Value = Value/1E3) %>%
  select(Variable, Year, Quarter, Value)
head(df1)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Year"],"name":[2],"type":["int"],"align":["right"]},{"label":["Quarter"],"name":[3],"type":["int"],"align":["right"]},{"label":["Value"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"River_Distant_TOTN","2":"1990","3":"1","4":"444.0407","_rn_":"1"},{"1":"River_Distant_TOTN","2":"1990","3":"2","4":"480.7027","_rn_":"2"},{"1":"River_Distant_TOTN","2":"1990","3":"3","4":"355.3202","_rn_":"3"},{"1":"River_Distant_TOTN","2":"1990","3":"4","4":"327.7417","_rn_":"4"},{"1":"River_Distant_TOTN","2":"1991","3":"1","4":"387.9025","_rn_":"5"},{"1":"River_Distant_TOTN","2":"1991","3":"2","4":"415.8960","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
# df1 %>% group_by(Variable) %>% summarise(mean(Value, na.rm = TRUE))

df2 <- df_hydro_summ %>%
  gather("Variable", "Value", Temperatur:Siktdyp) %>%
  mutate(Variable = paste0(Variable, "_", Depth)) %>%
  filter(!(Variable %in% c("Siktdyp_Intermediate", "Siktdyp_Deep"))) %>%
  select(Variable, Year, Quarter, Value) %>%
  select(Variable, Year, Quarter, Value)

head(df2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Year"],"name":[2],"type":["int"],"align":["right"]},{"label":["Quarter"],"name":[3],"type":["int"],"align":["right"]},{"label":["Value"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Temperatur_Deep","2":"1990","3":"1","4":"6.827000","_rn_":"1"},{"1":"Temperatur_Intermediate","2":"1990","3":"1","4":"6.507000","_rn_":"2"},{"1":"Temperatur_Surface","2":"1990","3":"1","4":"5.990500","_rn_":"3"},{"1":"Temperatur_Deep","2":"1990","3":"2","4":"6.749625","_rn_":"4"},{"1":"Temperatur_Intermediate","2":"1990","3":"2","4":"6.850250","_rn_":"5"},{"1":"Temperatur_Surface","2":"1990","3":"2","4":"7.659833","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
df3 <- df_plank_summ %>%
  gather("Variable", "Value", Kiselalger_med:Total_med) %>%
  select(Variable, Year, Quarter, Value)
head(df3)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Year"],"name":[2],"type":["int"],"align":["right"]},{"label":["Quarter"],"name":[3],"type":["int"],"align":["right"]},{"label":["Value"],"name":[4],"type":["int"],"align":["right"]}],"data":[{"1":"Kiselalger_med","2":"1994","3":"1","4":"21050","_rn_":"1"},{"1":"Kiselalger_med","2":"1994","3":"2","4":"1109400","_rn_":"2"},{"1":"Kiselalger_med","2":"1994","3":"3","4":"113800","_rn_":"3"},{"1":"Kiselalger_med","2":"1994","3":"4","4":"183000","_rn_":"4"},{"1":"Kiselalger_med","2":"1995","3":"1","4":"10400","_rn_":"5"},{"1":"Kiselalger_med","2":"1995","3":"2","4":"97100","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
dat <- rbind(df1, df2, df3) %>% as.data.frame()
```

## 2. Testing
### a. Test 1  

```r
test1 <- df_rivers_summ %>%
  filter(River_type %in% "Local" & Quarter %in% 2) %>%
  with(., rkt(Year, TrspTot_TOC))
print(test1)
```

```
## 
## Standard model
## Tau = 0.1396011
## Score =  49
## var(Score) =  2301
## 2-sided p-value =  0.3169952
## Theil-Sen's (MK) or seasonal/regional Kendall (SKT/RKT) slope=  5468.649
```

### b. Test 2

```r
trend_analysis <- function(variable, quarter, data = dat){
  df <- data %>%
    filter(Variable %in% variable & Quarter %in% quarter & !is.na(Value))
  result <- rkt(df$Year, df$Value)
  data.frame(Variable = variable, Quarter = quarter, 
             P = result$sl, Estimate = result$B, 
             Change_perc = 100*result$B/quantile(df$Value, 0.5), stringsAsFactors = FALSE)
}

trend_analysis("River_Local_TOC", 2)
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Quarter"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["P"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Estimate"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Change_perc"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"River_Local_TOC","2":"2","3":"0.3169952","4":"5.468649","5":"1.12333","_rn_":"50%"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## 3. Perform analysis  

```r
# ?map
df <- dat %>%
  filter(!is.na(Value)) %>%
  group_by(Variable, Quarter) %>%
  summarise(N = n()) %>%
  filter(N >= 8)

df_result_list <- 
  1:nrow(df) %>% map(~trend_analysis(df$Variable[.], df$Quarter[.]))

df_result <- bind_rows(df_result_list)
```

## 4. Show results
### a. Most extreme changes

```r
df_result %>% arrange(Change_perc) %>% tail()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Quarter"],"name":[2],"type":["int"],"align":["right"]},{"label":["P"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Estimate"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Change_perc"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"PON_Deep","2":"1","3":"3.361532e-03","4":"0.02400000","5":"2.962963","_rn_":"283"},{"1":"River_Distant_TOTP","2":"4","3":"5.004145e-02","4":"0.31418007","5":"3.122950","_rn_":"284"},{"1":"PON_Deep","2":"2","3":"4.805464e-03","4":"0.03463318","5":"3.354303","_rn_":"285"},{"1":"POC_Surface","2":"1","3":"4.511266e-06","4":"0.32426666","5":"3.496993","_rn_":"286"},{"1":"POC_Deep","2":"2","3":"5.813974e-04","4":"0.27332808","5":"3.555487","_rn_":"287"},{"1":"POC_Deep","2":"1","3":"6.225244e-05","4":"0.27142857","5":"3.961015","_rn_":"288"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

```r
df_result %>% arrange(Change_perc) %>% head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":[""],"name":["_rn_"],"type":[""],"align":["left"]},{"label":["Variable"],"name":[1],"type":["chr"],"align":["left"]},{"label":["Quarter"],"name":[2],"type":["int"],"align":["right"]},{"label":["P"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["Estimate"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["Change_perc"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"Dinoflagellater_med","2":"4","3":"1.482216e-05","4":"-5470.000","5":"-11.019339","_rn_":"1"},{"1":"Flagellater_med","2":"2","3":"2.186816e-03","4":"-187587.917","5":"-10.989333","_rn_":"2"},{"1":"Dinoflagellater_med","2":"3","3":"4.016441e-04","4":"-6813.158","5":"-9.084211","_rn_":"3"},{"1":"Total_med","2":"2","3":"3.670769e-03","4":"-199717.500","5":"-9.064101","_rn_":"4"},{"1":"Total_med","2":"1","3":"5.614106e-02","4":"-57248.222","5":"-7.943695","_rn_":"5"},{"1":"Flagellater_med","2":"1","3":"6.283882e-02","4":"-49191.213","5":"-7.684086","_rn_":"6"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

## 5. Save results

```r
write.csv(df_result, "Data_produced/06_df_result.RData",
          row.names = FALSE, quote = FALSE)
```

## 6. Some plots
### a. Dot plot theme

```r
# Code and theme from http://www.joyce-robbins.com/blog/2016/06/02/datavis-with-rdrawing-a-cleveland-dot-plot-with-ggplot2/
theme_dotplot <- theme_bw(14) +
    theme(axis.text.y = element_text(size = rel(.75)),
    	axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())
```

### b. Plot one season

```r
qrt <- 2        
df <- df_result %>% filter(Quarter == qrt) 

# create the plot
ggplot(df, aes(x = Change_perc, y = reorder(Variable, Change_perc))) +
	geom_point(color = "blue") +
	scale_x_continuous(limits = c(-12, 4),
		breaks = seq(-12, 4, 2)) +
	theme_dotplot +
	xlab("\nChange per year (%)") +
	ylab("Variable\n") +
	ggtitle(paste("Changes in quarter", qrt))
```

![](06_Timeseries_regression_Theil-Sen_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


### c. Plot for all seasons

```r
df_mean <- df_result  %>%
  group_by(Variable) %>%
  summarise(Mean_change = mean(Change_perc)) %>%
  arrange(Mean_change)

df <- df_result 
df$Variable <- factor(df$Variable, levels = df_mean$Variable)
df$Quarter <- factor(df$Quarter)

cols <- RColorBrewer::brewer.pal(4, "RdBu")[c(3,4,1,2)]
# create the plot
ggplot(df, aes(x = Change_perc, y = Variable, color = Quarter)) +
  geom_vline(xintercept = 0) +
	geom_point(size = 2) +
  # scale_color_brewer(palette = "RdBu", direction = -1) +
  scale_color_manual(values = cols) +
	scale_x_continuous(limits = c(-12, 4), breaks = seq(-12, 4, 2)) +
	theme_dotplot +
	xlab("\nChange per year (%)") +
	ylab("Variable\n")
```

![](06_Timeseries_regression_Theil-Sen_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

