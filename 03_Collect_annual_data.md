---
title: "03 Collect data for time trend analysis"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_depth: 3
    toc_float: true
    code_folding: hide
    df_print: paged   
---


## 0. Libraries

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 2.2.1     v purrr   0.2.4
## v tibble  1.4.2     v dplyr   0.7.6
## v tidyr   0.8.1     v stringr 1.3.1
## v readr   1.1.1     v forcats 0.3.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(readxl)
library(broom)
# library(pander)
```

## 1. Folders and files    
Content of folders (see code)  

```r
dir("Datasett")
```

```
## [1] "Bløtbunn"              "Eksempel datafil.xlsx" "hardbunn_kopi"        
## [4] "hydrografi"            "Plankton"              "River data (from OKA)"
```

```r
dir("Datasett/River data (from OKA)")
```

```
## [1] "Annual mean flows"                   
## [2] "Concentrations (individual samples)" 
## [3] "Monthly flow-weighted concentrations"
## [4] "Monthly loads"
```

```r
dir("Datasett/hydrografi")
```

```
## [1] "Arendal_allvars_1990_2016.csv"   "Arendal_allvars_1990_2016.Rdata"
## [3] "R scripts"                       "Rådata"
```

```r
dir("Datasett/Bløtbunn")
```

```
## [1] "Beskrivelse av bløtbunndata.docx"                   
## [2] "Klimaoverblikk bløtbunn_data til Helene og Dag.xlsx"
```

```r
dir("Datasett/hardbunn_kopi")
```

```
## [1] "HBanalysesett.csv" "other docs"        "r workspace"
```

```r
dir("Datasett/Plankton")
```

```
## [1] "Beskrivelse av planktondata.docx" "Planteplankton Arendal.xlsx"
```

## 2. River data   
Content of folders (see code)

```r
dir("Datasett/River data (from OKA)/Annual mean flows")
```

```
## [1] "Mean annual flow.xlsx"
```

```r
dir("Datasett/River data (from OKA)/Monthly loads")
```

```
## [1] "Gjerstadelva_Nidelva_monthly loads.xlsx"
## [2] "RIDx5_monthly loads.xlsx"               
## [3] "Storelva_monthly loads.xlsx"
```

```r
dir("Datasett/River data (from OKA)/Concentrations (individual samples)")
```

```
## [1] "Gjerstadelva_Nidelva_conc.xlsx" "RIDx5_conc.xlsx"               
## [3] "Storelva_conc.xlsx"
```

```r
dir("Datasett/River data (from OKA)/Monthly flow-weighted concentrations")
```

```
## [1] "Gjerstadelva_Nidelva_flow-weighted mean_month.xlsx"
## [2] "RIDx5_flow-weighted mean_month.xlsx"               
## [3] "Storelva_flow-weighted mean_month.xlsx"
```

### a. Data of river loads

```r
df1 <- read_excel("Datasett/River data (from OKA)/Monthly loads/Storelva_monthly loads.xlsx")  

df2 <- read_excel("Datasett/River data (from OKA)/Monthly loads/Gjerstadelva_Nidelva_monthly loads.xlsx")  

df3 <- read_excel("Datasett/River data (from OKA)/Monthly loads/RIDx5_monthly loads.xlsx")  

# head(df1, 3)
# head(df2, 3)
# head(df3, 3)

colnames(df1) %>% dput()
```

```
## c("Station ID", "Station Code", "Station name", "Year", "Month", 
## "TrspTot TOTN", "TrspTot NO3-N", "TrspTot NH4-N", "TrspTot TOTP", 
## "TrspTot TOC", "TrspTot ALK", "TrspTot Ca", "DisTot")
```

```r
colnames(df2) %>% dput()
```

```
## c("Station ID", "Station Code", "Station name", "Year", "Month", 
## "TrspTot TOTN", "TrspTot NO3-N", "TrspTot NH4-N", "TrspTot TOTP", 
## "TrspTot TOC", "TrspTot ALK", "TrspTot Ca", "DisTot")
```

```r
colnames(df3) %>% dput()
```

```
## c("Station ID", "Station Code", "Station name", "Year", "Month", 
## "TrspTot TOTN", "TrspTot NO3-N", "TrspTot NH4-N", "TrspTot TOTP", 
## "TrspTot PO4-P", "TrspTot TOC", "TrspTot SPM", "TrspTot SiO2", 
## "DisTot")
```

```r
df <- bind_rows(df1[-1,], df2[-1,], df3[-1,])

# colnames(df) %>% dput()
vars <- c("TrspTot TOTN", "TrspTot NO3-N", "TrspTot NH4-N", "TrspTot TOTP", 
          "TrspTot TOC", "TrspTot ALK", "TrspTot Ca", "DisTot")
for (var in vars)
  df[[var]] <- as.numeric(df[[var]])
df$Time <- with(df, lubridate::ymd(paste(Year, Month, "15")))

# Add "_" in column names (TrspTot Ca -> TrspTot_Ca)
colnames(df) <- sub(" ", "_", colnames(df), fixed = TRUE)

tb <- df %>% 
  gather("Variable", Value, TrspTot_TOTN:DisTot) %>%
  filter(!is.na(Value)) %>%
  xtabs(~Station_name + Variable, .)
tb
```

```
##                                 Variable
## Station_name                     DisTot TrspTot_ALK TrspTot_Ca
##   Drammenselva                      324           0          0
##   Glomma ved Sarpsfoss              324           0          0
##   Nidelva ovenf. Rygene             324         311        311
##   Numedalslågen                     324           0          0
##   Otra                              324           0          0
##   Skienselva                        324           0          0
##   Storelva v/ Nes verk              312         232        276
##   Søndeledelva v. Søndeleddammen    324         324        324
##                                 Variable
## Station_name                     TrspTot_NH4-N TrspTot_NO3-N TrspTot_TOC
##   Drammenselva                             299           324         303
##   Glomma ved Sarpsfoss                     299           324         324
##   Nidelva ovenf. Rygene                     17           311         311
##   Numedalslågen                            300           324         304
##   Otra                                     300           324         303
##   Skienselva                               299           324         303
##   Storelva v/ Nes verk                      17           276         276
##   Søndeledelva v. Søndeleddammen           324           324         324
##                                 Variable
## Station_name                     TrspTot_TOTN TrspTot_TOTP
##   Drammenselva                            324          324
##   Glomma ved Sarpsfoss                    324          324
##   Nidelva ovenf. Rygene                   311          271
##   Numedalslågen                           324          324
##   Otra                                    324          324
##   Skienselva                              324          324
##   Storelva v/ Nes verk                    276          275
##   Søndeledelva v. Søndeleddammen          324          227
```

### b, Plot seasonal cycle

```r
gg <- df %>%
  gather("Variable", "Value",  TrspTot_TOTN:DisTot) %>%
  group_by(Station_name, Variable, Month) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE)) %>%
  ggplot(., aes(Month, Mean)) + 
    geom_ribbon(aes(ymin = Min, ymax = Max), fill = "lightgreen") +
    geom_line() + geom_point() +
    facet_grid(Variable~Station_name, scales = "free_y")
gg
```

```
## Warning: Removed 120 rows containing missing values (geom_point).
```

![](03_Collect_annual_data_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### c. Quarterly means of all variables

```r
df2 <- df %>%
  mutate(Quarter = case_when(
    Month %in% 1:3 ~ 1,
    Month %in% 4:6 ~ 2,
    Month %in% 7:9 ~ 3,
    Month %in% 10:12 ~ 4
  )) %>%
  gather("Variable", "Value",  TrspTot_TOTN:DisTot) %>%
  group_by(Station_name, Variable, Year, Quarter) %>%
  summarise(Mean = mean(Value, na.rm = TRUE), Min = min(Value, na.rm = TRUE), Max = max(Value, na.rm = TRUE))
```


### d. Make quarterly site-specific variables

```r
df3 <- df2 %>%
  unite("VarQuarter", Variable, Quarter) %>%
  mutate(Station_short = substr(Station_name, 1, 4)) %>%
  unite("StVarQuarter", Station_short, VarQuarter) %>%
  arrange(StVarQuarter, Year)
```

### e. Test regression of every station / variable / quarter

```r
get_reg_coef <- function(df) {
  lm <- lm(Mean ~ Year, data = df)
  result <- subset(tidy(lm), term == "Year")
  # browser()
  result$Meanvalue <- mean(df$Mean)
  result$Perc_change_per_decade <- with(result, statistic/Meanvalue*10*100)
  result
  }

# Test
# get_reg_coef(df3 %>% filter(StVarQuarter %in% "Dram_DisTot_2" & !is.na(Mean)))


m_yeareffect <- df3 %>%
  filter(!is.na(Mean)) %>%
  group_by(StVarQuarter) %>%
  mutate(N_year = n()) %>%
  filter(N_year >= 10) %>%
  split(.$StVarQuarter) %>%
  map_df(~get_reg_coef(.), .id = "Variable")

ggplot(m_yeareffect, aes(Variable, statistic, fill = factor(p.value < 0.05))) +
         geom_col() +
  theme(axis.title.x = element_text(size = 4)) +
  coord_flip()
```

![](03_Collect_annual_data_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
```

### f. Regression results, alternative plot

```r
# Helper functions
substr_right <- function(txt, n){              # n = no. of characters to extract from the right
  substr(txt, nchar(txt) - n + 1, nchar(txt))
}
substr_left <- function(txt, n_to_remove){     # n_to_remove = no. of characters to remove from the right
  substr(txt, 1, nchar(txt) - n_to_remove)
}

substr_mid <- function(txt, n_start, n_to_remove){ 
  substr(txt, n_start, nchar(txt) - n_to_remove)
}

# Sorts a character vector as the first 
sort_char_as_num <- function(txt){
  num <- stringr::str_extract(txt, "([^[[:blank:]]]+)")  %>% as.numeric()
  txt[order(num)]
}

# Test
# sort(c("10 %", "5 %", "20 %"))              # ordinary sort:    "10 %" "20 %" "5 %"
# sort_char_as_num(c("10 %", "5 %", "20 %"))  # sort_char_as_num: "5 %"  "10 %" "20 %"

factorlevels_from_num <- function(txt){
  factor(txt, levels = sort_char_as_num(unique(txt)))
  }

# Test
# substr_right("abcd", 2)
# substr_left(c("abcd", "abcdefgh"), 2)
# substr_mid(c("abcd", "abcdefgh"), 2, 2)

# quantile(m_yeareffect$Perc_change_per_decade)  %>% round(1)
# quantile(m_yeareffect$Perc_change_per_decade %>% abs())  %>% round(1)

# Make new data set as we need Quarter as a separate variable in the plot.
# Also we want to plot in the order of Variable_River, not River_Variable (keep each variable together in plot)
df <- m_yeareffect %>%
  mutate(Quarter = substr_right(Variable, 1), 
         River_Variable = substr_left(Variable, 2),
         Variable2 = substr_mid(Variable, 6, 2),
         Variable_River = paste0(Variable2, "_", substr(Variable, 1, 4))) %>%
  mutate(Slope_category = 
           case_when(statistic < 0 & p.value < 0.05 ~ "Decrease (P < 0.05)",
                     statistic < 0 & p.value > 0.05 ~ "Decrease (P > 0.05)",
                     statistic > 0 & p.value > 0.05 ~ "Increase (P > 0.05)",
                     statistic > 0 & p.value < 0.05 ~ "Increase (P < 0.05)")
  ) %>%
  mutate(Perc_change_category = 
           case_when(Perc_change_per_decade < 0.5 ~ "0 - 0.5 %",
                     Perc_change_per_decade >= 0.5 &  Perc_change_per_decade < 1 ~ "0.5 - 1 %",
                     Perc_change_per_decade >= 1 &  Perc_change_per_decade < 2 ~ "1 - 2 %",
                     Perc_change_per_decade >= 2 &  Perc_change_per_decade < 10 ~ "2 - 10 %",
                     Perc_change_per_decade >= 10 &  Perc_change_per_decade < 20 ~ "10 - 20 %") 
  )  %>%
  mutate(Perc_change_category = factorlevels_from_num(Perc_change_category))


xtabs(~Perc_change_category, df)
```

```
## Perc_change_category
## 0 - 0.5 % 0.5 - 1 %   1 - 2 %  2 - 10 % 10 - 20 % 
##       194         8         3         1         2
```

```r
gg1 <- ggplot(df, aes(Quarter, Variable_River, size = abs(Perc_change_per_decade), color = Slope_category)) +
  geom_point() +
  scale_size_continuous("% change per decade", breaks = c(0, 1, 2, 5, 10, 20)) +
  scale_color_manual("Statistical significance", values = c("darkblue", "lightblue", "pink", "red3"))
# gg1
  
gg2 <- ggplot(df, aes(Quarter, Variable_River, size = Perc_change_category, color = Slope_category)) +
  geom_point() +
  scale_size_manual("% change per decade", values = 3:8) +
  scale_color_manual("Statistical significance", values = c("darkblue", "lightblue", "pink", "red3"))
gg2
```

![](03_Collect_annual_data_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


