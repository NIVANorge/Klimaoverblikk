# Klimaoverblikk
Data merging and analyses for the Klimaoverblikk-opsjon

### Usage  
*.Rmd files contains code  
*.md files contains code and results (tables/plots)   
  
### Scripts  
[Script 01](master/01_Check_data.md) - Checking data, some plots   
[Script 02](02_Organise_soft_bottom_for_PCA.md) - Code for organising soft-bottom data for PCA and other multivariate stuff  
[Script 03](03_Collect_annual_data.md) - Test script, not actually used later  
[Script 04](04_Get_plankton_bloom_data.md) - Finds spring bloom period (from Chl a) and extracts plankton abundance data from this period for PCA/DCA  
[Script 05](05_Annual_data_all.md) Test script (see 05b for the script actually used)  
[Script 05b](05b_Annual_and_quarterly_data_all.md) - Produces summarized data by year and quarter, written to folder   [Data_produced](Data_produced)  
[Script 06](06_Timeseries_regression_Theil-Sen.md) - Runs Theil-Sen regression for all annual and quarterly time series and produces Cleceland dot plots showing trends (see plot below) 
[Script 07](07_Plots_annual.md) - Mass production of time series plots of annual data with Theil-Sen regression lines. Stored in [Figures_07](Figures_07)  
[Script 08](08_Regression.md) - Runs multiple regression models (w/ backwards selection) for some hydrography and plankton variables  
  
![Theil-Sen trends](https:///06_Timeseries_regression_Theil-Sen_files/figure-html/unnamed-chunk-15-1.png)
  