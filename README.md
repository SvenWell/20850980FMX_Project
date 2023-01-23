# Purpose

This README is here to describe my analysis with the accompanying code.

This analysis will look at the dynamic conditional correlations of the
sector returns of the SWIX. The time period will start at January 1 2014
and end at the most recent date October 29 2021. The data will then be
subset for the month periods that South Africa experienced loadshedding.
The aim of this paper is to see if there is a difference in sector
dynamics between the Financials, Industrials and Resources for the SWIX.
The DCC-GARCH model will be used to model the dynamic correlation
structure of these returns during these periods of loadshedding and
allow us to study the impact of loadshedding on the market.

First we will load necessary packages and source our code files.

``` r
pacman::p_load(modelsummary, gt, knitr, kableExtra, tidyverse, lubridate, tinytex, rmarkdown, crypto2, quantmod, MTS, rmgarch, rugarch, tbl2xts, fmxdat, ggthemes, summarytools, vars, tseries, pander, cowplot, qpcR, tidyquant, stargazer, data.table)


list.files('code/', full.names = T, recursive = T) %>% as.list() %>% walk(~source(.))
```

Below is the code used to create this file and the Texevier document.

``` r
CHOSEN_LOCATION <- "/Users/svenwellmann/Desktop/Masters Semester 2/Financial Econometrics/FMX_Project/"
fmxdat::make_project(FilePath = glue::glue("{CHOSEN_LOCATION}"), ProjNam = "20850980Project", Mac = TRUE)

Texevier::create_template(directory = glue::glue("{CHOSEN_LOCATION}"), template_name = "Paper")
```

# Get Data

First I get the tickers for the top 40 from the data given for the
practical exam. I then use these tickers to pull data more recent data
using the tidyquant package. The ticker names have to be cleaned and
some errors are fixed to get as much data as possible. This is calling
the tq_get() function on the cleaned ticker values from the top 40.
There are tickers that data was not received for. These tickers either
have names that have been changed or their tickers do not correspond to
the tickers that are retrieved through tidyquant from Yahoo Finance. I
then manually change these ticker names and pull the data again.

``` r
T40 <- read_rds("data/T40.rds")

T40 %>% pull(date) %>%  max()

unique_tickers <- T40 %>% pull(Tickers) %>% unique() %>% data.frame()
colnames(unique_tickers) <- "tick"

unique_tickers_clean <- unique_tickers %>% mutate(Tickers = gsub(" SJ Equity", ".JO", tick)) %>% pull(Tickers)

# Download historical data for tickers for the JSE into R
# stock_data <- tq_get(unique_tickers_clean, from = "2014-01-01", to = "2022-12-31", get = "stock.prices", quiet = TRUE)

# stock_data_tickers <- stock_data %>% pull(symbol) %>% unique()

# unique_tickers_clean[!unique_tickers_clean %in% stock_data_tickers] 

# BHP.JO = BHG.JO
# SAB.JO = SZK.JO
# ITU.JO = ITE.JO
# IPL.JO = 
# LGL.JO = 
# LON.JO = 
# MND.JO = GND.JO
# REI.JO = 
# LBH.JO = LBR.JO
# MSM.JO = 
# ASR.JO = ADR.JO
# MDC.JO = 
# PSG.JO = KST.JO
# NEP.JO = 
# NHM.JO =
```

Here I have appended the missing tickers that I found. I get the data
daily from 1 January 2014 to 31 December 2022. These dates were chosen
as they correspond to periods with loadshedding. I then get the sectors
of the Stocks and add the sectors of the changed variables.

``` r
unique_tickers_clean <- c(unique_tickers_clean, "BHG.JO", "SZK.JO", "ITE.JO", "GND.JO", "LBR.JO", "ADR.JO", "KST.JO")

stock_data <- tq_get(unique_tickers_clean, from = "2014-01-01", to = "2022-12-31", get = "stock.prices", quiet = TRUE)

stock_data %>% pull(symbol) %>% unique

stock_data <- stock_data %>% dplyr::select(symbol, date, close)
colnames(stock_data) <- c("Tickers", "date", "close")

stocks_sectors <- left_join(x = stock_data, y = T40 %>% dplyr:::select(Tickers, Sector) %>% unique %>% mutate(Tickers = gsub(" SJ Equity", ".JO", Tickers)))


# Add the sector to the changed variables
stocks_sectors <- stocks_sectors %>% 
    mutate(Sector = case_when(Tickers == "BHG.JO" & is.na(Sector) ~ "Resources",
                              Tickers == "SZK.JO" & is.na(Sector) ~ "Industrials",
                              Tickers == "ITE.JO" & is.na(Sector) ~ "Financials",
                              Tickers == "GND.JO" & is.na(Sector) ~ "Resources",
                              Tickers == "LBR.JO" & is.na(Sector) ~ "Financials",
                              Tickers == "ADR.JO" & is.na(Sector) ~ "Resources",
                              Tickers == "KST.JO" & is.na(Sector) ~ "Financials",
                              TRUE ~ Sector))

#adr.jo <- stocks_sectors %>% filter(Tickers %in% "ADR.JO") %>% arrange(date) %>% unique
#gnd.jo <- stocks_sectors %>% filter(Tickers %in% "GND.JO") %>% arrange(date) %>% unique

stocks_sectors <- stocks_sectors %>% 
  group_by(Tickers, Sector) %>% 
  filter(!any(Tickers == "REM.JO" & Sector == "Financials")) %>% 
  filter(!any(Tickers == "MNP.JO" & Sector == "Industrials")) 
#%>% 
 # filter(!any(Tickers == "ADR.JO")) %>% 
  #filter(!any(Tickers == "GND.JO")) 

#stocks_sectors <- rbind(stocks_sectors, adr.jo, gnd.jo)

# length(stocks_sectors$Tickers) -length(stocks_sectors %>% distinct(Tickers, date, .keep_all = TRUE) %>% pull(Tickers))

stocks_sectors <- stocks_sectors %>% distinct(Tickers, date, .keep_all = TRUE)
```

I have then used a few sources to identify the periods of loadshedding
in South Africa. I have identified periods: \* November 2014 - March of
2015 \* February 2019 - March 2019 \* December 2019 - March 2020 \*
March 2021 - June 2021 \* October 2021 - November 2021 \* February
2022 - April 2022 \* June 2022 - December 2022

``` r
stocks_loadshed <- rbind(
stocks_sectors %>% filter(date >= "2014-11-01" & date <= "2015-3-31"),
stocks_sectors %>% filter(date >= "2019-02-01" & date <= "2019-3-31"),
stocks_sectors %>% filter(date >= "2019-12-01" & date <= "2020-3-31"),
stocks_sectors %>% filter(date >= "2021-3-01" & date <= "2021-6-30"),
stocks_sectors %>% filter(date >= "2021-9-01" & date <= "2021-11-30"),
stocks_sectors %>% filter(date >= "2022-2-01" & date <= "2022-4-30"),
stocks_sectors %>% filter(date >= "2022-6-01" & date <= "2022-12-31"))

stocks_no_loadshed <- stocks_sectors %>% filter(!date %in% (stocks_loadshed %>% pull(date)))
```

I then look at the stocks for NA values. There are only 2 stocks with NA
values. OML.JO and SOL.JO. OML.JO has 680 missing values out of 1706.
The stock has no information after 26 of June 2018. Due to these reasons
the stock is removed form the study.

``` r
stocks_no_loadshed %>% group_by(Tickers) %>% summarise(na_count = sum(is.na(close))) %>% filter(na_count>0)

stocks_loadshed <- stocks_loadshed %>% filter(!Tickers %in% "OML.JO")
stocks_no_loadshed <- stocks_no_loadshed %>% filter(!Tickers %in% "OML.JO")
```

Now I get the returns for all the stocks and impute any missing returns
based on the collective distribution.

``` r
loadshed_returns <- stocks_loadshed %>% 
    arrange(date) %>% 
    group_by(Tickers) %>% 
    mutate(simpleret = close/ lag(close)) %>% 
    mutate(dlogret = log(close) - log(lag(close))) %>% 
    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>% 
    filter(date>min(date))

no_loadshed_returns <- stocks_no_loadshed %>% 
    arrange(date) %>% 
    group_by(Tickers) %>% 
    mutate(simpleret = close/ lag(close)) %>% 
    mutate(dlogret = log(close) - log(lag(close))) %>% 
    mutate(scaledret = (dlogret - mean(dlogret, na.rm = T))) %>% 
    filter(date>min(date))

xts_loadshed_return <- loadshed_returns %>% tbl_xts(., cols_to_xts = "dlogret", spread_by = "Tickers")
xts_no_loadshed_return <- no_loadshed_returns %>% tbl_xts(., cols_to_xts = "dlogret", spread_by = "Tickers")

loadshed_returns <- impute_missing_returns(loadshed_returns, impute_returns_method = "Drawn_Distribution_Collective")


stocks_sectors %>% group_by(Tickers) %>% summarise(first_date = min(date))
```

# Doing the analysis with data given during the practical test

Unfortunately I could not scrape the SWIX portfolio weights for the
updated period and have had to use the data given. This leaves us with a
slightly smaller sample and also does not include the months of
significant loadshedding in 2022.

``` r
T40 <- read_rds("data/T40.rds")

sectors <- T40 %>% pull(Sector) %>% unique()
sector_return <- list()
for(i in 1:length(sectors)){
    # Loop through sectors and calculate returns and cumulative returns
    sector_return[[i]] <- portfolio_return_function(T40, sector = sectors[i]) %>% group_by(Portfolio) %>% 
      mutate(cumreturn_Rand = (cumprod(1 + Returns))) %>% # Start at 1
      mutate(cumreturn_Rand = cumreturn_Rand/dplyr:::first(cumreturn_Rand)) %>% 
      mutate(Sector = sectors[i])
}
# Rename tibbles
names(sector_return) <- sectors
# Combine Dataframes
sectors_cum_return <- rbind(sector_return[[1]], sector_return[[2]], sector_return[[3]]) %>% arrange(date)
```

Below we look at the different return structure of the ALSI and SWIX for
their respective sectors. We observe that the Industrials have seen the
largest cumulative growth of all sectors. Resources have seen minimal
cumulative growth, with majority of the growth being after 2017.

``` r
sectors_cum_return_plot <- sectors_cum_return %>% 
    ggplot() +
    geom_line(aes(date, cumreturn_Rand, colour = Portfolio), alpha = 0.8) + facet_wrap(~Sector) + fmxdat::fmx_cols() + 
    labs(title = "Cumulative Returns per Sector for ALSI and SWIX", y = "Cumulative Returns", x = "") + 
    fmxdat::theme_fmx(title.size = ggpts(25))
sectors_cum_return_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-9-1.png)

## Weights plot

To decide between the ALSI and SWIX I visualise the weight compositions
of the indices.

-   ALSI (J200)

``` r
weights_plot(T40, "ALSI")
```

![](README_files/figure-markdown_github/unnamed-chunk-10-1.png)

-   SWIX (J400)

``` r
weights_plot(T40, "SWIX")
```

![](README_files/figure-markdown_github/unnamed-chunk-11-1.png)

From the plots it can be seen that since 2020 the ALSI lowered their
weighting of Financial stocks and increased their weighting of resource
stocks in comparison to the SWIX. The SWIX also has less volatility in
weight contribution. Because of this the SWIX is chosen as the
representative portfolio.

The portfolio returns are then stratified into periods of loadshedding
and periods without loadshedding.

``` r
sectors_cum_return <- sectors_cum_return %>% filter(Portfolio == 'J400') # SWIX

swix_loadshed <- rbind(
sectors_cum_return %>% filter(date >= "2014-11-01" & date <= "2015-3-31"),
sectors_cum_return %>% filter(date >= "2019-02-01" & date <= "2019-3-31"),
sectors_cum_return %>% filter(date >= "2019-12-01" & date <= "2020-3-31"),
sectors_cum_return %>% filter(date >= "2021-3-01" & date <= "2021-6-30"),
sectors_cum_return %>% filter(date >= "2021-9-01" & date <= "2021-11-30"),
sectors_cum_return %>% filter(date >= "2022-2-01" & date <= "2022-4-30"),
sectors_cum_return %>% filter(date >= "2022-6-01" & date <= "2022-12-31"))

swix_no_loadshed <- sectors_cum_return %>% filter(!date %in% (swix_loadshed %>% pull(date)))

xts_swix_return <- sectors_cum_return %>% tbl_xts(., cols_to_xts = "Returns", spread_by = "Sector") 

xts_swix_return_loadshed <- swix_loadshed %>% tbl_xts(., cols_to_xts = "Returns", spread_by = "Sector") 
xts_swix_return_no_loadshed <- swix_no_loadshed %>% tbl_xts(., cols_to_xts = "Returns", spread_by = "Sector") 
```

The Log returns of all sectors across the entire period are the plotted.

``` r
log_returns_plot <- sectors_cum_return %>% mutate(Sector = factor(Sector, levels = c("Financials",  "Industrials", "Resources"))) %>% 
  ggplot() + geom_line(aes(x = date, y = Returns, colour = Sector, alpha = 1)) + 
facet_wrap(~Sector, scales = "free_y", nrow = 3) + 
guides(alpha = "none") + 
fmxdat::theme_fmx() +
labs(x = "", y = "", subtitle = " ") +
theme(legend.position = "none") +
scale_color_hue(l = 20) + 
scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 years") + 
theme(axis.text = element_text(size = 7))

log_returns_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-13-1.png)

# Fit DCC models

The DCC model is then fit over the whole period, the period of
loadshedding and the period without loadshedding. The gjrGARCH model is
used for this analysis. The residuals of the GARCH model are then used
in the DCC model.

``` r
# Using the rugarch package, let's specify our own univariate
# functions to be used in the dcc process:

# Step 1: Give the specifications to be used first:

# A) Univariate GARCH specifications:
uspec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), distribution.model = "sstd")
# B) Repeat uspec n times. This specification should be
# self-explanatory...
multi_univ_garch_spec <- multispec(replicate(ncol(xts_swix_return), uspec))
multi_univ_garch_spec_ls <- multispec(replicate(ncol(xts_swix_return_loadshed), uspec))
multi_univ_garch_spec_nls <- multispec(replicate(ncol(xts_swix_return_no_loadshed), uspec))

# Right, so now every series will have a GJR Garch univariate
# specification. (see ?ugarchspec for other options...)

# C) DCC Specs
spec.dcc = dccspec(multi_univ_garch_spec, dccOrder = c(1, 1), distribution = "mvnorm", lag.criterion = c("AIC", "HQ", "SC", "FPE")[1], model = c("DCC", "aDCC")[1])
spec.dcc_ls = dccspec(multi_univ_garch_spec_ls, dccOrder = c(1, 1), distribution = "mvnorm", lag.criterion = c("AIC", "HQ", "SC", "FPE")[1], model = c("DCC", "aDCC")[1])
spec.dcc_nls = dccspec(multi_univ_garch_spec_nls, dccOrder = c(1, 1), distribution = "mvnorm", lag.criterion = c("AIC", "HQ", "SC", "FPE")[1], model = c("DCC", "aDCC")[1])

# D) Enable clustering for speed:
cl = makePSOCKcluster(10)

# ------------------------ Step 2: The specs are now saved.
# Let's now build our DCC models...  ------------------------

# First, fit the univariate series for each column:
multf = multifit(multi_univ_garch_spec, xts_swix_return, cluster = cl)
multf_ls = multifit(multi_univ_garch_spec_ls, xts_swix_return_loadshed, cluster = cl)
multf_nls = multifit(multi_univ_garch_spec_nls, xts_swix_return_no_loadshed, cluster = cl)

# Now we can use multf to estimate the dcc model using our
# dcc.spec:
fit.dcc = dccfit(spec.dcc, data = xts_swix_return, solver = "gosolnp", cluster = cl, fit.control = list(eval.se = TRUE), fit = multf)
fit.dcc_ls = dccfit(spec.dcc_ls, data = xts_swix_return_loadshed, solver = "gosolnp", cluster = cl, fit.control = list(eval.se = TRUE), fit = multf_ls)
fit.dcc_nls = dccfit(spec.dcc_nls, data = xts_swix_return_no_loadshed, solver = "gosolnp", cluster = cl, fit.control = list(eval.se = TRUE), fit = multf_nls)

# And that is our DCC fitted model!

# We can now test the model's fit as follows: Let's use the
# covariance matrices to test the adequacy of MV model in
# fitting mean residual processes:
RcovList <- rcov(fit.dcc)  # This is now a list of the monthly covariances of our DCC model series.
RcovList_ls <- rcov(fit.dcc_ls)
RcovList_nls <- rcov(fit.dcc_nls)

covmat = matrix(RcovList, nrow(xts_swix_return), ncol(xts_swix_return) * ncol(xts_swix_return), byrow = TRUE)
covmat_ls = matrix(RcovList, nrow(xts_swix_return_loadshed), ncol(xts_swix_return_loadshed) * ncol(xts_swix_return_loadshed), byrow = TRUE)
covmat_nls = matrix(RcovList, nrow(xts_swix_return_no_loadshed), ncol(xts_swix_return_no_loadshed) * ncol(xts_swix_return_no_loadshed), byrow = TRUE)

mc1 = MCHdiag(xts_swix_return, covmat)
```

    ## Test results:  
    ## Q(m) of et: 
    ## Test and p-value:  13.84817 0.1800305 
    ## Rank-based test: 
    ## Test and p-value:  9.318122 0.5022021 
    ## Qk(m) of epsilon_t: 
    ## Test and p-value:  122.7352 0.01245581 
    ## Robust Qk(m):  
    ## Test and p-value:  80.72197 0.747515

``` r
mc1_ls = MCHdiag(xts_swix_return_loadshed, covmat_ls)
```

    ## Test results:  
    ## Q(m) of et: 
    ## Test and p-value:  340.7814 0 
    ## Rank-based test: 
    ## Test and p-value:  93.51436 1.110223e-15 
    ## Qk(m) of epsilon_t: 
    ## Test and p-value:  598.907 0 
    ## Robust Qk(m):  
    ## Test and p-value:  137.6813 0.0009137271

``` r
mc1_nls = MCHdiag(xts_swix_return_no_loadshed, covmat_nls)
```

    ## Test results:  
    ## Q(m) of et: 
    ## Test and p-value:  1216.349 0 
    ## Rank-based test: 
    ## Test and p-value:  581.763 0 
    ## Qk(m) of epsilon_t: 
    ## Test and p-value:  1703.145 0 
    ## Robust Qk(m):  
    ## Test and p-value:  318.3751 0

The results have to be pulled out of the data using the following code.
This will also rename our dynamic conditional correlations.

``` r
dcc.time.var.cor <- rcor(fit.dcc)
dcc.time.var.cor_ls <- rcor(fit.dcc_ls)
dcc.time.var.cor_nls <- rcor(fit.dcc_nls)

dcc.time.var.cor <- aperm(dcc.time.var.cor, c(3, 2, 1))
dcc.time.var.cor_ls <- aperm(dcc.time.var.cor_ls, c(3, 2, 1))
dcc.time.var.cor_nls <- aperm(dcc.time.var.cor_nls, c(3, 2, 1))

dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)
dim(dcc.time.var.cor_ls) <- c(nrow(dcc.time.var.cor_ls), ncol(dcc.time.var.cor_ls)^2)
dim(dcc.time.var.cor_nls) <- c(nrow(dcc.time.var.cor_nls), ncol(dcc.time.var.cor_nls)^2)

dcc.time.var.cor <- renamingdcc(ReturnSeries = xts_swix_return, DCC.TV.Cor = dcc.time.var.cor)
```

    ## Warning: `tbl_df()` was deprecated in dplyr 1.0.0.
    ## ℹ Please use `tibble::as_tibble()` instead.

``` r
dcc.time.var.cor_ls <- renamingdcc(ReturnSeries = xts_swix_return_loadshed, DCC.TV.Cor = dcc.time.var.cor_ls)
dcc.time.var.cor_nls <- renamingdcc(ReturnSeries = xts_swix_return_no_loadshed, DCC.TV.Cor = dcc.time.var.cor_nls)
```

## Statistics

First we asses the fit of the DCC model on the data.

``` r
star_fit.dcc <- fit.dcc@mfit$matcoef[rownames(fit.dcc@mfit$matcoef) %like% 'omega|alpha1|beta1|dcca1|dccb1',, drop = FALSE] #%>% data.frame()

# \\label{dccfitfull}

stargazer(star_fit.dcc,
          type = "text",
          title = "DCC GARCH Model Fit",
          ci = TRUE,
          ci.level = 0.95,
          p.auto = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align = TRUE)
```

# DCC GARCH Model Fit

## Estimate Std. Error t value Pr(\> \| t\| )

\[Resources\].omega 0.00000 0.00001 0.304 0.761  
\[Resources\].alpha1 0.005 0.033 0.147 0.883  
\[Resources\].beta1 0.947 0.050 18.749 0  
\[Industrials\].omega 0.00000 0.00000 1.755 0.079  
\[Industrials\].alpha1 0.017 0.009 1.822 0.068  
\[Industrials\].beta1 0.907 0.017 54.781 0  
\[Financials\].omega 0.00000 0.00000 0.966 0.334  
\[Financials\].alpha1 0.032 0.022 1.492 0.136  
\[Financials\].beta1 0.907 0.033 27.601 0  
\[Joint\]dcca1 0.039 0.005 7.807 0  
\[Joint\]dccb1 0.949 0.008 123.276 0  
————————————————————

``` r
star_fit.dcc_ls <- fit.dcc_ls@mfit$matcoef[rownames(fit.dcc_ls@mfit$matcoef) %like% 'omega|alpha1|beta1|dcca1|dccb1',, drop = FALSE]

# \\label{dccfitls}

stargazer(star_fit.dcc_ls,
          type = "text",
          title = "Loadshedding DCC GARCH Model Fit",
          ci = TRUE,
          ci.level = 0.95,
          p.auto = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align = TRUE) 
```

# Loadshedding DCC GARCH Model Fit

## Estimate Std. Error t value Pr(\> \| t\| )

\[Resources\].omega 0.00004 0.00003 1.399 0.162  
\[Resources\].alpha1 0.00000 0.101 0.00000 1.000  
\[Resources\].beta1 0.791 0.161 4.907 0.00000  
\[Industrials\].omega 0.00001 0.00000 24.162 0  
\[Industrials\].alpha1 0.00000 0.028 0.00000 1.000  
\[Industrials\].beta1 0.825 0.023 35.958 0  
\[Financials\].omega 0.00003 0.00001 3.121 0.002  
\[Financials\].alpha1 0.018 0.058 0.306 0.760  
\[Financials\].beta1 0.672 0.071 9.408 0  
\[Joint\]dcca1 0.050 0.015 3.413 0.001  
\[Joint\]dccb1 0.895 0.032 27.574 0  
————————————————————

``` r
star_fit.dcc_nls <- fit.dcc_nls@mfit$matcoef[rownames(fit.dcc_nls@mfit$matcoef) %like% 'omega|alpha1|beta1|dcca1|dccb1',, drop = FALSE]

# \\label{dccfitnls}

stargazer(star_fit.dcc_nls,
          type = "text",
          title = "No Loadshedding DCC GARCH Model Fit",
          ci = TRUE,
          ci.level = 0.95,
          p.auto = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          align = TRUE)
```

# No Loadshedding DCC GARCH Model Fit

## Estimate Std. Error t value Pr(\> \| t\| )

\[Resources\].omega 0.00000 0.00000 0.437 0.662  
\[Resources\].alpha1 0.026 0.021 1.256 0.209  
\[Resources\].beta1 0.934 0.035 26.758 0  
\[Industrials\].omega 0.00000 0.00001 0.365 0.715  
\[Industrials\].alpha1 0.023 0.036 0.641 0.521  
\[Industrials\].beta1 0.912 0.074 12.253 0  
\[Financials\].omega 0.00000 0.00001 0.369 0.712  
\[Financials\].alpha1 0.039 0.051 0.771 0.441  
\[Financials\].beta1 0.908 0.075 12.101 0  
\[Joint\]dcca1 0.036 0.005 6.746 0  
\[Joint\]dccb1 0.954 0.008 120.613 0  
————————————————————

## DCC Plots

The plots of the dynamic conditional correlations will be plotted for
all sectors and their respective relationships. These plots will be
repeated for the full period, with an indication of when the
loadshedding periods were, as well as for just the loadshedding periods.

### Full Period

``` r
dcc.time.var.cor_r_plot <- ggplot(dcc.time.var.cor %>% filter(grepl("Resources_", Pairs), !grepl("_Resources", Pairs))) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() + 
  ggtitle("Dynamic Conditional Correlations: Resources") + 
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 years") + 
  annotate("text", 
           x = c(as.Date("2014-9-01"), as.Date("2018-12-01"), as.Date( "2019-10-01"), as.Date("2021-1-01"), as.Date("2022-02-01")), 
           y = -0.3, 
           label = c("Period 1", "Period 2", "Period 3", "Period 4", "Period 5"),
           angle = 90) +
  xlab("") +
  geom_rect(aes(xmin = as.Date("2014-11-01"), xmax = as.Date("2015-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01)  +
  geom_rect(aes(xmin = as.Date("2019-02-01"), xmax = as.Date("2019-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) +
  geom_rect(aes(xmin = as.Date( "2019-12-01"), xmax = as.Date("2020-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) + 
  geom_rect(aes(xmin = as.Date("2021-3-01"), xmax = as.Date("2021-6-30"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2021-9-01"), xmax = as.Date("2021-11-30"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01)

dcc.time.var.cor_r_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
dcc.time.var.cor_f_plot <- ggplot(dcc.time.var.cor %>% filter(grepl("Financials_", Pairs), !grepl("_Financials", Pairs))) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() + 
  ggtitle("Dynamic Conditional Correlations: Financials") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 years") + 
  annotate("text", 
           x = c(as.Date("2014-9-01"), as.Date("2018-12-01"), as.Date( "2019-10-01"), as.Date("2021-1-01"), as.Date("2022-02-01")), 
           y = -0.3, 
           label = c("Period 1", "Period 2", "Period 3", "Period 4", "Period 5"),
           angle = 90) +
  xlab("") +
  geom_rect(aes(xmin = as.Date("2014-11-01"), xmax = as.Date("2015-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01)  +
  geom_rect(aes(xmin = as.Date("2019-02-01"), xmax = as.Date("2019-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) +
  geom_rect(aes(xmin = as.Date( "2019-12-01"), xmax = as.Date("2020-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) + 
  geom_rect(aes(xmin = as.Date("2021-3-01"), xmax = as.Date("2021-6-30"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2021-9-01"), xmax = as.Date("2021-11-30"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01)

dcc.time.var.cor_f_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
dcc.time.var.cor_i_plot <- ggplot(dcc.time.var.cor %>% filter(grepl("Industrials_", Pairs), !grepl("_Industrials", Pairs))) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() + 
  ggtitle("Dynamic Conditional Correlations: Industrials") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 years") + 
  annotate("text", 
           x = c(as.Date("2014-9-01"), as.Date("2018-12-01"), as.Date( "2019-10-01"), as.Date("2021-1-01"), as.Date("2022-02-01")), 
           y = -0.1, 
           label = c("Period 1", "Period 2", "Period 3", "Period 4", "Period 5"),
           angle = 90) +
  xlab("") +
  geom_rect(aes(xmin = as.Date("2014-11-01"), xmax = as.Date("2015-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01)  +
  geom_rect(aes(xmin = as.Date("2019-02-01"), xmax = as.Date("2019-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) +
  geom_rect(aes(xmin = as.Date( "2019-12-01"), xmax = as.Date("2020-3-31"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) + 
  geom_rect(aes(xmin = as.Date("2021-3-01"), xmax = as.Date("2021-6-30"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01) +
  geom_rect(aes(xmin = as.Date("2021-9-01"), xmax = as.Date("2021-11-30"), ymin = -Inf, ymax = Inf), fill = "lightgray", alpha = 0.01)

dcc.time.var.cor_i_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-21-1.png)

### Load Shedding

First I need to create a dummy column of periods of loadshedding in
order to graph the periods accurately.

``` r
dcc.time.var.cor_ls <- dcc.time.var.cor_ls %>% mutate(Period = case_when(date >= "2014-11-01" & date <= "2015-3-31" ~ "Period 1",
                              date >= "2019-02-01" & date <= "2019-3-31" ~ "Period 2",
                              date >= "2019-12-01" & date <= "2020-3-31" ~ "Period 3",
                              date >= "2021-3-01" & date <= "2021-6-30" ~ "Period 4",
                              date >= "2021-9-01" & date <= "2021-11-30" ~ "Period 5",
                              date >= "2022-2-01" & date <= "2022-4-30" ~ "Period 6"))
```

``` r
dcc.time.var.cor_ls_r_plot <- ggplot(dcc.time.var.cor_ls %>% filter(grepl("Resources_", Pairs), !grepl("_Resources", Pairs))) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() + 
  ggtitle("Dynamic Conditional Correlations: Resources", subtitle = "Periods of Loadshedding") +
  facet_grid(~Period, scales = "free_x", space = "free_x") +
  scale_x_date(labels = scales::date_format("%Y %b"), date_breaks = "2 months") + 
  theme(axis.text = element_text(size = 7)) +
  xlab("") 
  

dcc.time.var.cor_ls_r_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
dcc.time.var.cor_ls_i_plot <- ggplot(dcc.time.var.cor_ls %>% filter(grepl("Industrials_", Pairs), !grepl("_Industrials", Pairs))) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() + 
  ggtitle("Dynamic Conditional Correlations: Industrials", subtitle = "Periods of Loadshedding") +
  facet_grid(~Period, scales = "free_x", space = "free_x") +
  scale_x_date(labels = scales::date_format("%Y %b"), date_breaks = "2 months") + 
  theme(axis.text = element_text(size = 7)) +
  xlab("") 
  

dcc.time.var.cor_ls_i_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
dcc.time.var.cor_ls_f_plot <- ggplot(dcc.time.var.cor_ls %>% filter(grepl("Financials_", Pairs), !grepl("_Financials", Pairs))) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() + 
  ggtitle("Dynamic Conditional Correlations: Financials", subtitle = "Periods of Loadshedding") +
  facet_grid(~Period, scales = "free_x", space = "free_x") +
  scale_x_date(labels = scales::date_format("%Y %b"), date_breaks = "2 months") + 
  theme(axis.text = element_text(size = 7)) +
  xlab("") 
  

dcc.time.var.cor_ls_f_plot
```

![](README_files/figure-markdown_github/unnamed-chunk-25-1.png)

# Fit the DCC model on the entire period, the period of loadshedding and the period without loadshedding

``` r
DCCPre <- dccPre(xts_swix_return, include.mean = T, p = 0)
```

    ## Sample mean of the returns:  0.0003456932 0.0005711699 0.0004298297 
    ## Component:  1 
    ## Estimates:  3e-06 0.068837 0.921021 
    ## se.coef  :  1e-06 0.007333 0.008076 
    ## t-value  :  4.034459 9.387105 114.0503 
    ## Component:  2 
    ## Estimates:  3e-06 0.090672 0.893963 
    ## se.coef  :  1e-06 0.010489 0.012194 
    ## t-value  :  4.133875 8.644733 73.30971 
    ## Component:  3 
    ## Estimates:  4e-06 0.102099 0.882119 
    ## se.coef  :  1e-06 0.010795 0.012082 
    ## t-value  :  4.461851 9.458361 73.00865

``` r
# We now have the estimates of volatility for each series. 
# Follow my lead below in changing the output to a usable Xts series for each column in xts_swix_return_loadshed:
Vol <- DCCPre$marVol
colnames(Vol) <- colnames(xts_swix_return)
Vol <- 
  data.frame( cbind( date = index(xts_swix_return), Vol)) %>% # Add date column which dropped away...
  mutate(date = as.Date(date)) %>% tbl_df()  # make date column a date column...
TidyVol <- Vol %>% gather(Sector, Sigma, -date)
ggplot(TidyVol) + geom_line(aes(x = date, y = Sigma, colour = Sector))
```

![](README_files/figure-markdown_github/unnamed-chunk-27-1.png)

## Summary statistics for DCC output

``` r
dcc.time.var.cor %>% spread(key = Pairs, value = Rho) %>% dplyr::select(c(Financials_Industrials, Financials_Resources, Industrials_Resources)) %>% 
  descr(stats = c("mean", "sd", "min", "max"), round.digits = 4) %>% 
  kable(col.names = c("Financials -> Industrials", "Financials -> Resources", "Industrials -> Resources")) %>%
  kable_styling() %>%
  add_footnote("Note: This table provides summary statistics of the dynamic conditional correlations of daily returns of different sectors within the SWIX over the entire period of this study.", notation="none")
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Financials -\> Industrials
</th>
<th style="text-align:right;">
Financials -\> Resources
</th>
<th style="text-align:right;">
Industrials -\> Resources
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.6098154
</td>
<td style="text-align:right;">
0.4088567
</td>
<td style="text-align:right;">
0.4732161
</td>
</tr>
<tr>
<td style="text-align:left;">
Std.Dev
</td>
<td style="text-align:right;">
0.1774655
</td>
<td style="text-align:right;">
0.2042157
</td>
<td style="text-align:right;">
0.1562134
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:right;">
-0.2038155
</td>
<td style="text-align:right;">
-0.4322444
</td>
<td style="text-align:right;">
-0.1155815
</td>
</tr>
<tr>
<td style="text-align:left;">
Max
</td>
<td style="text-align:right;">
0.8744940
</td>
<td style="text-align:right;">
0.7944034
</td>
<td style="text-align:right;">
0.8173085
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border:0;" colspan="100%">
<sup></sup> Note: This table provides summary statistics of the dynamic
conditional correlations of daily returns of different sectors within
the SWIX over the entire period of this study.
</td>
</tr>
</tfoot>
</table>

``` r
dcc.time.var.cor_ls %>% spread(key = Pairs, value = Rho) %>% dplyr::select(c(Financials_Industrials, Financials_Resources, Industrials_Resources)) %>% 
  descr(stats = c("mean", "sd", "min", "max"), round.digits = 4) %>% 
  kable(col.names = c("Financials -> Industrials", "Financials -> Resources", "Industrials -> Resources")) %>%
  kable_styling() %>%
  add_footnote("Note: This table provides summary statistics of the dynamic conditional correlations of daily returns of different sectors within the SWIX over the periods that encounter loadshedding in this study.", notation="none")
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Financials -\> Industrials
</th>
<th style="text-align:right;">
Financials -\> Resources
</th>
<th style="text-align:right;">
Industrials -\> Resources
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.5518966
</td>
<td style="text-align:right;">
0.3879273
</td>
<td style="text-align:right;">
0.4724266
</td>
</tr>
<tr>
<td style="text-align:left;">
Std.Dev
</td>
<td style="text-align:right;">
0.1245376
</td>
<td style="text-align:right;">
0.1433449
</td>
<td style="text-align:right;">
0.1334285
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:right;">
0.2235938
</td>
<td style="text-align:right;">
-0.0118184
</td>
<td style="text-align:right;">
0.0996513
</td>
</tr>
<tr>
<td style="text-align:left;">
Max
</td>
<td style="text-align:right;">
0.8128866
</td>
<td style="text-align:right;">
0.7211935
</td>
<td style="text-align:right;">
0.7746606
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border:0;" colspan="100%">
<sup></sup> Note: This table provides summary statistics of the dynamic
conditional correlations of daily returns of different sectors within
the SWIX over the periods that encounter loadshedding in this study.
</td>
</tr>
</tfoot>
</table>

``` r
dcc.time.var.cor_nls %>% spread(key = Pairs, value = Rho) %>% dplyr::select(c(Financials_Industrials, Financials_Resources, Industrials_Resources)) %>% 
  descr(stats = c("mean", "sd", "min", "max"), round.digits = 4) %>% 
  kable(col.names = c("Financials -> Industrials", "Financials -> Resources", "Industrials -> Resources")) %>%
  kable_styling() %>%
  add_footnote("Note: This table provides summary statistics of the dynamic conditional correlations of daily returns of different sectors within the SWIX over the periods that do not encounter loadshedding in this study.", notation="none")
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Financials -\> Industrials
</th>
<th style="text-align:right;">
Financials -\> Resources
</th>
<th style="text-align:right;">
Industrials -\> Resources
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.6186700
</td>
<td style="text-align:right;">
0.4188031
</td>
<td style="text-align:right;">
0.4808297
</td>
</tr>
<tr>
<td style="text-align:left;">
Std.Dev
</td>
<td style="text-align:right;">
0.1794254
</td>
<td style="text-align:right;">
0.2089847
</td>
<td style="text-align:right;">
0.1516228
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:right;">
-0.2036136
</td>
<td style="text-align:right;">
-0.4319181
</td>
<td style="text-align:right;">
-0.0908740
</td>
</tr>
<tr>
<td style="text-align:left;">
Max
</td>
<td style="text-align:right;">
0.8748958
</td>
<td style="text-align:right;">
0.8027730
</td>
<td style="text-align:right;">
0.8242634
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border:0;" colspan="100%">
<sup></sup> Note: This table provides summary statistics of the dynamic
conditional correlations of daily returns of different sectors within
the SWIX over the periods that do not encounter loadshedding in this
study.
</td>
</tr>
</tfoot>
</table>

``` r
summstats_all <- xts_swix_return %>% 
  descr(stats = c("mean", "med", "sd", "min", "max", "skewness", "kurtosis", "n.valid"), round.digits = 4, order = c("Financials", "Industrials", "Resources"))

rownames(summstats_all) <- c("Mean", "Median", "Std.Dev", "Min", "Max", "Skewness", "Kurtosis", "Observations" )

summstats_all %>% kable() %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Financials
</th>
<th style="text-align:right;">
Industrials
</th>
<th style="text-align:right;">
Resources
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.0004298
</td>
<td style="text-align:right;">
0.0005712
</td>
<td style="text-align:right;">
0.0003457
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
0.0006027
</td>
<td style="text-align:right;">
0.0008255
</td>
<td style="text-align:right;">
0.0003141
</td>
</tr>
<tr>
<td style="text-align:left;">
Std.Dev
</td>
<td style="text-align:right;">
0.0156859
</td>
<td style="text-align:right;">
0.0133266
</td>
<td style="text-align:right;">
0.0190795
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:right;">
-0.1305802
</td>
<td style="text-align:right;">
-0.0848300
</td>
<td style="text-align:right;">
-0.1630378
</td>
</tr>
<tr>
<td style="text-align:left;">
Max
</td>
<td style="text-align:right;">
0.0876787
</td>
<td style="text-align:right;">
0.0838867
</td>
<td style="text-align:right;">
0.1429455
</td>
</tr>
<tr>
<td style="text-align:left;">
Skewness
</td>
<td style="text-align:right;">
-0.2231290
</td>
<td style="text-align:right;">
0.0197523
</td>
<td style="text-align:right;">
-0.0078519
</td>
</tr>
<tr>
<td style="text-align:left;">
Kurtosis
</td>
<td style="text-align:right;">
6.0305257
</td>
<td style="text-align:right;">
3.0440106
</td>
<td style="text-align:right;">
6.5525177
</td>
</tr>
<tr>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:right;">
3458.0000000
</td>
<td style="text-align:right;">
3458.0000000
</td>
<td style="text-align:right;">
3458.0000000
</td>
</tr>
</tbody>
</table>

``` r
summstats_loadshed <- xts_swix_return_loadshed %>% 
  descr(stats = c("mean", "med", "sd", "min", "max", "skewness", "kurtosis", "n.valid"), round.digits = 4, order = c("Financials", "Industrials", "Resources"))

rownames(summstats_loadshed) <- c("Mean", "Median", "Std.Dev", "Min", "Max", "Skewness", "Kurtosis", "Observations" )

summstats_loadshed %>% kable() %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Financials
</th>
<th style="text-align:right;">
Industrials
</th>
<th style="text-align:right;">
Resources
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
-0.0009874
</td>
<td style="text-align:right;">
0.0005666
</td>
<td style="text-align:right;">
-0.0008332
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
-0.0005264
</td>
<td style="text-align:right;">
0.0004494
</td>
<td style="text-align:right;">
-0.0002087
</td>
</tr>
<tr>
<td style="text-align:left;">
Std.Dev
</td>
<td style="text-align:right;">
0.0199832
</td>
<td style="text-align:right;">
0.0151634
</td>
<td style="text-align:right;">
0.0253557
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:right;">
-0.1305802
</td>
<td style="text-align:right;">
-0.0848300
</td>
<td style="text-align:right;">
-0.1630378
</td>
</tr>
<tr>
<td style="text-align:left;">
Max
</td>
<td style="text-align:right;">
0.0773945
</td>
<td style="text-align:right;">
0.0623935
</td>
<td style="text-align:right;">
0.1429455
</td>
</tr>
<tr>
<td style="text-align:left;">
Skewness
</td>
<td style="text-align:right;">
-1.6102407
</td>
<td style="text-align:right;">
-0.6851596
</td>
<td style="text-align:right;">
-0.7235923
</td>
</tr>
<tr>
<td style="text-align:left;">
Kurtosis
</td>
<td style="text-align:right;">
12.2839171
</td>
<td style="text-align:right;">
5.4277833
</td>
<td style="text-align:right;">
11.0085448
</td>
</tr>
<tr>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:right;">
351.0000000
</td>
<td style="text-align:right;">
351.0000000
</td>
<td style="text-align:right;">
351.0000000
</td>
</tr>
</tbody>
</table>

``` r
summstats_no_loadshed <- xts_swix_return_no_loadshed %>% 
  descr(stats = c("mean", "med", "sd", "min", "max", "skewness", "kurtosis", "n.valid"), round.digits = 4, order = c("Financials", "Industrials", "Resources"))

rownames(summstats_no_loadshed) <- c("Mean", "Median", "Std.Dev", "Min", "Max", "Skewness", "Kurtosis", "Observations" )

summstats_no_loadshed %>% kable() %>% kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Financials
</th>
<th style="text-align:right;">
Industrials
</th>
<th style="text-align:right;">
Resources
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.0005899
</td>
<td style="text-align:right;">
0.0005717
</td>
<td style="text-align:right;">
0.0004789
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
0.0006670
</td>
<td style="text-align:right;">
0.0008822
</td>
<td style="text-align:right;">
0.0003539
</td>
</tr>
<tr>
<td style="text-align:left;">
Std.Dev
</td>
<td style="text-align:right;">
0.0151195
</td>
<td style="text-align:right;">
0.0131057
</td>
<td style="text-align:right;">
0.0182357
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:right;">
-0.0930441
</td>
<td style="text-align:right;">
-0.0643214
</td>
<td style="text-align:right;">
-0.1098863
</td>
</tr>
<tr>
<td style="text-align:left;">
Max
</td>
<td style="text-align:right;">
0.0876787
</td>
<td style="text-align:right;">
0.0838867
</td>
<td style="text-align:right;">
0.1097025
</td>
</tr>
<tr>
<td style="text-align:left;">
Skewness
</td>
<td style="text-align:right;">
0.1663539
</td>
<td style="text-align:right;">
0.1430388
</td>
<td style="text-align:right;">
0.2301004
</td>
</tr>
<tr>
<td style="text-align:left;">
Kurtosis
</td>
<td style="text-align:right;">
3.1990410
</td>
<td style="text-align:right;">
2.4856243
</td>
<td style="text-align:right;">
3.7555872
</td>
</tr>
<tr>
<td style="text-align:left;">
Observations
</td>
<td style="text-align:right;">
3107.0000000
</td>
<td style="text-align:right;">
3107.0000000
</td>
<td style="text-align:right;">
3107.0000000
</td>
</tr>
</tbody>
</table>
