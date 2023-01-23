weights_plot <- function(data, output){
    # First we get the weights of the ALSI and SWIX into xts format
    j200_weights <- data %>% dplyr:::select(date, Tickers, J200) %>% spread(Tickers, J200) %>% tbl_xts()
    j400_weights <- data %>% dplyr:::select(date, Tickers, J400) %>% spread(Tickers, J400) %>% tbl_xts()
    # Then we get the returns for all the Tickers into xts format
    stock_returns <- data %>% dplyr:::select(date, Tickers, Return) %>% spread(Tickers, Return)
    # Forcing the NA values to 0
    # We can do this as NA weights are just 0
    j200_weights[is.na(j200_weights)] <- 0
    j400_weights[is.na(j400_weights)] <- 0
    # For NA's in returns it is safer to impute values for the NA's
    stock_returns <- impute_missing_returns(stock_returns, impute_returns_method = "Drawn_Distribution_Collective")
    stock_returns <- stock_returns %>% tbl_xts()
    J200_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns,
                                                   weights = j200_weights, lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE,
                                                   value = 1, geometric = TRUE)
    J400_RetPort <- rmsfuns::Safe_Return.portfolio(stock_returns,
                                                   weights = j400_weights,
                                                   lag_weights = TRUE,
                                                   verbose = TRUE, contribution = TRUE,
                                                   value = 1, geometric = TRUE)
    # Need to clean and save the data
    # Clean and save portfolio returns and weights:
    J200_Contribution <- J200_RetPort$"contribution" %>% xts_tbl()
    J200_BPWeight <- J200_RetPort$"BOP.Weight" %>% xts_tbl()

    J200_BPValue <- J200_RetPort$"BOP.Value" %>% xts_tbl()

    # Clean and save portfolio returns and weights:
    J400_Contribution <- J400_RetPort$"contribution" %>% xts_tbl()

    J400_BPWeight <- J400_RetPort$"BOP.Weight" %>% xts_tbl()

    J400_BPValue <- J400_RetPort$"BOP.Value" %>% xts_tbl()
    names(J200_Contribution) <- c("date", names(J200_RetPort$"contribution"))
    names(J200_BPWeight) <- c("date", names(J200_RetPort$"BOP.Weight"))
    names(J200_BPValue) <- c("date", names(J200_RetPort$"BOP.Value"))
    names(J400_Contribution) <- c("date", names(J400_RetPort$"contribution"))
    names(J400_BPWeight) <- c("date", names(J400_RetPort$"BOP.Weight"))
    names(J400_BPValue) <- c("date", names(J400_RetPort$"BOP.Value"))
    resources_tickers <- T40 %>% filter(Sector %in% "Resources") %>% pull(Tickers) %>% unique
    industrials_tickers <- T40 %>% filter(Sector %in% "Industrials") %>% pull(Tickers) %>% unique
    financials_tickers <- T40 %>% filter(Sector %in% "Financials") %>% pull(Tickers) %>% unique
    J400_BPWeight$resource_weight <- rowSums(J400_BPWeight[, resources_tickers])
    J400_BPWeight$industrial_weight <- rowSums(J400_BPWeight[, industrials_tickers])
    J400_BPWeight$financial_weight <- rowSums(J400_BPWeight[, financials_tickers])
    J200_BPWeight$resource_weight <- rowSums(J200_BPWeight[, resources_tickers])
    J200_BPWeight$industrial_weight <- rowSums(J200_BPWeight[, industrials_tickers])
    J200_BPWeight$financial_weight <- rowSums(J200_BPWeight[, financials_tickers])


    if(output == "ALSI") {
        J200_BPWeight %>%
            dplyr:::select(date, resource_weight, industrial_weight, financial_weight) %>%
            tbl_xts() %>%
            .[endpoints(.,'months')] %>%
            chart.StackedBar()
    } else {
        J400_BPWeight %>%
            dplyr:::select(date, resource_weight, industrial_weight, financial_weight) %>%
            tbl_xts() %>%
            .[endpoints(.,'months')] %>%
            chart.StackedBar()
    }

}
