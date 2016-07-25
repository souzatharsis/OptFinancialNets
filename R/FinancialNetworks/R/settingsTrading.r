initialiseSettingsTrading <- function(index                  = "FTSE100",
                                      initialDate            = 20100101,
                                      finalDate              = 20100110,
                                      autoRiskFreeRate       = 1,
                                      riskFreeRate           = 0,
                                      transactionCosts       = 0.0005,
                                      marginTransactionCosts = 0,
                                      stopLoss               = 0,
                                      slippage               = 0.0010,
                                      slippageOpen           = 0.0025,
                                      initialInvestment      = 100000,
                                      useLotSizing           = 0,
                                      useFixedLot            = 1,
                                      lotSize                = 1,
                                      automaticLot           = 0) {

    # if autoRiskFreeRate = 1, it overrides the riskFreeRate parameter
    # with an automatic choice

    settingsTrading <- settingsTradingObject$new(
        index                  = index,
        initialDate            = initialDate,
        finalDate              = finalDate,
        autoRiskFreeRate       = autoRiskFreeRate,
        riskFreeRate           = riskFreeRate,
        transactionCosts       = transactionCosts,
        marginTransactionCosts = marginTransactionCosts,
        stopLoss               = stopLoss,
        slippage               = slippage,
        slippageOpen           = slippageOpen,
        initialInvestment      = initialInvestment,
        useLotSizing           = useLotSizing,
        useFixedLot            = useFixedLot,
        lotSize                = lotSize,
        automaticLot           = automaticLot);

    settingsTrading$prepareAndCheckData();

    return(settingsTrading);
}

settingsTradingObject <- setRefClass("settingsTradingObject", 
    
    fields = list(
        # Basic parameters
        index                  = "character",
        initialDate            = "numeric",
        finalDate              = "numeric",
        autoRiskFreeRate       = "numeric",
        riskFreeRate           = "numeric",
        riskFreeRateDaily      = "numeric",
        transactionCosts       = "numeric",
        marginTransactionCosts = "numeric",
        slippage               = "numeric",
        slippageOpen           = "numeric",
        stopLoss               = "numeric",
        initialInvestment      = "numeric",
        useLotSizing           = "numeric",
        useFixedLot            = "numeric",
        lotSize                = "numeric",
        automaticLot           = "numeric"
    ),

    methods = list(

################
# Methods (non idented with the definition of the class)

prepareAndCheckData = function() {

    .errorCheckNonNegativeInteger(initialDate);
    .errorCheckNonNegativeInteger(finalDate);
    .errorCheckBinaryParameter(autoRiskFreeRate);
    .errorCheckBinaryParameter(useLotSizing);
    .errorCheckBinaryParameter(useFixedLot);
    .errorCheckNonNegativeInteger(lotSize);
    .errorCheckBinaryParameter(automaticLot);
    .errorCheckLessThanOneAndMoreThanMinusOne(riskFreeRate);
    .errorCheckNonNegativeLessThanOne(transactionCosts);
    .errorCheckNonNegativeLessThanOne(marginTransactionCosts);
    .errorCheckNonNegativeLessThanOne(slippage);
    .errorCheckNonNegativeLessThanOne(slippageOpen);
    .errorCheckStrictlyPositive(initialInvestment);
    .errorCheckNonNegativeLessThanOne(stopLoss);

    if (finalDate < initialDate) stop0("Error: dates are not consistent in settingsTrading");
    if (autoRiskFreeRate) {
        if (index == "NIFTY50" || index == "BSE30") {
            riskFreeRate <<- 0.09;
        } else if (index == "FTSE100") {
            riskFreeRate <<- 0.02;
        } else if (index == "HANGSENG50") {
            riskFreeRate <<- 0.04;
        } else {
            riskFreeRate <<- 0.02;
        }
    }

    if (index == "NIFTY50" && initialDate != finalDate) {
        initialInvestment <<- 50000000;
    }
    
    if (useLotSizing == 1) {
        if (index == "NIFTY50" || index == "BSE30") {
            useFixedLot <<- 0;
        }
    }

    riskFreeRateDaily <<- annualRateToDaily(riskFreeRate);


},

setInvestment = function(inv) {
    .errorCheckStrictlyPositive(initialInvestment);
    initialInvestment <<- inv;
},

makeStopLossZero = function() {
    stopLoss <<- 0;
},

setDates = function(date1, date2) {
    .errorCheckNonNegativeInteger(date1);
    .errorCheckNonNegativeInteger(date2);
    if (date2 < date1) stop0("Error: dates are not consistent in settingsTrading");
    
    initialDate <<- date1;
    finalDate   <<- date2;
},

getAsString = function() {
    s = "";
    s = paste0(s, "settingsTrading:index ",                  index,                  "\n");
    s = paste0(s, "settingsTrading:initialDate ",            initialDate,            "\n");
    s = paste0(s, "settingsTrading:finalDate ",              finalDate,              "\n");
    s = paste0(s, "settingsTrading:autoRiskFreeRate ",       autoRiskFreeRate,       "\n");
    s = paste0(s, "settingsTrading:riskFreeRate ",           riskFreeRate,           "\n");
    s = paste0(s, "settingsTrading:transactionCosts ",       transactionCosts,       "\n");
    s = paste0(s, "settingsTrading:marginTransactionCosts ", marginTransactionCosts, "\n");
    s = paste0(s, "settingsTrading:stopLoss ",               stopLoss,               "\n");
    s = paste0(s, "settingsTrading:slippage ",               slippage,               "\n");
    s = paste0(s, "settingsTrading:slippageOpen ",           slippageOpen,           "\n");
    s = paste0(s, "settingsTrading:initialInvestment ",      initialInvestment,      "\n");
    s = paste0(s, "settingsTrading:useLotSizing ",           useLotSizing,           "\n");
    s = paste0(s, "settingsTrading:useFixedLot ",            useFixedLot,            "\n");
    s = paste0(s, "settingsTrading:lotSize ",                lotSize,                "\n");
    s = paste0(s, "settingsTrading:automaticLot ",           automaticLot,           "\n");
    return(s);
}


################

    )
)







