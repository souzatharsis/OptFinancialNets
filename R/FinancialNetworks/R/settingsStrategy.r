initialiseSettingsStrategy <- function(rebalanceFrequency     = 5,
                                       inSample               = 500,
                                       tradeOnFutures         = 1,
                                       tradeOnCashAsset       = 1,
                                       gearing                = 1,
                                       limitNumberOfCompanies = 0,
                                       includeDaysBefore      = 1,
                                       necessaryDaysBefore    = 0) {

    settingsStrategy <- settingsStrategyObject$new(
        inSample               = inSample,
        gearing                = gearing,
        rebalanceFrequency     = rebalanceFrequency,
        tradeOnFutures         = tradeOnFutures,
        tradeOnCashAsset       = tradeOnCashAsset,
        limitNumberOfCompanies = limitNumberOfCompanies,
        includeDaysBefore      = includeDaysBefore,
        necessaryDaysBefore    = necessaryDaysBefore);

    settingsStrategy$prepareAndCheckData();

    return(settingsStrategy);
}

settingsStrategyObject <- setRefClass("settingsStrategyObject", 
    
    fields = list(
        inSample               = "numeric",
        gearing                = "numeric",
        rebalanceFrequency     = "numeric",
        tradeOnFutures         = "numeric",
        tradeOnCashAsset       = "numeric",
        limitNumberOfCompanies = "numeric",
        includeDaysBefore      = "numeric",
        necessaryDaysBefore    = "numeric",
        inSampleMultiplier     = "numeric"
    ),

    methods = list(

################
# Methods (non idented with the definition of the class)


setTradeOnCashAsset = function(value) {
    tradeOnCashAsset <<- value;
    .errorCheckBinaryParameter(tradeOnCashAsset);
},


prepareAndCheckData = function() {

    .errorCheckPositiveInteger(inSample);
    .errorCheckPositiveInteger(rebalanceFrequency);
    .errorCheckBinaryParameter(tradeOnFutures);
    .errorCheckBinaryParameter(tradeOnCashAsset);
    .errorCheckNonNegativeInteger(limitNumberOfCompanies);
    .errorCheckBinaryParameter(includeDaysBefore);
    .errorCheckNonNegativeInteger(necessaryDaysBefore);
    .errorCheckGreaterOrEqualToOne(gearing);
       
      
    if (necessaryDaysBefore == 0) {
        necessaryDaysBefore <<- inSample + 1;
    }

    # Necessary as we may need more than in-sample dates to be able to have valid predictors 
    # TODO: There will be problems if necessaryDaysBefore is bigger than 3*inSample
    inSampleMultiplier <<- 3;


},

getAsString = function() {
    s = "";
    s = paste0(s, "settingsStrategy:inSample ",               inSample,               "\n");
    s = paste0(s, "settingsStrategy:gearing ",                gearing,                "\n");
    s = paste0(s, "settingsStrategy:rebalanceFrequency ",     rebalanceFrequency,     "\n");
    s = paste0(s, "settingsStrategy:tradeOnFutures ",         tradeOnFutures,         "\n");
    s = paste0(s, "settingsStrategy:tradeOnCashAsset ",       tradeOnCashAsset,       "\n");
    s = paste0(s, "settingsStrategy:limitNumberOfCompanies ", limitNumberOfCompanies, "\n");
    s = paste0(s, "settingsStrategy:includeDaysBefore ",      includeDaysBefore,      "\n");
    s = paste0(s, "settingsStrategy:necessaryDaysBefore ",    necessaryDaysBefore,    "\n");

    return(s);
}

################

    )
)







