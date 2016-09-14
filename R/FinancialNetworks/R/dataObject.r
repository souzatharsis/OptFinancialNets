##############################
# 
# MAIN FUNCTION TO LOAD ALL THE DATA
# 
# This function will return a dataObject which contains all the necessary data to run a strategy. There are many parameters.
#
# 
# MAIN PARAMETERS:
#
# db                  -> Connection to the database
#
# COMPANIES TO BE SELECTED PARAMETERS:
#
# -----> These 3 params below are falling in disuse, we used to filter by market capitalisation.
#
# limitToTopCompanies     -> Use only a subset of the companies of the index (instead of all).
# limitByPercentageMV     -> 1: Value of valueTopCompanies will be a percentage of total market value
#                            0: or an absolute number
# valueTopCompanies       -> Percentage or number of top companies to be used 
#                            Futures will be included here if tradeOnFutures = 1
#
# OTHER PARAMETERS
#
# logReturns              -> 1 to compute log returns, 0 to compute returns

.loadData <- function( # MAIN PARAMETERS
                       db, 
                       settingsTrading,
                       settingsStrategy,
                       settingsNews   = NULL,
                       settingsReturn = NULL,
                       # COMPANIES TO BE SELECTED PARAMETERS
                       limitToTopCompanies=0, limitByPercentageMV=1, valueTopCompanies=100,
                       # CONTROL PARAMETERS
                       logReturns=0
                     ) {

    if (is.null(settingsNews))   settingsNews   = initialiseSettingsNews();
    if (is.null(settingsReturn)) settingsReturn = initialiseSettingsReturn();
    .errorCheckSettingsObjNotNull(settingsTrading);
    .errorCheckSettingsObjNotNull(settingsStrategy);
    .errorCheckSettingsObjNotNull(settingsNews);
    .errorCheckSettingsObjNotNull(settingsReturn);
 
    dataObj <- DataObject$new(
        # Basic parameters
        settingsTrading     = settingsTrading,
        settingsStrategy    = settingsStrategy,
        settingsNews        = settingsNews,
        settingsReturn      = settingsReturn,
        actualInitialDate   = settingsTrading$initialDate,
        actualFinalDate     = settingsTrading$finalDate,

        # Companies traded parameters

        limitToTopCompanies  = limitToTopCompanies,
        limitByPercentageMV  = limitByPercentageMV,
        valueTopCompanies    = valueTopCompanies,

        # Control parameters
        logReturns              = logReturns
    );


    dataObj$loadBasicData(db);
    
        
    return (dataObj);
}




DataObject <- setRefClass("DataObject", 
    
    fields = list(
        # Basic parameters
        settingsTrading     = "ANY",
        settingsStrategy    = "ANY",
        settingsNews        = "ANY",
        settingsReturn      = "ANY",
        
        actualInitialDate   = "numeric",
        actualFinalDate     = "numeric",

        limitToTopCompanies = "numeric", 
        limitByPercentageMV = "numeric", 
        valueTopCompanies   = "numeric",

        # CONTROL parameters (1 or 0)
        logReturns              = "numeric",

        # Basic Data loaded from the database
        indexPrices        = "numeric",    # Prices for the index
        futures            = "numeric",    # Futures prices
        dates              = "numeric",    # list of dates
        companies          = "character",  # list of companies names
        companiesIds       = "character",  # list of companies ids
        gearingFactor      = "numeric",
        needsMarginAccount = "numeric",
        prices             = "matrix",  # (days x companies)

        indexLow      = "numeric",
        indexHigh     = "numeric",
        indexOpen     = "numeric",
        low           = "matrix",  # (days x companies)
        high          = "matrix",  # (days x companies)
        open          = "matrix",  # (days x companies)

        # Dimensions
        numDates      = "numeric",
        numCompanies  = "numeric",
        numRebalances = "numeric",

        # Basic news data loaded from the database
        impact       = "matrix",
        impactPos    = "matrix",
        impactNeg    = "matrix",
        newsFlow     = "matrix",

        # Data that is automatically computed
        firstTrade          = "numeric",
        returns             = "matrix",
        indexReturns        = "numeric",
        rebalances          = "numeric",
        lots                = "matrix",
        wasInIndex          = "matrix",
        
        # Data that is manually computed (functions need to be invoked)
        rsi             = "matrix",
        
        # All here is numRebalances x numCompanies
        garchVol        = "matrix",
        garchRet        = "matrix",
        archRet         = "matrix",
        predRet         = "matrix",
        predVol         = "matrix",

        # Flags to tell if data was computed
        computedRSI     = "numeric",
        computedArch    = "numeric",
        computedGarch   = "numeric",
        computedPredRet = "numeric",
        computedPredVol = "numeric"
        
    ),

    methods = list(

################
# Methods (non idented with the definition of the class)

loadBasicData = function(db) {

    
    liveDate = 0;
    if (actualInitialDate == actualFinalDate) {
        if (actualInitialDate == 0) {
            actualInitialDate <<- selectLastIndexDate(db, settingsTrading$index);
            actualFinalDate   <<- actualInitialDate;
        
            settingsTrading$setDates(actualInitialDate, actualFinalDate);
        } else {
            liveDate = actualInitialDate;
        }
    }



    ######################################
    #### LOADING INDEX PRICES AND FUTURES
    results = .selectIndexPrices(db=db, index=settingsTrading$index, initialDate=actualInitialDate, finalDate=actualFinalDate, 
                                 inSample=settingsStrategy$inSampleMultiplier*settingsStrategy$inSample, 
                                 includePreviousDates=settingsStrategy$includeDaysBefore);
    dates             <<- results$dates;
    futures           <<- results$futures;
    actualInitialDate <<- results$initialDate;
    actualFinalDate   <<- results$finalDate;
    indexPrices       <<- results$prices;
    indexHigh         <<- results$high; 
    indexOpen         <<- results$open; 
    indexLow          <<- results$low; 
    rm(results);
    ######################################

    ######################################
    #### LOADING LIST OF COMPANIES
    cps = "";
    beg = 1;
    if (settingsStrategy$includeDaysBefore == 1) {
        for (i in 1 : length(dates)-1) {
            if (dates[i+1] >= settingsTrading$initialDate) {
                beg = i;
                break;
            }
        }   
    }
    cps = .selectCompanies(db=db, index=settingsTrading$index, initialDate=dates[beg], finalDate=actualFinalDate, 
                           inSample=settingsStrategy$inSample, valueTopCompanies=valueTopCompanies, 
                           limitToTopCompanies=limitToTopCompanies, 
                           limitByPercentageMV=limitByPercentageMV); 

    companies    <<- cps$companies;
    companiesIds <<- cps$companiesIds;
    rm(cps);
    ######################################

    
    if (settingsStrategy$limitNumberOfCompanies > 0 && settingsNews$limitCompaniesByNewsFlow == 0) {
        companies    <<- companies[1:settingsStrategy$limitNumberOfCompanies];
        companiesIds <<- companiesIds[1:settingsStrategy$limitNumberOfCompanies];
    }

    ######################################
    #### LOADING PRICES
    prs = .selectPricesGivenCompanies(db=db, companies=companiesIds, index=settingsTrading$index, 
                                      initialDate=actualInitialDate, finalDate=actualFinalDate);
    prices <<- prs$prices;
    high   <<- prs$high; 
    low    <<- prs$low; 
    open   <<- prs$open; 
    rm(prs);
    ######################################
  
    TIME = proc.time();
    removeInvalidPricesDataObj();
   
    actualInitialDate <<- dates[1];
   
    if (actualInitialDate > settingsTrading$initialDate) actualInitialDate <<- settingsTrading$initialDate;

    if (settingsStrategy$includeDaysBefore == 1) {
        if (firstTrade > length(dates) || dates[firstTrade] > settingsTrading$initialDate) {
            for (i in 1 : (length(dates)-1)) {
                if (dates[i] >= settingsTrading$initialDate) {
                    firstTrade <<- i;
                    break;
                }
            }

        }
    }


    numDates <<- length(dates);
   
    if (settingsStrategy$tradeOnFutures) {
        companiesIds <<- c("FUTURES", companiesIds); 
        companies    <<- c(paste0(settingsTrading$index, " futures"), companies);
        prices       <<- cbind(futures, prices);
        high         <<- cbind(indexHigh, high);
        low          <<- cbind(indexLow, low);
        open         <<- cbind(indexOpen, open);
    }


    if (numDates < firstTrade) stop0("Too few dates to compute results.");

    # Rebalances
    numRebalances <<- floor( ((numDates-1) - firstTrade) / settingsStrategy$rebalanceFrequency) + 1;
    if (firstTrade == numDates) numRebalances <<- 1;
    if (numRebalances <= 0) stop0("Not enough data, number of rebalances is zero");
    rebalances <<- numeric(numRebalances);
    rebalances[1] <<- firstTrade;
    if (numRebalances > 1) {
        for (i in 2 : numRebalances) {
            rebalances[i] <<- rebalances[i-1] + settingsStrategy$rebalanceFrequency;
        }
    }

    impact    <<- matrix(0, nrow=numDates, ncol = 1);
    impactPos <<- matrix(0, nrow=numDates, ncol = 1);
    impactNeg <<- matrix(0, nrow=numDates, ncol = 1);
    newsFlow  <<- matrix(0, nrow=numDates, ncol = 1);

    if (settingsNews$loadNews) {
        cps = companiesIds;
        if (settingsStrategy$tradeOnFutures) cps = companiesIds[2:length(companiesIds)];
        impactResult = .computeImpact(db=db, index=settingsTrading$index, initialDate=actualInitialDate, 
                                      finalDate=actualFinalDate, 
                                      companiesIds=cps, dates=dates, 
                                      computeImpact="both",
                                      newsSource=settingsNews$newsSource,
                                      relevance=settingsNews$newsRelevance, 
                                      positiveImpactThreshold=settingsNews$positiveImpactThreshold, 
                                      negativeImpactThreshold=settingsNews$negativeImpactThreshold,
                                      minutesDecay = settingsNews$minutesDecay, 
                                      includeMarketCap = settingsNews$includeMarketCap, 
                                      includeCategories = settingsNews$includeCategories);

        impactPos <<- impactResult$impactPos;
        impactNeg <<- impactResult$impactNeg;
        newsFlow  <<- impactResult$newsFlow;
        impact    <<- impactResult$impact;
        rm(impactResult);
        
        if (!settingsStrategy$tradeOnFutures) {
            # If not futures, remove news data from it
            impactPos <<- impactPos[,2:ncol(impactPos)];
            impactNeg <<- impactNeg[,2:ncol(impactNeg)];
            newsFlow  <<- newsFlow [,2:ncol(newsFlow )];
            impact    <<- impact   [,2:ncol(impact   )];
        }
    }

    if (settingsStrategy$tradeOnCashAsset) {
        companiesIds <<- c("RFR", companiesIds); 
        companies    <<- c("Risk free rate", companies); 
        rfr = rep(1, nrow(prices));
        for (i in 2 : length(rfr)) {
            rfr[i] = rfr[i-1] + (rfr[i-1] * settingsTrading$riskFreeRateDaily);
        }
        prices       <<- cbind(rfr, prices);
        high         <<- cbind(rfr, high);
        open         <<- cbind(rfr, open);
        low          <<- cbind(rfr, low);
        if (settingsNews$loadNews) {
            temp = matrix(0, nrow=numDates, ncol=1);
            impactPos <<- cbind(temp, impactPos);
            impactNeg <<- cbind(temp, impactNeg);
            newsFlow  <<- cbind(temp, newsFlow );
            impact    <<- cbind(temp, impact   );
        }
    }
    open   <<- unname(open);
    high   <<- unname(high);
    low    <<- unname(low);
    prices <<- unname(prices);

    numCompanies <<- ncol(prices);

    ###################
    # lots
    lots <<- matrix(0, nrow=numDates, ncol = numCompanies);
    if (settingsTrading$useLotSizing == 1) {
        TIME = proc.time();
        printf("Getting lots information in ");
        if (settingsTrading$useFixedLot) {
            lots <<- matrix(settingsTrading$lotSize, nrow=numDates, ncol = numCompanies);
        } else {
            lots <<- matrix(-1, nrow=numDates, ncol = numCompanies);
            firstCompany = 1;
            if (settingsStrategy$tradeOnCashAsset) {
                lots[,firstCompany] <<- rep(1, numDates);
                firstCompany = firstCompany + 1;
            }
            if (settingsStrategy$tradeOnFutures) {
                lots[,firstCompany] <<- .selectLotsFutures(db=db, index=settingsTrading$index, dates=dates);
                firstCompany = firstCompany + 1;
            }
            for (j in firstCompany : numCompanies) {
                #printf("Company %s = %s\n", companiesIds[j], companies[j]);
                lots[,j] <<- .selectLots(db=db, index=settingsTrading$index, company=companiesIds[j], dates=dates);
            }
            if (settingsTrading$automaticLot == 1) {
                firstCompany = 1;
                if (settingsStrategy$tradeOnCashAsset) firstCompany = firstCompany + 1;
                lots <<- .calculateLotSizes(prices=prices, dates=dates, lots=lots, firstTrade=firstTrade, firstCompany=firstCompany);
            } else {
                for (i in 1 : nrow(lots)) {
                    for (j in 1 : ncol(lots)) {
                        if (lots[i,j] == -1) lots[i,j] <<- 1;
                    }
                }
                for (c in 1 : ncol(lots)) {
                    row = nrow(lots);
                    if (lots[row,c] == 1) {
                        lastValidLot = -1;
                        while (lastValidLot < 0) {
                            row = row - 1;
                            if (row == 0) break;
                            if (lots[row,j] > 1) {
                                lastValidLot = row;
                                break;
                            }
                        }
                        if (lastValidLot > 1 && lastValidLot < nrow(lots)) {
                            for (i in (lastValidLot + 1) : nrow(lots)) {
                                lots[i, c] <<- lots[lastValidLot, c];
                            }
                        }
                    }
                }
            }

        }
        printf("%.2fs\n", (proc.time() - TIME)[3]);
    }
    ###################

   
    # wasInIndex
    TIME = proc.time();
    #wasInIndexTemp = matrix(1, nrow=numRebalances, ncol = numCompanies);
   
    printf("Compiling index information: ");
    wasInIndexTemp = matrix(0, nrow=numRebalances, ncol = numCompanies);
    if (settingsNews$limitCompaniesByNewsFlow == 0 || settingsStrategy$limitNumberOfCompanies == 0) {
        for (i in 1 : numRebalances) {
            printf(" %d", i);
            date = dates[rebalances[i]];
            for (j in 1 : numCompanies) {
                if (j == 1 && (settingsStrategy$tradeOnFutures == 1 || settingsStrategy$tradeOnCashAsset)) {
                    wasInIndexTemp[i,j] = 1;
                } else if (j == 2 && settingsStrategy$tradeOnFutures == 1 && settingsStrategy$tradeOnCashAsset) {
                    wasInIndexTemp[i,j] = 1;
                } else {
                    wasInIndexTemp[i,j] = .wasInTheIndex(db=db, index=settingsTrading$index, company=companiesIds[j], date=date);
                }
            }
        }
        printf("\n");
    } else {
        # Gets here if both number of companies is limited and news flow is used as limiting criteria.
        for (i in 1 : numRebalances) {
            printf(" %d", i);
            begin = (i-settingsStrategy$inSample+1);
            end = rebalances[i];
            if (begin < 1) begin = 1;
            
            newsFlowTotal = numeric();
            if (end > begin) {
                newsFlowTotal = colSums(newsFlow[begin:end, ]);
            } else {
                newsFlowTotal = newsFlow[begin:end, ];
            }
            temp = data.frame(a=seq(1, length(newsFlowTotal)), b=newsFlowTotal);
            temp = order(temp[,2], decreasing=TRUE);
            
            currentIndex = 1;
            for (j in 1 : length(temp)) {
                jj = temp[j];
                if (sum(prices[begin:end,jj] < 0) > 0) {
                    next;
                }
                wasInIndexTemp[i, jj] = 1;
                currentIndex = currentIndex + 1;
                if (currentIndex > settingsStrategy$limitNumberOfCompanies) break;
            }
        }
        printf("\n");
    }

    wasInIndex   <<- wasInIndexTemp;
    #printf("\nFinished in %.2fs\n", proc.time() - TIME);

   
    computedRSI     <<- 0;
    computedArch    <<- 0;
    computedGarch   <<- 0;
    computedPredRet <<- 0;
    computedPredVol <<- 0;


    ######################################
    ### CALCULATE RETURNS
    if (logReturns) {
        ret  = .calculateLogReturns(prices,      dates); 
        iret = .calculateLogReturns(indexPrices, dates); 
    } else {
        ret  = .calculateReturns(prices,      dates); 
        iret = .calculateReturns(indexPrices, dates); 
    }
    returns      <<- ret$returns;
    indexReturns <<- iret$returns;

    if (settingsStrategy$tradeOnCashAsset) {
        returns[,1] <<- round(returns[,1], digits = 10);
    }
    rm(ret);
    rm(iret);
    ######################################

    #######

    needsMarginAccount <<- rep(0, length(companies));
    if (settingsStrategy$gearing >= 1) {
        gearingFactor <<- numeric(length(companies));
        for (i in 1 : length(gearingFactor)) {
            gearingFactor[i] <<- settingsStrategy$gearing;
        }
        if (settingsStrategy$tradeOnCashAsset) gearingFactor[1] <<- 1;
        for (i in 1 : length(gearingFactor)) {
            returns[,i] <<- returns[,i] * gearingFactor[i];
        }
        for (i in 1 : length(gearingFactor)) {
            if (gearingFactor[i] > 1) needsMarginAccount[i] <<- 1;
        }
    }


},


getRSI = function() {
    
    if (computedRSI == 0) {
        rsi  <<- matrix(0, nrow=numDates, ncol = numCompanies);

        TIME = proc.time();
        printf("\nRSI is being computed...\n");
        for (i in 1 : ncol(prices)) {
            if (i %% 10 == 0) {
                printf("%.0f%% ", i/ncol(prices)*100);
            }
            rsi[,i] <<- .computeRSI(prices[,i], days=settingsReturn$rsiDays, emaRatio=settingsReturn$rsiRatio, inSample=settingsStrategy$inSample);
        }
        printf("\nRSI computed in %.2fs\n", (proc.time() - TIME)[3]);
        computedRSI <<- 1;
    }
    return (rsi);
},

checkGarch = function(useNews = 0) {
    
    if (computedGarch == 0) {
        printf("\nGarch is being computed...\n"); TIME = proc.time();
       
        garchVol <<- matrix(0, nrow=numRebalances, ncol = numCompanies);
        garchRet <<- matrix(0, nrow=numRebalances, ncol = numCompanies);
        
        currentPerc = 1; 
        percInterval = 1;
        
        for (i in 1 : numRebalances) {
            beg = (i-garchSample+1);
            if (beg < 0) beg = 1;
            end = rebalances[i];
            for (j in 1 : numCompanies) {
                
                if (wasInIndex[i, j] == 0) next;
                data = returns[beg:end, j];
                
                p = .fitGarchNew(dataVector = data, addExtraFactors = useNews, predictReturn=TRUE, 
                                impactPos[beg:end,j], impactNeg[beg:end,j]);

                if (is.nan(p$vol)) p = .fitGarchNew(dataVector = data, addExtraFactors = 0, predictReturn=TRUE);
                
                garchVol[i, j] <<- p$vol;
                garchRet[i, j] <<- p$ret;
            }
            if ((i/numRebalances)*100 >= currentPerc) {
                printf("%.0f%% ", (i/numRebalances)*100);
                currentPerc = currentPerc + percInterval;
                if (currentPerc > 100) currentPerc = 100;
            }
        }

        printf("\nGarch computed in %.2fs\n", (proc.time() - TIME)[3]);
        computedGarch <<- 1;
    }

},

getGarchRet = function(useNews = 0) {
    checkGarch(useNews);
    return(garchRet);
},

getGarchVol = function(useNews = 0) {
    checkGarch(useNews);
    return(garchVol);
},

getImpactPos = function() {
    if (settingsNews$loadNews == 0) stop0("Error in getImpactPos: news was not loaded, settingsNews$loadNews should be 1.");
    return (impactPos);
},

getImpactNeg = function() {
    if (settingsNews$loadNews == 0) stop0("Error in getImpactNeg: news was not loaded, settingsNews$loadNews should be 1.");
    return (impactNeg);
},

getNewsFlow = function() {
    if (settingsNews$loadNews == 0) stop0("Error in getNewsFlow: news was not loaded, settingsNews$loadNews should be 1.");
    return (newsFlow);
},

getPredRet = function() {
    if (computedPredRet == 0) stop0("Error: Predicted returns were not computed.");
    return (predRet);
},

getPredVol = function() {
    if (computedPredVol == 0) stop0("Error: Predicted volatilities were not computed.");
    return (predVol);
},

getArch = function() {
    
    if (computedArch == 0) {

        TIME = proc.time();
        printf("\nArch is being computed...\n");
       
        archRet <<- matrix(0, nrow=numRebalances, ncol = numCompanies);
        
        currentPerc = 1;
        percInterval = 1;
        for (i in 1 : numRebalances) {
            beg = (i-garchSample+1);
            if (beg < 0) beg = 1;
            end = rebalances[i];
            
            for (j in 1 : numCompanies) {
                if (wasInIndex[i, j] == 0) next;
                archRet[i, j] <<- .fitArch(dataVector = returns[beg:end, j]);
            }
            if ((i/numRebalances)*100 >= currentPerc) {
                printf("%.0f%% ", (i/numRebalances)*100);
                currentPerc = currentPerc + percInterval;
                if (currentPerc > 100) currentPerc = 100;
            }
        }
        printf("\nArch computed in %.2fs\n", (proc.time() - TIME)[3]);
        computedArch <<- 1;
    }
    return(archRet);
},


##########################
### PREDICTIONS

predictVolatility.garch = function(useNews = 0) {
    if (computedPredVol == 1) {
        printf("WARNING in predictVolatility.garch: Predicted volatility was already computed, no action was taken.");
        return;
    }
    .errorCheckBinaryParameter(useNews,"useNews");
    
    # Just error checking
    if (useNews) getImpactPos();

    TIME = proc.time();
    printf("\nVolatility with garch is being computed...\n");
    getGarchVol(useNews);
    
    #predVol <<- matrix(0, nrow=numRebalances, ncol = numCompanies);
    predVol <<- garchVol;
    computedPredVol <<- 1;
    printf("Volatility with Garch computed in %.2fs\n", (proc.time() - TIME)[3]);
},
   

predictReturn.perfectDirection = function() {
   if (computedPredRet == 1) {
        printf("WARNING in predictReturn.perfectDirection: Predicted return was already computed, no action was taken.");
        return;
    }
    
    predRet <<- matrix(0, nrow=numRebalances, ncol = numCompanies);

    for (i in 1 : numRebalances) {
        for (j in 1 : numCompanies) {
            predRet[i,j] <<- returns[numRebalances[i]+1, j];
        }
    }

    for (j in 1 : numCompanies) {
        # Fixed seed to get always the same results ( 6543333 )
        # TODO check if this works
        # TODO where to call perfectDirection?
        predRet[,j] <<- flipSignals(predRet[,j], seed=6543336, amountFlipped = 1-settingsReturn$amountPerfect);
    }

    computedPredRet <<- 1;
},

predictReturn.logisticRegression = function(useRsi = 0, useImpactPos = 0, useImpactNeg = 0, 
                                            useNewsFlow = 0, useLagReturn = 0) {

    if (computedPredRet == 1) {
        printf("WARNING in predictReturn.logisticRegression: Predicted return was already computed, no action was taken.");
        return;
    }

    .errorCheckBinaryParameter(useRsi);
    .errorCheckBinaryParameter(useImpactPos);
    .errorCheckBinaryParameter(useImpactNeg);
    .errorCheckBinaryParameter(useNewsFlow);
    .errorCheckBinaryParameter(useLagReturn);

    if (useRsi + useImpactPos + useImpactNeg + useNewsFlow + useLagReturn == 0)
        stop0("Error in predictReturn.logisticRegression: At least one predictor must be selected.");
   
    if (useImpactPos == 1) getImpactPos(); 
    if (useImpactNeg == 1) getImpactNeg(); 
    if (useNewsFlow  == 1) getNewsFlow(); 

    if (useRsi) getRSI();
    
    # Independent variable: 0 or 1
    nextDay = matrix(0, nrow=numDates-1, ncol = numCompanies);
    for (i in 1 : numDates-1) {
        for (j in 1 : numCompanies) {
            if (returns[i+1,j] > 0) {
                nextDay[i,j] = 1;
            } else {
                nextDay[i,j] = 0;
            }
        }
    }

    TIME = proc.time();
    printf("\nLogistic regression is being computed...\n");
    
    predRet <<- matrix(0, nrow=numRebalances, ncol = numCompanies);
    
    for (i in 1 : numRebalances) {
        for (cp in 1 : numCompanies) {
            begin = i - settingsStrategy$inSample + 1;
            end = rebalances[i] - 1;
            ii = rebalances[i];
            
            df <- data.frame(y = nextDay[begin:end, cp], stringsAsFactors=FALSE);
            if (useLagReturn) df$lagRet   = returns  [begin:end, cp];
            if (useRsi)       df$rsi      = rsi      [begin:end, cp];
            if (useImpactPos) df$impPos   = impactPos[begin:end, cp];
            if (useImpactNeg) df$impNeg   = impactNeg[begin:end, cp];
            if (useNewsFlow)  df$newsFlow = newsFlow [begin:end, cp];
            
            new.df = data.frame();
            new.df.defined = 0;
            if (useLagReturn) {
                if (new.df.defined == 0) { 
                    new.df = data.frame(lagRet = returns[ii, cp]);
                    new.df.defined = 1;
                } else {
                    new.df$lagRet = returns  [ii, cp];
                }
            }
            if (useRsi) {
                if (new.df.defined == 0) { 
                    new.df = data.frame(rsi = rsi[ii, cp]);
                    new.df.defined = 1;
                } else {
                    new.df$rsi = rsi[ii, cp];
                }
            }
            if (useImpactPos) {
                if (new.df.defined == 0) { 
                    new.df = data.frame(impPos = impactPos[ii, cp]);
                    new.df.defined = 1;
                } else {
                    new.df$impPos = impactPos[ii, cp];
                }
            }
            if (useImpactNeg) {
                if (new.df.defined == 0) { 
                    new.df = data.frame(impNeg = impactNeg[ii, cp]);
                    new.df.defined = 1;
                } else {
                    new.df$impNeg = impactNeg[ii, cp];
                }
            }
            if (useNewsFlow) {
                if (new.df.defined == 0) { 
                    new.df = data.frame(newsFlow = newsFlow[ii, cp]);
                    new.df.defined = 1;
                } else {
                    new.df$newsFlow = newsFlow [ii, cp];
                }
            }

            glm.fit = glm(y~., data = df, family=binomial);
            
            # All probabilities in the training set
            #glm.prb = predict(glm.fit, type="response");

            # Probability in the test set
            glm.prb = predict(glm.fit, new.df, type="response");
            
            #print(df);
            #print(summary(glm.fit));
            #print(summary(glm.prb));
            #print(new.df);
            #print(glm.prb);

            probLogit = as.numeric(glm.prb) - 0.5;

            pred = 0;
    
            if (probLogit > settingsReturn$logitTolerance) {
                pred = mean(returns[returns > 0]);
                pred = 0.015;
                pred = 0.015 + (0.02 * (2*probLogit));
                #if (nextReturn > 0) {
                #    predRet = nextReturn;
                #}
            } else if (probLogit < -1*settingsReturn$logitTolerance) {
                pred = mean(returns[returns < 0]);
                pred = -0.015;
                pred = -0.015  - (-0.02 * (2*probLogit));
                #if (nextReturn < 0) {
                #    predRet = nextReturn;
                #}
            }
            predRet[i,j] <<- pred;
        }
    }   
    
    computedPredRet <<- 1;
    printf("Logistic regression computed in %.2fs\n", (proc.time() - TIME)[3]);
},

populatePortfolioObject = function(PORT) {
    
    if (!class(PORT) == "portfolio") stop0("Error in populatePortfolioObject: Object must be of class portfolio.");
    
    ###############################
    PORT$params['logReturns']  = logReturns;
    PORT$params['actualDate1'] = actualInitialDate;
    PORT$params['actualDate2'] = actualFinalDate;
        
    if (limitToTopCompanies) {
        PORT$params['limitToTopCompanies']   = sprintf("%d", limitToTopCompanies);
        PORT$params['limitByPercentageMV']   = sprintf("%d", limitByPercentageMV);
        PORT$params['valueTopCompanies']     = valueTopCompanies;
    }
    
    PORT$firstTrade              = firstTrade; 
    PORT$nameBenchmark           = settingsTrading$index; 
    PORT$dates                   = dates; 
    PORT$companies               = companiesIds; 
    PORT$rebalances              = rebalances;
    PORT$benchmark               = indexPrices; 


    return(PORT);   
},


# Statistics

accuracyReturnPrediction = function() {

    if (computedPredRet == 0) {
        printf("WARNING in accuracyReturnPrediction: Predicted return was not yet computed, no statistics will be shown.");
        return;
    }

    total = 0;
    right = 0;
    wrong = 0;
    for (i in firstTrade : (numDates-1)) {
        for (j in 1 : numCompaniesTraded) {
            company = companiesTraded[i, j];
            total = total + 1;

            # LOGIT
            rw = rightOrWrong(predRet[i,j], returns[(i+1), company], tolerance = tolerance);
            if (rw > 0)      {right = right + 1;}
            else if (rw < 0) {wrong = wrong + 1;}
            

        }
    }
    printf("\n----------\nAccuracy of returns prediction:\n");
    printf("Total:   %4d\n", total);
    printf("Right:   %4d (%5.2f total), (%5.2f known)\n", right, 100*right/total, 100*right/(right+wrong));
    printf("Wrong:   %4d\n", wrong);
    printf("Unknown: %4d\n", total - right - wrong);
    printf("----------\n");



},


removeInvalidPricesDataObj = function() {

    firstDate = settingsTrading$initialDate;

    toRemove = numeric();
    for (i in 2 : length(indexPrices)) {
        if (indexPrices[i] == indexPrices[i-1]) {
            toRemove = c(toRemove, i);
        } else {
            p = prices[i,];
            if (length(p) == length(p[p == -1])) {
                toRemove = c(toRemove, i);
            }
        }
    }
    if (length(toRemove) > 0) {
        for (i in seq(length(toRemove), 1, by=-1)) {
            indexPrices <<- indexPrices[-toRemove[i]];
            futures     <<- futures[-toRemove[i]];
            prices      <<- as.matrix(prices[-toRemove[i],]);
            dates       <<- dates[-toRemove[i]];

            if (length(indexHigh) > 0) indexHigh <<- indexHigh[-toRemove[i]];
            if (length(indexLow) > 0)  indexLow  <<- indexLow [-toRemove[i]];
            if (length(indexOpen) > 0) indexOpen <<- indexOpen[-toRemove[i]];
            if (nrow(low) > 0)         low       <<- as.matrix(low [-toRemove[i],]);
            if (nrow(high) > 0)        high      <<- as.matrix(high[-toRemove[i],]);
            if (nrow(open) > 0)        open      <<- as.matrix(open[-toRemove[i],]);
        }
    }

    # Find index of first date newer than firstDate
    ind = 0;
    for (i in 1 : length(dates)) {
        if (dates[i] >= firstDate) {
            ind = i;
            break;
        }
    }
    if (ind == 0) stop0("Out-of-sample period is too short.");
   

    ind = ind - settingsStrategy$necessaryDaysBefore;
    
    if (ind >= 1) {
        indexPrices <<- indexPrices[-(1:ind)];
        futures     <<- futures[-(1:ind)];
        prices      <<- as.matrix(prices[-(1:ind),]);
        dates       <<- dates[-(1:ind)];

        if (length(indexHigh) > 0) indexHigh <<- indexHigh[-(1:ind)];
        if (length(indexLow) > 0)  indexLow  <<- indexLow [-(1:ind)];
        if (length(indexOpen) > 0) indexOpen <<- indexOpen[-(1:ind)];
        if (nrow(low) > 0)         low       <<- as.matrix(low [-(1:ind),]);
        if (nrow(high) > 0)        high      <<- as.matrix(high[-(1:ind),]);
        if (nrow(open) > 0)        open      <<- as.matrix(open[-(1:ind),]);

    }
    #for (i in 1 : length(dates)) {
    #    if (dates[i] >= firstDate) {
    #        ind = i;
    #        break;
    #    }
    #}
    #printf("ind = %d\n", ind);

    firstTrade <<- settingsStrategy$necessaryDaysBefore;
}




################

    )
)









