#
# Given a matrix of prices, where each column represents daily/weekly/monthly prices 
# of a certain company, compute returns.
#
# Parameters:
#
# prices   -> matrix of prices.
# dates    -> not necessary, but list of dates matching the prices.
# 
# Return:
#  - returns    -> matrix of returns, first return is zero.

.calculateReturns <- function(prices, dates=numeric(), muteTimePrint=TRUE, excludeFirstRow=FALSE) {

    TIME = proc.time();
    options(warn=-1);
    if (!exists("prices")) {
        stop0("Prices not defined");
    }
    if (length(dates) > 0 && is.matrix(prices) && nrow(prices) != length(dates)) {
        stop0("Dates length does not match prices length.");
    }
    if (length(dates) > 0 && is.vector(prices) && length(prices) != length(dates)) {
        stop0("Dates length does not match prices length.");
    }
    if (is.matrix(prices)) {
        returns = diff(prices)/prices[-nrow(prices),];
    } else {
        returns = diff(prices)/prices[-length(prices)];
    }
    returns = replace(returns, is.nan(returns), 0);
    returns = replace(returns, returns <= -1, 0);
    
    if (!excludeFirstRow) {
        if (is.vector(returns)) {
            returns = c(0, returns);
        } else if (is.matrix(returns)) {
            returns = rbind(rep(0, ncol(returns)), returns);
        }
    } else if (length(dates) > 1) {
        dates = dates[2:length(dates)];
    }

    #if (length(dates) > 1) {
    #    dates = dates[2:length(dates)];
    #}
    if (!muteTimePrint) printf("Calculate returns in          %.2fs\n", (proc.time() - TIME)[3]);
    return(list("dates"=dates, "returns"=returns));
    options(warn=0);
    ##########################
}

#
# Given a matrix of prices, where each column represents daily/weekly/monthly prices 
# of a certain company, compute log returns.
#
# Parameters:
#
# prices   -> matrix of prices.
# dates    -> not necessary, but list of dates matching the prices.
# 
# Return:
#  - logReturns -> matrix of returns, containing (nrow - 1) returns.
#  - dates      -> list of dates, containing (nrow - 1) dates.
.calculateLogReturns <- function(prices, dates=numeric(), muteTimePrint=FALSE, excludeFirstRow = FALSE) {


    TIME = proc.time();
    options(warn=-1);
    if (!exists("prices")) {
        stop0("Prices not defined");
    }
    if (length(dates) > 0 && is.matrix(prices) && nrow(prices) != length(dates)) {
        stop0("Dates length does not match prices length.");
    }
    if (length(dates) > 0 && is.vector(prices) && length(prices) != length(dates)) {
        stop0("Dates length does not match prices length.");
    }
    returns = diff(log(prices));
    returns = replace(returns, is.nan(returns), 0);

    if (!excludeFirstRow) {
        if (is.vector(returns)) {
            returns = c(0, returns);
        } else if (is.matrix(returns)) {
            returns = rbind(rep(0, ncol(returns)), returns);
        }
    } else  if (length(dates) > 1) {
        dates = dates[2:length(dates)];
    }
    if (!muteTimePrint) printf("Calculate returns in          %.2fs\n", (proc.time() - TIME)[3]);
    return(list("dates"=dates, "logReturns"=returns));
    options(warn=0);
}

#
# Given a matrix of returns, compute the rolling volatility.
#
# Parameters:
#
# returnsData       -> matrix of returns.
# maxDaysVolatility -> maximum of past days used to compute the volatility.
# volMeasure        -> "sd" for standard deviation, "var" for variance
# 
# Return:
#  - volatility -> matrix of volatility data
.calculateVolatility <- function(returnsData=matrix(, nrow=0, ncol=0), maxDaysVolatility=10, volMeasure = "sd", muteTimePrint=FALSE) {


    TIME = proc.time();
    if (!muteTimePrint) printf("\nComputing volatility...\n");
    options(warn=-1);
  
    


    if (!exists("returnsData"))  stop0("Matrix of returnsData not defined");
    if (is.numeric(returnsData)) {
        returnsData = as.matrix(returnsData);
    }
    if (!is.matrix(returnsData)) stop0("Returns are not in matrix format");
    if (volMeasure != "sd" && volMeasure != "var") stop0("volMeasure must be \"sd\" or \"var\"");
    if (nrow(returnsData) == 0 || ncol(returnsData) == 0)  stop0("Returns has zero rows or columns");
    if (nrow(returnsData) <= maxDaysVolatility)  stop0("Very few days to calculate volatility");
    

    volatility = matrix(0, nrow=nrow(returnsData), ncol=ncol(returnsData));
    lastReturn = min(maxDaysVolatility, nrow(returnsData));
    for (i in 2 : lastReturn) {
        if (volMeasure == "sd") {
            if (ncol(returnsData) == 1) {
                volatility[i,1] = sd(returnsData[1:i,1]);
            } else {
                volatility[i,] = apply(returnsData[1:i,], 2, sd)
            }
        } else {
            if (ncol(returnsData) == 1) {
                volatility[i,1] = var(returnsData[1:i,1]);
            } else {
                volatility[i,] = apply(returnsData[1:i,], 2, var)
            }
        }
    }
    for (i in (maxDaysVolatility+1) : nrow(returnsData)) {
        if (volMeasure == "sd") {
            if (ncol(returnsData) == 1) {
                volatility[i,1] = sd(returnsData[(i-maxDaysVolatility+1):i,1]);
            } else {
                volatility[i,] = apply(returnsData[(i-maxDaysVolatility+1):i,], 2, sd);
            }
        } else {
            if (ncol(returnsData) == 1) {
                volatility[i,1] = var(returnsData[(i-maxDaysVolatility+1):i,1]);
            } else {
                volatility[i,] = apply(returnsData[(i-maxDaysVolatility+1):i,], 2, var);
            }
        }
    } 

    #print(volatility);
    
    if (!muteTimePrint) printf("Volatility calculated in  %.2fs\n", (proc.time() - TIME)[3]);
    return(volatility);
    options(warn=0);
}


#
# Compute a matrix of lag data given a vector 
#
# Parameters:
#
# originalData       -> vector of data.
# dates              -> not necessary, but list of dates matching the prices.
# numberOfLags       -> Number of lags to calculate
# 
# Return:
#  - lagReturns -> matrix of data with lags, containing (numberOfLags+1) columns.
#  - dates      -> list of dates, containing (dates-numberOfLags) dates.
#  - error:     -> if 1 then there was an error
#  - msg:       -> if error=1 then this holds the error message

.computeLagData <- function(originalData=numeric(), numberOfLags=3, dates=numeric()) {


    if (!exists("originalData")) {
        stop0("Prices not defined");
    }
    if (is.matrix(originalData)) {
        if (ncol(originalData) > 1) stop0("If data isa matrix data should ");
        originalData = as.numeric(originalData);
    }
    if (!is.vector(originalData)) {
        stop0("Returns should be a vector");
    }
    if (length(dates) > 0 && length(originalData) != length(dates)) {
        stop0("Dates length does not match originalData length.");
    }
    if (numberOfLags >= length(originalData)) {
        stop0("Number of return lags is greater or equal than the length of the originalData vector.");
    }
   
    #print(originalData);
    lagData = matrix(0, nrow=(length(originalData)-numberOfLags), ncol=(1+numberOfLags));
    for (i in 1 : (numberOfLags+1)) {
        lagData[,i] = originalData[(numberOfLags+2-i):(length(originalData)+1-i)];
    }
            
    
    if (length(dates) > numberOfLags) {
        dates = dates[(numberOfLags+1):length(dates)];
    }
    return(list("dates"=dates, "lagData"=lagData));
    options(warn=0);
    ##########################
}



# Function that returns the coefficients (intercept and slopes) for 
# multiple linear (or quantile) regression.
#
# Parameters:
#  - dataMatrix      -> Matrix of data, first column will be regressed against the remaining columns
#  - typeRegression  -> "ls" -> Least squares regression
#                       "qt" -> Quantile regression with tau = qtRegressionTau
#  - qtRegressionTau -> Tau for the quantile regression, useless if least-squares regression is used
#  - singleFit       -> If TRUE fit only against last column
#
# 
# Return:
# - coeffs     -> Vector of coefficients, first position is the intercept.
.fitData <- function(dataMatrix = matrix(0, nrow=0, ncol=0), typeRegression="ls", qtRegressionTau = 0.5, singleFit = FALSE){

    if (!is.matrix(dataMatrix)) stop0("Data matrix is not a matrix.");
    if (ncol(dataMatrix) < 2)   stop0("Data matrix has less than two columns.");
    if (nrow(dataMatrix) < 1)   stop0("Data matrix has no rows.");
    if (typeRegression != "ls" && typeRegression != "qt") {
        stop0("Type regression must be either \"ls\" (least-squares) or \"qt\" (quantile).");
    }
    if (typeRegression == "qt" && !"quantreg" %in% rownames(installed.packages())) {
        stop0("To use quantile regression you must install package \"quantreg\".");
    }
    
    fit = NULL;
    if (typeRegression == "ls") {
        if (!singleFit) {
            fit = lm(formula = dataMatrix[,1] ~ dataMatrix[,2:ncol(dataMatrix)]);
        } else {
            fit = lm(formula = dataMatrix[,1] ~ dataMatrix[,ncol(dataMatrix)]);
        }
    } else if (typeRegression == "qt") {
        suppressMessages(library("quantreg"));
        if (!singleFit) {
            fit = rq(formula = dataMatrix[,1] ~ dataMatrix[,2:ncol(dataMatrix)], tau = qtRegressionTau);
        } else {
            fit = rq(formula = dataMatrix[,1] ~ dataMatrix[,ncol(dataMatrix)], tau = qtRegressionTau);
        }
    }
    if (is.null(fit)) stop0("Error: Fit did not work.");

    coeffs = as.numeric(fit$coefficients);
    return(coeffs);
}

.fitGarchNew <- function(dataVector = numeric(), addExtraFactors = FALSE, predictReturn=FALSE, returnCoeffs=FALSE, ...) {
    if (!is.numeric(dataVector)) stop0("Data vector is not a vector.");
    if (length(dataVector) < 2)    stop0("Data vector does not have enough rows.");
    
    extraFactors = matrix();
    if (addExtraFactors) {
        ef = list(...);
        for (i in seq_along(ef)) {
            extraData = ef[[i]];
            if (!is.numeric(extraData)) stop0("Extra vectors must be numeric.");
            if (is.numeric(extraData) && length(extraData) != length(dataVector)) stop0("Extra factor and data have different lengths.");
        }
        extraFactors = do.call(cbind, ef);
    }

    lastData = tail(dataVector, 1);
    
    suppressMessages(library("tseries"));
    suppressMessages(library("fGarch"));
    #fit = garch(dataVector, order=c(1, 1), control = garch.control(trace = FALSE));
    fit = garchFit(data=dataVector, formula=~garch(1, 1), trace = FALSE);
  
    #predErrado = .garchFit.predict.one.day(fit, lastData);
    prediction = predict(fit, n.ahead=1);
    predFunc = prediction$standardDeviation;
    predRet  = prediction$meanForecast;
   

    resid    = fit@residuals;
    condVar  = fit@h.t;
    coeffs   = as.numeric(coef(fit));
    lastVar  = tail(sqrt(condVar), 1);
    ## Function uses z, i don't know what this is
    what     = fit@fit$series$z;
    lastData = tail(what, 1);


#    print(coef(fit));

    pred = coeffs[2];
    pred = pred + coeffs[3] * lastData^2;
    pred = pred + coeffs[4] * lastVar^2;
    pred = sqrt(pred);

    predExtra = 0;
    allCoeffs = c(coeffs[2], coeffs[3], coeffs[4]);
    if (addExtraFactors) {
        # ARBEX Change to two impacts
        fitlm = lm(formula = resid^2 ~ extraFactors);
        coeffslm = as.numeric(fitlm$coefficients);
        #printf("Coeffs: \n");
        #print(coeffslm);
        #printf("\n");

        if (!any(is.na(coeffslm))) {
            predExtra = coeffslm[1];
            allCoeffs = c(coeffslm[1], coeffs[3], coeffs[4]);
            for (i in 2 : length(coeffslm)) {
                predExtra = predExtra + coeffslm[i] * extraFactors[nrow(extraFactors),i-1];
                allCoeffs = c(allCoeffs, coeffslm[i]);
            }
            predExtra = predExtra + coeffs[3] * lastData^2;
            predExtra = predExtra + coeffs[4] * lastVar^2;
            predExtra = sqrt(predExtra);
            pred = predExtra;
        }
    }

#    ####################
#    # tseries
#    suppressMessages(library("tseries"));
#    fit2 = garch(dataVector, order = c(1, 1), control = garch.control(trace = FALSE));
#    predModel2 = 0;
#    if(!is.logical(fit2)) {
#        h.last = tail( fitted(fit2)[,1] ,1);
#        predModel2 = sqrt(sum( coef(fit2) * c(1,  lastData^2, h.last^2) ));
#    }
#    ####
#
#
#    ####################
#    ## Rugarch
#    suppressMessages(library("rugarch"));
#    predModel3 = 0;
#    spec = ugarchspec();
#    #show(spec);
#    fit3 = ugarchfit(spec, dataVector);
#    #show(fit3);
#    forecast = ugarchforecast(fit3, n.ahead=1, data=dataVector);
#    predModel3 = forecast@forecast$sigmaFor[1,1];
#    coeffsRugarch = fit3@fit$coef;
#    
#    print(coeffsRugarch);
#
#    predRu = coeffsRugarch[4];
#    predRu = predRu + coeffsRugarch[5] * lastData^2;
#    predRu = predRu + coeffsRugarch[6] * lastVar^2;
#    predRu = sqrt(predRu);
#    ####################
#   
#
#    #printf("PREE = %.10f\n", predExtra);
#    printf("PRED FUNC = %.10f\n", predFunc);
#    printf("PRED CALC = %.10f\n", pred);
#    printf("PRED MOD2 = %.10f\n", predModel2);
#    printf("PRED MOD3 = %.10f\n", predModel3);
#    printf("PRED RUGR = %.10f\n", predRu);
#
   
    if (returnCoeffs) {
        return(allCoeffs);
    } else {
        if (predictReturn) {
            return(list("vol"=pred, "ret"=predRet));
        } else {
            return(pred);
        }
    }
}



.fitGarch <- function(dataVector = numeric(), impact=numeric(), impactPos=numeric(), impactNeg=numeric(),
                     nextImpact=0, nextImpactPos=0, nextImpactNeg=0, useNews=FALSE, numberOfLags=1, useOneImpact=TRUE) {


    if (!is.numeric(dataVector)) stop0("Data vector is not a vector.");
    if (length(dataVector) < 2)    stop0("Data vector does not have enough rows.");
    
    #suppressMessages(library("tseries"));
    #garchResult = garch(dataVector, order=c(numberOfLags, numberOfLags), control = garch.control(trace = FALSE));
    suppressMessages(library("fGarch"));
    garchResult = garchFit(data=dataVector, formula=~garch(1, 1), trace = FALSE);
   

    # Coeff 1 is mu, not used
    # Coeff 2 is intercept
    # Coeff 3 is alpha from RESIDUAL
    # Coeff 4 is beta from VOLATILITY
    coeffs = as.numeric(coef(garchResult));
    
    # Both are the same for residuals (fGarch)
    #resid    = garchResult@fit$series$z;
    resid    = garchResult@residuals;
    condVar  = garchResult@h.t;

    pred = 0;

    # This test is to check if conditional variance is "the real thing" or
    # the predicted volatility from previous time periods, answer is it is the
    # predicted volatility from previous time periods
    #   predA = coeffs[2];
    #   predA = predA + coeffs[4] * condVar[length(condVar)-1];
    #   predA = predA + coeffs[3] * (resid[length(resid)-1]^2);
    #   printf("Cond var: %.10f\n", condVar[length(condVar)]);
    #   printf("Pred a:   %.10f\n", predA);


    #print(predict(garchResult, n.ahead=1));
    if (!useNews) {
        pred = coeffs[2];
        for (i in 1 : numberOfLags) {
            pred = pred + coeffs[2+numberOfLags+i] * condVar[length(condVar)+1-i];
            pred = pred + coeffs[2+i] * (resid[length(resid)+1-i]^2);
        }

    } else {
        firstIndex = 1;
        for (i in 1 : length(condVar)) {
            if (is.na(condVar[i])) {
                firstIndex = firstIndex + 1;
            }
        }
        
        # ARBEX Change to two impacts
        if (useOneImpact) {
            fit = lm(formula = resid[firstIndex:length(resid)]^2 ~ impact[firstIndex:length(impact)]);
        } else {
            fit = lm(formula = resid[firstIndex:length(resid)]^2 ~ impactPos[firstIndex:length(impactPos)] + impactNeg[firstIndex:length(impactNeg)]);
        }

        coeffs2 = as.numeric(fit$coefficients);

        if (any(is.na(coeffs2))) {
            pred = coeffs[2];
            for (i in 1 : numberOfLags) {
                pred = pred + coeffs[2+numberOfLags+i] * condVar[length(condVar)+1-i];
                pred = pred + coeffs[2+i] * (resid[length(resid)+1-i]^2);
            }

        } else {

            pred = 0;
            if (useOneImpact) {
                #pred = coeffs2[1] + coeffs2[2] * nextImpact;
                pred = coeffs2[1] + coeffs2[2] * impact[length(impact)];
            } else {
                #pred = coeffs2[1] + coeffs2[2] * nextImpactPos + coeffs2[3] * nextImpactNeg;
                pred = coeffs2[1] + coeffs2[2] * impactPos[length(impactPos)] + coeffs2[3] * impactNeg[length(impactNeg)];
            }
            for (i in 1 : numberOfLags) {
                pred = pred + coeffs[2+numberOfLags+i] * (condVar[length(condVar)+1-i]);
                pred = pred + coeffs[2+i] * (resid[length(resid)+1-i]^2);
            }
            #printf("Formula: %.8f + %.8f*%.2f + %.6f*%.8f + %.6f*%.8f = %.8f\n", coeffs2[1], coeffs2[2], nextImpact, coeffs[2+numberOfLags+i], (condVar[length(condVar)+1-i]), coeffs[2+i], resid[length(resid)+1-i]^2, pred);
            if (pred < 0) {
                pred = coeffs[2];
                for (i in 1 : numberOfLags) {
                    pred = pred + coeffs[2+numberOfLags+i] * condVar[length(condVar)+1-i];
                    pred = pred + coeffs[2+i] * (resid[length(resid)+1-i]^2);
                }
            }
        }
    }
    #printf("pred = %.10f, sqrt = %.10f\n", pred, sqrt(pred));

    return(pred);

}

    # Q3) Why is the meanForecast for all 10 future periods the same?
    
    # Because you have just modeled the variance equation and the mean
    # equation has only a constant. Garch will help you forecasting the
    # variance of the returns but not the returns themselves. If you want
    # return forecasts you should model the mean equation, e.g. 

    # fgarch.fitted <- fGarch::garchFit(~ arma(2,1)+garch(1,1), data =
    # as.zoo(na.omit(r)), trace =  FALSE)

    # > predict(fgarch.fitted, n.ahead = 5)
    #    meanForecast   meanError standardDeviation
    # 1 -0.0009371963 0.008495794       0.008495794
    # 2 -0.0003940789 0.008556624       0.008539586
    # 3  0.0004374707 0.008608828       0.008582793
    # 4  0.0005284133 0.008651758       0.008625429
    # 5  0.0005093462 0.008693970       0.008667503

    # My example
    # garchResult = garchFit(~ arma(1,1)+garch(1,1), data=a);
    # predict(garchResult, n.ahead=1)


.fitArch <- function(dataVector = numeric()) {

    if (!is.numeric(dataVector)) stop0("Data vector is not a vector.");
    if (length(dataVector) < 2)    stop0("Data vector does not have enough rows.");
    
    suppressMessages(library("fGarch"));
    
    arimaResult = arima(dataVector, order = c(1, 0, 1), method = "ML");
    predictedReturn = as.numeric(predict(arimaResult, n.ahead=1)$pred);

    return(predictedReturn);
}


# FUNCTION <--- calculateImpactRavenpack
#
# Calculate the news impact for Ravenpack data.
#
# Parameters
#  
# newsData         -> Data frame of news data (the return from function loadNewsDataRavenpack)
# dates            -> List of dates for which the impact will be calculated 
# computeImpact      -> If TRUE there will be a single impact measure with news from all companies together.
#                     If FALSE there will be one impact measure per company
# companies        -> List of companies (their ravenpack codes), necessary if computeImpact = companies or both 
# categoryData     -> Hash table containing pairs (category, weights)
# finalHour        -> Hour (UTC) of the impact calculation (format HHMMSS)
# utcDifference    -> Difference from the market to UTC time (in hours)
# minutesDecay     -> How many minutes so that the impact decays to half its value
# thresholdImpact  ->
# marketCap        -> Matrix of market caps, optional, if exists then it is included in impact
# debug            -> TRUE to print debug information
#
#
# Return
#  - impact:    -> If computeImpact is "companies", a matrix of impacts, dimensions (number of companies) x (number of dates).
#               -> If computeImpact is "index", a list of impacts of dimension (number of dates).
#               -> If computeImpact is "both", a matrix of impacts where the first column is the index.
#  - error:     -> if 1 then there was an error
#  - msg:       -> if error=1 then this holds the error message
.calculateImpactRavenpack <- function(newsData=NULL, dates=numeric(), computeImpact="companies", companies=character(), thresholdImpact=0.01,
                                      categoryData=hash(), finalHour="160000", utcDifference = 0, minutesDecay=90, 
                                      debug=0, marketCap=NULL) {
    
    TIME = proc.time(); 
    printf("\nCalculating impact...\n");

    if (computeImpact != "companies" && computeImpact != "index" && computeImpact != "both") stop0("Impact: computeImpact must be companies, index or both");

    .errorCheckHour(finalHour);
    .errorCheckUTC(utcDifference);
    .errorCheckCategoryData(categoryData);
    .errorCheckNewsDataRavenpack(newsData);
    .errorCheckNumericVector("Dates", dates);
    if (computeImpact == "companies" || computeImpact == "both") {
        .errorCheckCompanies(companies);
    }
    if (!is.character(companies)) stop0("Error: Companies is optional but if passed it must be a character vector.");

    numDates = length(dates);
    numCompanies = length(companies);
    
    marketCapIncluded = FALSE;
    if (numCompanies > 0) {
        if (is.matrix(marketCap) && ncol(marketCap) == (numCompanies+1)) {
            marketCapIncluded = TRUE;
        }
    }
    
    
    numNews = nrow(newsData);

    impact1      = rep(0, numDates);
    impactPos1   = rep(0, numDates);
    impactNeg1   = rep(0, numDates);
    newsFlow1    = rep(0, numDates);
    newsFlowPos1 = rep(0, numDates);
    newsFlowNeg1 = rep(0, numDates);
        
    impact2      = matrix(0, nrow=numDates, ncol=numCompanies);
    impactPos2   = matrix(0, nrow=numDates, ncol=numCompanies);
    impactNeg2   = matrix(0, nrow=numDates, ncol=numCompanies);
    newsFlow2    = matrix(0, nrow=numDates, ncol=numCompanies);
    newsFlowPos2 = matrix(0, nrow=numDates, ncol=numCompanies);
    newsFlowNeg2 = matrix(0, nrow=numDates, ncol=numCompanies);

    indexCompanies = hash();
    for (i in 1: length(companies)) {
        indexCompanies[companies[i]] = i;
    }


    lambda = calculateLambda(minutesDecay);

    currentNews = 1;
   

    updatedHour = updateHourTimezone(as.numeric(finalHour), utcDifference);
    daysAdjusted = updatedHour$daysAdjusted;
    updatedHour = updatedHour$hour;
    
    #printf("Final hour = %06d, daysAdjusted %d, utc: %.2f, original %06d\n", updatedHour, daysAdjusted, utcDifference, as.numeric(finalHour));

    minutesWeGoBack = thresholdForImpactCalculation(lambda=lambda, threshold = thresholdImpact);
    timeFor = proc.time();
    

    #print(marketCap);
    currentMarketCapIndex = 1;
    currentMarketCapDate = 0;
    if (marketCapIncluded) currentMarketCapDate = marketCap[1, ncol(marketCap)];


    for (i in 1 : numDates) {
        currentDate = dates[i];
        
        if (marketCapIncluded) {
            if (currentMarketCapIndex < nrow(marketCap) && marketCap[(currentMarketCapIndex+1), ncol(marketCap)] <= currentDate) {
                currentMarketCapIndex = currentMarketCapIndex + 1; 
                currentMarketCapDate = marketCap[(currentMarketCapIndex), ncol(marketCap)];
            }
        }
        #printf("DATES: %d ---> %d and %d\n", currentDate, currentMarketCapDate, currentMarketCapIndex);


        if (daysAdjusted != 0) currentDate = addDate(currentDate, daysAdjusted);
        initialDateHour = subtractMinutes(date1=currentDate, hour1=updatedHour, minutes = minutesWeGoBack);
        initialDate = initialDateHour$date;
        initialHour = initialDateHour$hour;
        
        initialDateNext = 0;
        initialHourNext = 0;
        if (i < numDates) {
            initialDateHourNext = subtractMinutes(date1=dates[i+1], hour1=updatedHour, minutes = minutesWeGoBack);
            initialDateNext = initialDateHourNext$date;
            initialHourNext = initialDateHourNext$hour;
        }

        if (debug) printf("---- DATE %d ---------------\nImpact to include news from %d, %06d to %d, %06d\n", currentDate, initialDate, initialHour, currentDate, updatedHour);
        
        tempNewsIndex = currentNews;
        foundFirst = FALSE;
        for (j in currentNews : numNews) {
            dateNews = newsData$date[j];
            hourNews = newsData$hour[j];
            if (debug) printf("NEWS %03d cp %7s, s %02d, %d %06d -> ", j, newsData$company[j], newsData$score[j], dateNews, hourNews);
            if (dateNews > initialDate || (dateNews == initialDate && hourNews > initialHour)) {
                if (!foundFirst && (dateNews > initialDateNext || (dateNews == initialDateNext && hourNews > initialHourNext))) {
                    tempNewsIndex = j;
                    foundFirst = TRUE;
                }
                if (dateNews < currentDate || (dateNews == currentDate && hourNews < updatedHour)) {
                    catWeight = 1;
                    marketWeight = 1;
                    if (newsData$categoryGroup[j] != "" && has.key(newsData$categoryGroup[j], categoryData)) {
                        catWeight = as.numeric(categoryData[[newsData$categoryGroup[j]]]);
                    }
                    if (marketCapIncluded) {
                        marketWeight = marketCap[currentMarketCapIndex, indexCompanies[[newsData$company[j]]]]
                    }
                    if (debug) printf("cat:%.1f mw:%.3f ", catWeight, marketWeight);
 
                    #timeTemp = proc.time();
                    score = (newsData$score[j] - 50);
                    imp = marketWeight * catWeight * score * exp(-1 * lambda * (differenceMinutes(dateNews, hourNews, currentDate, updatedHour)/90));
                    
                    cind = indexCompanies[[newsData$company[j]]];
                    if (debug) printf(" (imp %d before %.5f) ", cind, impact[i,cind]);
                    
                    impact1[i]        = impact1[i] + imp;
                    impact2[i,cind]   = impact2[i,cind] + imp;

                    newsFlow1[i]      = newsFlow1[i] + 1;
                    newsFlow2[i,cind] = newsFlow2[i,cind] + 1;

                    if (score > 0) {
                        impactPos1[i]        = impactPos1[i] + imp;
                        impactPos2[i,cind]   = impactPos2[i,cind] + imp;
                        newsFlowPos1[i]      = newsFlowPos1[i] + 1;
                        newsFlowPos2[i,cind] = newsFlowPos2[i,cind] + 1;
                    } else {
                        impactNeg1[i]        = impactNeg1[i] + imp;
                        impactNeg2[i,cind]   = impactNeg2[i,cind] + imp;
                        newsFlowNeg1[i]      = newsFlowNeg1[i] + 1;
                        newsFlowNeg2[i,cind] = newsFlowNeg2[i,cind] + 1;
                    }

                    if (debug) printf("add %.5f to impact (total so far %.5f)\n", imp, impact[i,cind]);


                } else {
                    if (debug) printf("past current date, do not include\n");
                    if (!foundFirst) {
                        tempNewsIndex = j+1;
                    }
                    break;
                }
            } else {
                if (debug) printf("skip this news\n");
            }
        }
        currentNews = tempNewsIndex;
    }
    #timeFor = (proc.time() - timeFor)[3];

    impact      = numeric(numDates);
    impactPos   = numeric(numDates);
    impactNeg   = numeric(numDates);
    newsFlow    = numeric(numDates);
    newsFlowPos = numeric(numDates);
    newsFlowNeg = numeric(numDates);
    if (computeImpact == "index") {
        impact      = impact1;
        impactPos   = impactPos1;
        impactNeg   = impactNeg1;
        newsFlow    = newsFlow1;
        newsFlowPos = newsFlowPos1;
        newsFlowNeg = newsFlowNeg1;
    } else if (computeImpact == "companies") {
        impact      = impact2;
        impactPos   = impactPos2;
        impactNeg   = impactNeg2;
        newsFlow    = newsFlow2;
        newsFlowPos = newsFlowPos2;
        newsFlowNeg = newsFlowNeg2;
    } else if (computeImpact == "both") {
        impact      = cbind(impact1,    impact2);
        impactPos   = cbind(impactPos1, impactPos2);
        impactNeg   = cbind(impactNeg1, impactNeg2);
        newsFlow    = cbind(newsFlow1,  newsFlow2);
        newsFlowPos = cbind(newsFlowPos1,  newsFlowPos2);
        newsFlowNeg = cbind(newsFlowNeg1,  newsFlowNeg2);
    }

    TIME = (proc.time() - TIME)[3];
    printf("Function finished in          %.2fs\n", TIME);
    #printf("Time iterating through data   %.2fs\n", timeFor);
    


    return(list("impact"=impact, "impactPos"=impactPos, "impactNeg"=impactNeg, "newsFlow"=newsFlow, "newsFlowPos"=newsFlowPos, "newsFlowNeg"=newsFlowNeg));

}




# TODO -> calculateImpact for ThomsonReuters has a bug, not working properly
#         should redo it similarly to Ravenpack's



# FUNCTION <--- calculateImpactThomsonReuters
#
# Calculate the news impact for Thomson Reuters data.
#
# Parameters
#  
# newsData         -> Data frame of news data (the return from function loadNewsDataRavenpack)
# dates            -> List of dates for which the impact will be calculated 
# computeImpact      -> If TRUE there will be a single impact measure with news from all companies together.
#                     If FALSE there will be one impact measure per company
# companies        -> List of companies (their ravenpack codes), necessary if computeImpact = companies or both
# finalHour        -> Hour (UTC) of the impact calculation (format HHMMSS)
# utcDifference    -> Difference from the market to UTC time (in hours)
# minutesDecay     -> How many minutes so that the impact decays to half its value
# thresholdImpact  ->
# marketCap        -> Matrix of market caps, optional, if exists then it is included in impact
# debug            -> TRUE to print debug information
#
#
# Return
#  - impact:    -> If computeImpact is "companies", a matrix of impacts, dimensions (number of companies) x (number of dates).
#               -> If computeImpact is "index", a list of impacts of dimension (number of dates).
#               -> If computeImpact is "both", a matrix of impacts where the first column is the index.
#  - error:     -> if 1 then there was an error
#  - msg:       -> if error=1 then this holds the error message
.calculateImpactThomsonReuters <- function(newsData=NULL, dates=numeric(), computeImpact="companies", companies=character(), 
                                           thresholdImpact=0.01, finalHour="160000", utcDifference = 0, minutesDecay=90, debug=0, marketCap=NULL) {
    
    TIME = proc.time(); 
    printf("\nCalculating impact...\n");


    if (computeImpact != "companies" && computeImpact != "index" && computeImpact != "both") stop0("Impact: computeImpact must be companies, index or both");

    .errorCheckHour(finalHour);
    .errorCheckUTC(utcDifference);
    .errorCheckNewsDataThomsonReuters(newsData);
    .errorCheckNumericVector("Dates", dates);
    if (computeImpact == "companies" || computeImpact == "both") {
        .errorCheckCompanies(companies);
    }
    if (!is.character(companies)) stop0("Error: Companies is optional but if passed it must be a character vector.");

    numDates = length(dates);
    numCompanies = length(companies);
    
    marketCapIncluded = FALSE;
    if (numCompanies > 0) {
        if (is.matrix(marketCap) && ncol(marketCap) == (numCompanies+1)) {
            marketCapIncluded = TRUE;
        }
    }
    
    
    numNews = nrow(newsData);
    impact    = numeric(numDates);
    impactPos = numeric(numDates);
    impactNeg = numeric(numDates);
    newsFlow  = numeric(numDates);
    if (computeImpact == "companies") {
        impact    = matrix(0, nrow=numDates, ncol=numCompanies);
        impactPos = matrix(0, nrow=numDates, ncol=numCompanies);
        impactNeg = matrix(0, nrow=numDates, ncol=numCompanies);
        newsFlow  = matrix(0, nrow=numDates, ncol=numCompanies);
    }

    indexCompanies = hash();
    for (i in 1: length(companies)) {
        indexCompanies[companies[i]] = i;
    }

    lambda = calculateLambda(minutesDecay);

    currentNews = 1;

    updatedHour = updateHourTimezone(as.numeric(finalHour), utcDifference);
    daysAdjusted = updatedHour$daysAdjusted;
    updatedHour = updatedHour$hour;
    
    #printf("Final hour = %06d, daysAdjusted %d, utc: %.2f, original %06d\n", updatedHour, daysAdjusted, utcDifference, as.numeric(finalHour));

    minutesWeGoBack = thresholdForImpactCalculation(lambda=lambda, threshold = thresholdImpact);
    timeFor = proc.time();
    
    #print(marketCap);
    currentMarketCapIndex = 1;
    currentMarketCapDate = 0;
    if (marketCapIncluded) currentMarketCapDate = marketCap[1, ncol(marketCap)];

    for (i in 1 : numDates) {
        currentDate = dates[i];
        
        if (marketCapIncluded) {
            if (currentMarketCapIndex < nrow(marketCap) && marketCap[(currentMarketCapIndex+1), ncol(marketCap)] <= currentDate) {
                currentMarketCapIndex = currentMarketCapIndex + 1; 
                currentMarketCapDate = marketCap[(currentMarketCapIndex), ncol(marketCap)];
            }
        }
        #printf("DATES: %d ---> %d and %d\n", currentDate, currentMarketCapDate, currentMarketCapIndex);


        if (daysAdjusted != 0) currentDate = addDate(currentDate, daysAdjusted);
        initialDateHour = subtractMinutes(date1=currentDate, hour1=updatedHour, minutes = minutesWeGoBack);
        initialDate = initialDateHour$date;
        initialHour = initialDateHour$hour;
        
        initialDateNext = 0;
        initialHourNext = 0;
        if (i < numDates) {
            initialDateHourNext = subtractMinutes(date1=dates[i+1], hour1=updatedHour, minutes = minutesWeGoBack);
            initialDateNext = initialDateHourNext$date;
            initialHourNext = initialDateHourNext$hour;
        }

        if (debug) printf("---- DATE %d ---------------\nImpact to include news from %d, %06d to %d, %06d\n", currentDate, initialDate, initialHour, currentDate, updatedHour);
        
        tempNewsIndex = currentNews;
        foundFirst = FALSE;
        for (j in currentNews : numNews) {
            dateNews = newsData$date[j];
            hourNews = newsData$hour[j];
            if (debug) printf("NEWS %03d cp %7s, s %.2f, %d %06d -> ", j, newsData$company[j], newsData$score[j], dateNews, hourNews);
            if (dateNews > initialDate || (dateNews == initialDate && hourNews > initialHour)) {
                if (!foundFirst && (dateNews > initialDateNext || (dateNews == initialDateNext && hourNews > initialHourNext))) {
                    tempNewsIndex = j;
                    foundFirst = TRUE;
                }
                if (dateNews < currentDate || (dateNews == currentDate && hourNews < updatedHour)) {
                    marketWeight = 1;
                    if (marketCapIncluded) {
                        marketWeight = marketCap[currentMarketCapIndex, indexCompanies[[newsData$company[j]]]]
                    }
 
                    #timeTemp = proc.time();
                    score = (newsData$score[j] - 50);
                    imp = marketWeight * score * exp(-1 * lambda * (differenceMinutes(dateNews, hourNews, currentDate, updatedHour)/90));
                    
                    if (computeImpact == "companies") {
                        cind = indexCompanies[[newsData$company[j]]];
                        if (debug) printf(" (imp %d before %.5f) ", cind, impact[i,cind]);
                        
                        impact[i,cind]   = impact[i,cind] + imp;
                        newsFlow[i,cind] = newsFlow[i,cind] + 1;
                        if (score > 0) {
                            impactPos[i,cind] = impactPos[i,cind] + imp;
                        } else {
                            impactNeg[i,cind] = impactNeg[i,cind] + imp;
                        }

                        if (debug) printf("add %.5f to company's impact (total so far %.5f)\n", imp, impact[i,cind]);
                    } else {
                        impact[i] = impact[i] + imp;
                        newsFlow[i] = newsFlow[i] + 1;

                        if (score > 0) {
                            impactPos[i] = impactPos[i] + imp;
                        } else {
                            impactNeg[i] = impactNeg[i] + imp;
                        }

                        if (debug) printf("add %.5f to date impact (total so far %.5f)\n", imp, impact[i]);
                    }


                } else {
                    if (debug) printf("past current date, do not include\n");
                    if (!foundFirst) {
                        tempNewsIndex = j+1;
                    }
                    break;
                }
            } else {
                if (debug) printf("skip this news\n");
            }
        }
        currentNews = tempNewsIndex;
    }
    #timeFor = (proc.time() - timeFor)[3];
    
    TIME = (proc.time() - TIME)[3];
    printf("Function finished in          %.2fs\n", TIME);
    #printf("Time iterating through data   %.2fs\n", timeFor);
    
    return(list("impact"=impact, "impactPos"=impactPos, "impactNeg"=impactNeg, "newsFlow"=newsFlow));

}



.sortedInSampleReturns <- function(positions, returns) {
    
    if (!is.matrix(returns)) stop0("In sortedInSampleReturns, returns must be a matrix");
    if (!is.numeric(positions)) stop0("In sortedInSampleReturns, positions must be a numeric vector");
    if (length(positions) != ncol(returns)) stop0("In sortedInSampleReturns, length(positions) must be equal to ncol(returns)");

    return (sort(returns %*% positions));
}



# Deviation from average of past "days" days
.computeDeviationFromAverage <- function(data, days = 10, inSample=10) {
    if (!is.numeric(data))       stop0("Data should be a numeric vector.");
    if (length(data) < inSample) stop0("In sample is bigger than length of data vector.");
    if (inSample < (days))       stop0("In sample must be greater or equalt to days parameter.");

    avg = rep(0, length(data));

    for (i in inSample : length(data)) {
        avg[i] = data[i] - mean(data[(i-days+1):i]);
    }

    return(avg);
}


# RSI is computed as a score from -50 to 50
.computeRSI <- function(data, days = 14, emaRatio = 0.3333, inSample = 14) {

    if (!is.numeric(data))       stop0("Data should be a numeric vector.");
    if (length(data) < inSample) stop0("In sample is bigger than length of data vector.");
    if (inSample < (days+1))     stop0("In sample must be greater than days parameter.");

    suppressMessages(library("TTR"));

    rsi = rep(0, length(data));

    for (i in 1 : length(data)) {
    #for (i in inSample : length(data)) {

        end = i;
        begin = i - days;
        if (begin < 1) {
            next;
        }
        #################
        ## CALCULATE RSI
        POS = diff(data[begin:end]);
        NEG = POS;
        POS[POS < 0] = 0;
        NEG[NEG > 0] = 0;
        NEG = NEG * -1;
        
        rs = 100;
       
        emapos = EMA(POS, n=days, ratio=emaRatio);
        emapos = emapos[length(emapos)];
        emaneg = EMA(NEG, n=days, ratio=emaRatio);
        emaneg = emaneg[length(emaneg)];

        if (emaneg > 0) rs = emapos/emaneg;

        rsi[i] = (100 - (100 / (1 + rs))) - 50;
    }
    
    return(rsi);
}



# The function for computing the Ichimoku cloud
.computeIchimoku <- function(high, low, close, p1, p2, p3) {

    # Normal = 9, 26, 52    

    if (length(high) != length(low))   stop0("High and low have different lenghts");
    if (length(high) != length(close)) stop0("High and close have different lenghts");

    # Number of observations
    Nobs <- length(high);

    # The maximum of these should be p3, check
    if ((p1 > p2) | (p1 > p3) | (p2 > p3)) stop0("Parameters in ichimoku should enter in ascending order")

    # Set the max
    maxp <- p3;

    # You will leave out maxp observations
    cloud <- matrix(0, nrow= Nobs - maxp, ncol=5)
    colnames(cloud) <- c("Tenkan", "Kijun", "SenkouA", "SenkouB", "Chikou")

    # Run a loop to make the computations
    for (i in seq(maxp+1, Nobs, 1)) {
    
        # Compute the cloud
        tenkan  <- (max(high[seq(i-p1, i, 1)]) + min(low[seq(i-p1, i, 1)])) / 2;
        kijun   <- (max(high[seq(i-p2, i, 1)]) + min(low[seq(i-p2, i, 1)])) / 2;
        senkouA <- (tenkan + kijun) / 2;
        senkouB <- (max(high[seq(i-p3, i ,1)]) + min(low[seq(i-p3, i, 1)])) / 2;
        chikou  <- close[i];

        # Save in appropriate places
        cloud[(i-maxp), ] <- c(tenkan, kijun, senkouA, senkouB, chikou);
    }

    # OK, now align them correctly: SenkouA and SenkouB are moved p2 periods forward
    # while Chikou is moved p2 periods backward…
    A1 <- rbind(cloud[,1:2], matrix(NA, p2, 2))
    A2 <- rbind(matrix(NA, p2, 2), cloud[,3:4])
    A3 <- c(cloud[(p2+1):(Nobs-maxp),5], matrix(NA,2*p2,1))
    
    new.cloud <- cbind(A1,A2,A3)
    colnames(new.cloud) <- colnames(cloud)

    # Align the data as well
    new.data <- rbind(data[(maxp+1):Nobs,],matrix(NA,p2,3))
    colnames(new.data) <- colnames(data)

    # OK, return everything
    return(list(data=new.data, cloud = new.cloud))
}



# Imports a CSV file of category weights and returns a hash
# table containing a mapping from category to weight.
#
# Parameters:
# - filename     -> csv file
#
# Return:
#  - categoryGroups -> Hash mapping from category group to weight. 
#  - error:         -> if 1 then there was an error
#  - msg:           -> if error=1 then this holds the error message
.importCategories <- function(filename="") {

    suppressMessages(library("hash"));
    
    if (!file.exists(filename)) {
        stop0("File provided does not exist.");
    }

    data = read.csv(filename, header=FALSE, sep=",");

    categoryGroups = hash();
    for (i in 1 : nrow(data)) {
        #printf("%s - %d\n", data[i,1], data[i,2]);
        categoryGroups[data[i,1]] = data[i,2];
    }
    
    return(categoryGroups);
}



#
# Compute positions of the value strategy
#
# Paramters
#  prices           -> Vector of prices
#  firstTrade       -> When the first trade should take place
#  transactionCosts -> represented as a percentage of the total trade value
#  debug            -> 1 to Print debug information
#
# Return:
#  positions   -> Vector of positions. Its length will be the same as the number of rows in the lagReturns matrix 
#                 plus number of lag returns. The first positions up to in sample will be filled with zero.
#
.computeBestPortfolioSingleAsset <- function(prices, firstTrade = 1, debug=0, transactionCosts = 0) {

    # Maybe do error checking later, maybe not
    if (debug) printf("\nComputing positions for optimal portfolio\n");
    TIME = proc.time();
    
    positions    = numeric(length(prices));
    
    for (i in firstTrade : length(prices)-1) {
        if (prices[i+1]*(1-transactionCosts) > prices[i]/(1-transactionCosts)) {
            positions[i] = 1;
        } else if (prices[i+1]/(1-transactionCosts) < prices[i]*(1-transactionCosts)) {
            positions[i] = -1;
        } 
    }
    
    if (debug) printf("Time computing optimal positions:    %.2fs\n", (proc.time() - TIME)[3]);
    return(positions);
}



# Sigma[t]^2 = w + a* Sigma[t-1]^2 + b*r[t-1]^2
.garch.predict.one.day <- function(fit, r.last) {
    h.last = tail( fitted(fit)[,1] ,1)          
    sqrt(sum( coef(fit) * c(1,  r.last^2, h.last^2) ))  
}
 
# same as predict( fit, n.ahead=1, doplot=F)[3]
.garchFit.predict.one.day <- function(fit, r.last) {
    h.last = tail(sqrt(fit@h.t), 1)
    sqrt(sum( fit@fit$matcoef[,1] * c(1,  r.last^2, h.last^2) ))
}






# FUNCTION <--- calculateImpactIntradayThomsonReuters
#
# Calculate the news impact for Thomson Reuters data.
#
.calculateImpactIntradayThomsonReuters <- function(newsData=NULL, dates=numeric(), hours=numeric(), thresholdImpact=0.01, 
                                                   utcDifference = numeric(), minutesDecay=90, debug=0) {
    
    TIME = proc.time(); 
    if (debug >= 0) printf("\nCalculating impact...\n");

    .errorCheckUTC(utcDifference);
    .errorCheckNewsDataThomsonReuters(newsData);
    .errorCheckNumericVector("Dates", dates);
    .errorCheckNumericVector("Hours", hours);
   
    if (length(dates) != length(hours))         stop0("Length of dates and hours must be the same.");
    if (length(dates) != length(utcDifference)) stop0("Length of dates and utcDifference must be the same.");

    numDates = length(dates);
    
    numNews = nrow(newsData);
    

    impact    = numeric(numDates);
    impactPos = numeric(numDates);
    impactNeg = numeric(numDates);
    newsFlow  = numeric(numDates);

    lambda = calculateLambda(minutesDecay);

    currentNews = 1;
    
    if (sum(utcDifference) != 0) {
        for (i in 1 : length(hours)) {
            updatedHour = updateHourTimezone(hours[i], utcDifference[i]);
            if (updatedHour$daysAdjusted != 0) dates[i] = addDate(days[i], daysAdjusted);
            hours[i] = updatedHour$hour;
        }
    } 

    minutesWeGoBack = thresholdForImpactCalculation(lambda=lambda, threshold = thresholdImpact);
    timeFor = proc.time();
    
    for (i in 1 : numDates) {
        currentDate = dates[i];
        
        initialDateHour = subtractMinutes(date1=currentDate, hour1=hours[i], minutes = minutesWeGoBack);
        initialDate = initialDateHour$date;
        initialHour = initialDateHour$hour;
        
        initialDateNext = 0;
        initialHourNext = 0;
        if (i < numDates) {
            initialDateHourNext = subtractMinutes(date1=dates[i+1], hour1=hours[i+1], minutes = minutesWeGoBack);
            initialDateNext = initialDateHourNext$date;
            initialHourNext = initialDateHourNext$hour;
        }

        if (debug > 0) printf("---- DATE %d ---------------\nImpact to include news from %d, %06d to %d, %06d\n", currentDate, initialDate, initialHour, currentDate, hours[i]);
        
        tempNewsIndex = currentNews;
        foundFirst = FALSE;
        for (j in currentNews : numNews) {
            dateNews = newsData$date[j];
            hourNews = newsData$hour[j];
            if (debug > 0) printf("NEWS %03d cp %7s, s %.2f, %d %06d -> ", j, newsData$company[j], newsData$score[j], dateNews, hourNews);
            if (dateNews > initialDate || (dateNews == initialDate && hourNews > initialHour)) {
                if (!foundFirst && (dateNews > initialDateNext || (dateNews == initialDateNext && hourNews > initialHourNext))) {
                    tempNewsIndex = j;
                    foundFirst = TRUE;
                }
                if (dateNews < currentDate || (dateNews == currentDate && hourNews < hours[i])) {
 
                    score = (newsData$score[j] - 50);
                    imp = score * exp(-1 * lambda * (differenceMinutes(dateNews, hourNews, currentDate, hours[i])/90));
                    
                    impact[i] = impact[i] + imp;
                    newsFlow[i] = newsFlow[i] + 1;
                    if (score > 0) {
                        impactPos[i] = impactPos[i] + imp;
                    } else {
                        impactNeg[i] = impactNeg[i] + imp;
                    }
                    if (debug > 0) printf("add %.5f to date impact (total so far %.5f)\n", imp, impact[i]);

                } else {
                    if (debug > 0) printf("past current date, do not include\n");
                    if (!foundFirst) {
                        tempNewsIndex = j+1;
                    }
                    break;
                }
            } else {
                if (debug > 0) printf("skip this news\n");
            }
        }
        currentNews = tempNewsIndex;
    }
    #timeFor = (proc.time() - timeFor)[3];
    
    TIME = (proc.time() - TIME)[3];
    if (debug >= 0) printf("Function finished in          %.2fs\n", TIME);
    
    return(list("impact"=impact, "impactPos"=impactPos, "impactNeg"=impactNeg, "newsFlow"=newsFlow));

}


# Lot will be defined for last Thursday of the month that comes closest
# to the end of the time series
.getLotSizeIndia <- function(prices=numeric(), dates=numeric(), idealValueForALot = 300000, lot=-1) {
   
    if (lot != -1) return(lot);

    if (length(prices) != length(dates)) stop0("Error in getLotSizeIndia, prices and dates should be have the same length.");
    lastThursday = findLastThursdayOfPreviousMonth(dates[length(dates)]);

    dateIndex = -1;
    for (i in seq(from=length(dates), to=1, by=-1)) {
        ## It is <= because if the thursday is a holiday,
        ## use the last day before that thursday
        if (dates[i] <= lastThursday) {
            dateIndex = i;
            break;
        }
    }

    #printf("Date = %d, lastThursday = %d, lastDate = %d\n", dates[dateIndex], lastThursday, dates[length(dates)]);
    
    if (dateIndex > length(dates) || dateIndex == -1) stop0("Last thursday could not be found in getLotSizeIndia.");

    if (prices[dateIndex] <= 0) return(100);
    
    optimalLot = idealValueForALot / prices[dateIndex];
    
    multiple = 25;
    if (optimalLot >= 400)  multiple = 50;
    if (optimalLot >= 1000) multiple = 100;
    if (optimalLot >= 1500) multiple = 500;
    if (optimalLot >= 2000) multiple = 1000;

    lotLess = optimalLot - optimalLot %% multiple;
    lotMore = lotLess + multiple;

    if (abs(optimalLot - lotLess) <= abs(optimalLot - lotMore)) {
        optimalLot = lotLess;
    } else {
        optimalLot = lotMore;
    }

    if (optimalLot < 25) optimalLot = 25;

    return(optimalLot);
}

# Lot will be defined for last Thursday of the month that comes closest
# to the end of the time series
.calculateLotSizes <- function(prices=matrix(), dates=numeric(), firstTrade = 1, firstCompany = 1, lots = matrix(), idealValueForALot = 300000) {
   
    if (nrow(prices) != length(dates)) stop0("Error in calculateLotSizes, prices and dates should be have the same length.");
    if (length(dates) <= firstTrade)   stop0("Error: dates not right in calculateLotSizes");
    if (nrow(lots) != nrow(prices))    stop0("Error in calculateLotSizes, prices and lots different.");
    if (ncol(lots) != ncol(prices))    stop0("Error in calculateLotSizes, prices and lots different.");


    thursdays = numeric();
    first = floor(dates[firstTrade] / 100);
    last  = floor(dates[length(dates)] / 100);

    dt = first;
    while (dt <= last) {
        thursdays = c(thursdays, findLastThursdayOfPreviousMonth(dt*100+1));
        dt = dt + 1;
        if (dt %% 100 == 13) dt = dt + 100 - 12;
    }

    indices = rep(-1, length(thursdays));
    cur = 1;
    for (i in 1 : length(dates)) {
        if (cur <= length(thursdays)) { 
            if (dates[i] == thursdays[cur]) {
                indices[cur] = i;
                cur = cur + 1;
            }
            if (i > 1 && dates[i] > thursdays[cur] && dates[i-1] < thursdays[cur]) {
                indices[cur] = i-1;
                cur = cur + 1;
            }
        }
    }

    #for (i in 1 : length(indices)) {
    #    printf("Date %d = %d (%d)\n", thursdays[i], indices[i], dates[indices[i]]);
    #}

    if (any(indices == -1)) stop0("Error in calculateLotSizes: indices not found properly");
   
    for (j in firstCompany : ncol(prices)) {
        if (any(lots[firstTrade:nrow(lots),j] == -1)) {
            cur = 1;
            optimalLot = 100;
            for (i in firstTrade : length(dates)) {
                if (i == firstTrade || (cur < length(indices) && dates[i] == dates[indices[cur+1]])) {
                    if (prices[i,j] > 0) {
                        optimalLot = idealValueForALot / prices[i,j];
                        multiple = 25;
                        if (optimalLot >= 400)  multiple = 50;
                        if (optimalLot >= 1000) multiple = 100;
                        if (optimalLot >= 1500) multiple = 500;
                        if (optimalLot >= 2000) multiple = 1000;
                
                        lotLess = optimalLot - optimalLot %% multiple;
                        lotMore = lotLess + multiple;

                        if (abs(optimalLot - lotLess) <= abs(optimalLot - lotMore)) {
                            optimalLot = lotLess;
                        } else {
                            optimalLot = lotMore;
                        }
        
                        if (optimalLot < 25) optimalLot = 25;
                    }
                }
                if (cur < length(indices) && dates[i] == dates[indices[cur+1]]) cur = cur + 1;
                
                #printf("DATE %d, cur = %d, ddd = %d, lot = %d\n", dates[i], cur, dates[indices[cur]], optimalLot);
                if (lots[i,j] == -1) lots[i,j] = optimalLot;
            }
        }
    }
    
    #for (j in firstCompany : ncol(lots)) {
    #    printf("CP %d:\n", j);
    #    for (i in firstTrade : nrow(lots)) {
    #        printf("%d ", lots[i,j]);
    #        if (i == nrow(lots) || i %% 10 == 0) printf("\n");
    #    }
    #    if (j >= 12) break;
    #}

    return(lots);
}

