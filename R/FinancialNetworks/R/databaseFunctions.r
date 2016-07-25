# FUNCTION ----> .selectIndexPrices
#
# Given an index, this function select its prices for the date range specified.
#
# Parameters
# db                   -> open db handle that contains market data
# index                -> Index for which strategy will be computed such as FTSE or SP500 
# initialDate          -> Initial date for the tests, YYYYMMDD, if 0 then get the oldest date in the database.
# finalDate            -> Final date for the tests, YYYYMMDD, if 0 then get the most recent date in the database.
# inSample             -> Number of days for in-sample, default = 0. This is used to check, in case the dates 
#                         interval is less than the in-sample period.
# includePreviousDates -> If true than dates before the initial date will be select, up to inSample dates
#
# Return:
#  - initialDate: Same as initialDate parameter, but non zero if the original parameter was 0
#  - finalDate:   Same as finalDate parameter, but non zero if the original parameter was 0
#  - dates:       vector of dates
#  - prices:      vector of prices
#  - error:       if 1 then there was an error
#  - msg:         if error=1 then this holds the error message

.selectIndexPrices <- function(db, index="", initialDate=0, finalDate=0, inSample=0, includePreviousDates=0) {
  
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);
    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);

    .checkIndex(db, index);

    stat = paste0("select date, close as price, high, low, open, futureClose as futures, futureHigh as futuresHigh, futureLow as futuresLow, futureOpen as futuresOpen from marketPricesIndex where indexId=\"", index, "\" and date >= ", as.character(initialDate), " and date <= ", finalDate, " order by date");
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) {
        stop0("Either the index argument is empty or no data was found for the index in selectIndexPrices"); 
    }
    dates       = res$date;
    prices      = res$price;
    futures     = res$futures;
    low         = res$low;
    high        = res$high;
    open        = res$open;
    futuresLow  = res$futuresLow;
    futuresHigh = res$futuresHigh;
    futuresOpen = res$futuresOpen;


    if (includePreviousDates == 1) {
        stat = paste0("select date, close as price, high, low, open, futureClose as futures, futureHigh as futuresHigh, futureLow as futuresLow, futureOpen as futuresOpen from marketPricesIndex where indexId=\"", index, "\" and date < ", as.character(initialDate), " order by date DESC limit ", inSample);
        res = .query(dbHandle=db, statement=stat);
        dates       = c(rev(res$date),        dates      );
        prices      = c(rev(res$price),       prices     );
        futures     = c(rev(res$futures),     futures    );
        low         = c(rev(res$low),         low        );
        high        = c(rev(res$high),        high       );
        open        = c(rev(res$open),        open       );
        futuresLow  = c(rev(res$futuresLow),  futuresLow );
        futuresHigh = c(rev(res$futuresHigh), futuresHigh);
        futuresOpen = c(rev(res$futuresOpen), futuresOpen);
    }
    
    initialDate = dates[1];
    finalDate = dates[length(dates)];
    
    for (i in 1 : length(dates)) {
        #printf("INDEX %d -- %d: %d - %.2f - %.2f\n", i, length(dates), dates[i], prices[i], futures[i]);
        if (is.na(prices[i])) {
            if (i > 1) { 
                prices[i] = prices[i-1];
            } else {
                prices[i] = prices[i+1];
            }
        }
        if (is.na(low[i]))  low[i]  = prices[i];
        if (is.na(high[i])) high[i] = prices[i];
        if (is.na(open[i])) open[i] = prices[i];
    }
    if (is.na(futures    [length(futures)]))     futures    [length(futures)]     = prices[length(prices)];
    if (is.na(futuresLow [length(futuresLow)]))  futuresLow [length(futuresLow)]  = low   [length(low)];
    if (is.na(futuresHigh[length(futuresHigh)])) futuresHigh[length(futuresHigh)] = high  [length(high)];
    if (is.na(futuresOpen[length(futuresOpen)])) futuresOpen[length(futuresOpen)] = open  [length(open)];
    
    for (i in seq(length(futures)-1, 1, -1)) {
        if (is.na(futures    [i])) futures[i]     = (futures[i+1] * prices[i]) / prices[i+1]; 
        if (is.na(futuresLow [i])) futuresLow[i]  = futures[i];
        if (is.na(futuresHigh[i])) futuresHigh[i] = futures[i];
        if (is.na(futuresOpen[i])) futuresOpen[i] = futures[i]; 
        #if (is.na(futuresLow[i]))  futuresLow[i]  = (futuresLow[i+1]  * low[i]   ) / low[i+1]; 
        #if (is.na(futuresHigh[i])) futuresHigh[i] = (futuresHigh[i+1] * high[i]  ) / high[i+1]; 
        #if (is.na(futuresOpen[i])) futuresOpen[i] = (futuresOpen[i+1] * open[i]  ) / open[i+1]; 
    }
    
    rm(res);
    
    #for (i in 1 : length(dates)) {
    #    printf("%d %8.2f %8.2f\n", dates[i], prices[i], futures[i]);
    #}
    


    return(list("initialDate"=initialDate, "finalDate"=finalDate, "dates"=dates, "prices"=prices, "futures"=futures, "high"=futuresHigh, "low"=futuresLow, "open"=futuresOpen, "error"=0, "msg"=""));

}

# FUNCTION ----> .selectInSampleIndexPrices
#
# Given an index and a date, this function select inSample prices prior to the chosen date.
#
# Parameters
# db                   -> open db handle that contains market data
# index                -> Index for which strategy will be computed such as FTSE or SP500 
# date                 -> Final date for the tests, YYYYMMDD.
# inSample             -> Number of days for in-sample
# extraDays            -> inSample + extraDays dates will be selected from the db
#
# Return:
#  - dates:       vector of dates
#  - prices:      vector of prices
#  - futures:     vector of future prices
#  - error:       if 1 then there was an error
#  - msg:         if error=1 then this holds the error message
.selectInSampleIndexPrices <- function(db, index="", date=0, inSample=10, extraDays=0) {
   
    stat = paste0("select date, close as price, futureClose as futures from marketPricesIndex where indexId=\"", index, "\" and date <= ", as.character(date), " order by date desc limit ", inSample + extraDays);
    
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) {
        stop0("Either the index argument is empty or no data was found for the index"); 
    }
    if (nrow(res) != (inSample + extraDays)) {
        stop0(paste0("There are not enough dates in the database: ", nrow(res), " were found (desired was ", inSample + extraDays, ")")); 
    }
    dates   = rev(res$date);
    prices  = rev(res$price);
    futures = rev(res$futures);

    
    initialDate = dates[1];
    finalDate = dates[length(dates)];
    
    return(list("initialDate"=initialDate, "finalDate"=finalDate, "dates"=dates, "prices"=prices, "futures"=futures));
}

selectLastIndexDate <- function(db, index) {
    res = .query(dbHandle=db, statement=paste0("Select MAX(date) as date from marketPricesIndex where indexId=\"", index, "\" and close IS NOT NULL"));
    if (nrow(res) == 0) stop0(paste0("No data for ", index));
    return(as.numeric(res$date[1])); 
}

.selectTickerAndName <- function(db, companies) {
    if (!is.character(companies)) stop0("In selectTickerAndName, companies must be a character vector");
    names   = character(length(companies));
    tickers = character(length(companies));
    
    for (i in 1 : length(companies)) {
        if (companies[i] == "RFR") {
            names[i] = "Cash asset";
            tickers[i] = "CASH";
            next;
        }
        if (companies[i] == "FUTURES") {
            names[i] = "Index futures";
            tickers[i] = "FUTURES";
            next;
        }
        res = .query(dbHandle=db, statement=paste0("Select name, listingTicker from company where ravenpackId=\"", companies[i], "\""));
        if (nrow(res) == 0) stop0(paste0("Company ", companies[i] , " not found in the database"));
        names  [i] = res$name[1];
        tickers[i] = res$listingTicker[1];
    }
    return(list("names"=names, "tickers"=tickers));
}




# FUNCTION ----> .selectCompaniesInIndeAndPrices
#
# Select companies that were part of the index during a date, also the prices for each company.
#
# Parameters
# db                  -> open db handle that contains market data
# index               -> Index for which strategy will be computed such as FTSE or SP500 
# date1               -> Initial date, YYYYMMDD.
# date2               -> Final date, YYYYMMDD, in this date company must have been in the index to be selected.
# numDates            -> For consistency checking
#
# limitToTopCompanies -> - If FALSE, all available companies from the index will be selected.
#                        - If TRUE, then a limited number of companies will be selected. The companies to be selected
#                        are decided based on the highest Market Value.                 
#
# limitByPercentageMV -> Only valid if limitToTopCompanies is TRUE
#                        - If FALSE, then we choose the valueTopCompanies with the highest market value.
#                        - If TRUE, then we choose all companies such that we cover valueTopCompanies percent 
#                        of the total market value.
#
# valueTopCompanies   -> Only valid if limitToTopCompanies is TRUE
#                        - If limitByPercentageMV is FALSE, than this should be the number of companies with the highest 
#                        market value
#                        - If limitByPercentageMV is TRUE, than this should be the desired percentage 
#                        of total market value - the percentage should be between 0 and 100
#
# Return:
#  - companies:        list of companies names
#  - companiesIds:     list of companies ravenpack ids
#  - companiesTickers: list of companies tickers
#  - prices:           price matrix (companies x dates)
#  - error:            if 1 then there was an error
#  - msg:              if error=1 then this holds the error message
.selectCompaniesInIndexAndPrices <- function(db, index="", date1=0, date2=0, numDates=1,
                                            limitToTopCompanies=FALSE, limitByPercentageMV=FALSE, valueTopCompanies=100) {

    
    TIME = proc.time();

    ############################################
    #### SELECT COMPANIES
    
    stat = "select c.ravenpackId as id, c.name as name, c.listingTicker as ticker ";
    stat = paste0(stat, "from company as c, companyIndex as i ");
    stat = paste0(stat, "where i.indexId=\"", index, "\" ");
    stat = paste0(stat, "and i.companyId = c.ravenpackId ");
    stat = paste0(stat, "and i.startDate <= ", as.character(date2), " ");
    stat = paste0(stat, "and i.endDate > ",  as.character(date2), " ");
    stat = paste0(stat, "order by c.name ");
    
    res = .query(dbHandle=db, statement=stat);

    numCompanies = nrow(res);
    if (numCompanies == 0) {
        stop0("No companies were found.");
    }

    companies        = res$name;
    companiesIds     = res$id;
    companiesTickers = res$ticker;

    prices = matrix(, nrow=numDates, ncol=0);
    
    toRemove = numeric(); 

    ## Load prices
    for (i in 1 : numCompanies) {
        stat = paste0("select close as prices from datesAdjustedPrices where companyId=\"", companiesIds[i], "\" and indexId=\"", index, "\" and date >= ", date1,  " and date <= ", date2, " order by date");
        res = .query(dbHandle=db, statement=stat);
        
        
        temp = res$prices;
        
        if (length(temp) != numDates) {
            toRemove = c(toRemove, i);
            next;
        }
        if (length(temp[temp > 0]) != length(temp)) {
            toRemove = c(toRemove, i);
            next;
        }
         
        prices = cbind(prices, temp);

    }
    # If row row.names(prices) = NULL
    colnames(prices) = NULL;
    
    if (length(toRemove) > 0) {
        for (i in seq(from=length(toRemove), to=1, by=-1)) {
            companiesIds     <- companiesIds[-toRemove[i]];
            companies        <- companies[-toRemove[i]];
            companiesTickers <- companiesTickers[-toRemove[i]];
        }
    }
    return(list("companies"=companies, "companiesIds"=companiesIds, "companiesTickers"=companiesTickers, "prices"=prices));
}








# FUNCTION ----> .selectCompaniesAndPrices

# Given an index and a time interval, this function select the companies that were part of the index 
# for at least part of the specified dates. The prices for the companies during the period are also 
# returned. If a specific price for a specific date is equal to -1 there are two possibilites:
#    - The company was not part of the index during that date
#    - Our database does not have that price for some other reason
#
#
# Parameters
# db                  -> open db handle that contains market data
# index               -> Index for which strategy will be computed such as FTSE or SP500 
# initialDate         -> Initial date for the tests, YYYYMMDD, if 0 then get the oldest date in the database.
# finalDate           -> Final date for the tests, YYYYMMDD, if 0 then get the most recent date in the database.
# inSample            -> Number of days for in-sample, default = 0. This is used to check, in case the dates 
#                        interval is less than the in-sample period.
#
# limitToTopCompanies -> - If FALSE, all available companies from the index will be selected.
#                        - If TRUE, then a limited number of companies will be selected. The companies to be selected
#                        are decided based on the highest Market Value.                 
#
# limitByPercentageMV -> Only valid if limitToTopCompanies is TRUE
#                        - If FALSE, then we choose the valueTopCompanies with the highest market value.
#                        - If TRUE, then we choose all companies such that we cover valueTopCompanies percent 
#                        of the total market value.
#
# valueTopCompanies   -> Only valid if limitToTopCompanies is TRUE
#                        - If limitByPercentageMV is FALSE, than this should be the number of companies with the highest 
#                        market value
#                        - If limitByPercentageMV is TRUE, than this should be the desired percentage 
#                        of total market value - the percentage should be between 0 and 100
#
# Return:
#  - companies:      list of companies, by name
#  - companiesIds:   list of companies, by code
#  - prices:         matrix of prices, the prices on column "i" match the company of index "i" in the companies vector
#  - dates:          list of dates, with the same length as the list of prices for each company
#  - error:          if 1 then there was an error
#  - msg:            if error=1 then this holds the error message
.selectCompaniesAndPrices <- function(db, index="", initialDate=0, finalDate=0, inSample=0, 
                                     limitToTopCompanies=FALSE, limitByPercentageMV=FALSE, valueTopCompanies=100) {

    
    TIME = proc.time();
    printf("\nLoading market data...\n");

    suppressMessages(library("hash"));
    .errorCheckTopCompanies(limitToTopCompanies, limitByPercentageMV, valueTopCompanies);
    
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);
    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);

    ############################################
    #### SELECT COMPANIES
    TIMES1 = proc.time();
    stat = .createQuerySelectCompanies(index, initialDate, finalDate, limitToTopCompanies, limitByPercentageMV, valueTopCompanies);
    #printf("%s\n", stat);
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) {
        stop0("Either the index argument is empty or no companies were found for the parameters chosen."); 
    }
    
    existCompany = hash();
    existCompanyDate = hash();
    companies      = character();
    companiesCodes = character();
    ids = character();

    for (i in 1 : nrow(res)) {
        if (limitToTopCompanies) existCompanyDate[paste0(res$id[i], res$date1[i], sep="-")] = 1; 
        if (has.key(res$id[i], existCompany)) {
            next;
        }
        existCompany[res$id[i]] = 1;
        companies      = append(companies,      res$name[i]);
        ids = append(ids, res$id[i]);
    }
    #for (i in 1 : length(companies)) {
    #    printf("Company %d - %s - %s\n", i, ids[i], companies[i]);
    #}
    
    rm(res);
    TIMES1 = (proc.time() - TIMES1)[3];
    #### END SELECT COMPANIES
    ############################################
    
    
    ############################################
    #### SELECT PRICES
    TIMES2 = proc.time();
    stat = paste0("select date from datesAdjustedPrices where companyId=\"", ids[1], "\" and date >= ", initialDate,  " and date <= ", finalDate, " order by date");
    res = .query(dbHandle=db, statement=stat);
    dates = res$date;
    numDates = length(dates);
    rm(res);
   
    numCompanies = length(companies);
    #printf("NUM DATES: %d\n", numDates);
    prices = matrix(-1, nrow=numDates, ncol=numCompanies);

    for (i in 1 : numCompanies) {
    
        #TIME = proc.time();
        #printf("%03d/%03d) Loading prices for %s in... ", i, numCompanies, companies[i]);
        
        stat = paste0("select close as prices from datesAdjustedPrices where companyId=\"", ids[i], "\" and indexId=\"", index, "\" and date >= ", initialDate,  " and date <= ", finalDate, " order by date");
        #printf("%s\n", stat);
        res = .query(dbHandle=db, statement=stat);
        
        if (nrow(res) != numDates) {
            stop0(paste0("Company ", companies[i], " has ", nrow(res), " prices, should have ", numDates));
        }
        prices[,i] = res$prices;
        
        #printf(" %.2fs\n", (proc.time() - TIME)[3]);
    }
    TIMES2 = (proc.time() - TIMES2)[3];
    #### END SELECT PRICES
    ############################################



    
    ##################################################
    #### REMOVE PRICES FROM COMPANIES THAT ARE NOT TOP
    if (limitToTopCompanies) {
        TIMES3 = proc.time();

        #stat = paste0("select date from marketPricesIndex where indexId=\"",index, "\" and date >= ", initialDate, " and date <= ", finalDate, " order by date");
        #res = .query(dbHandle=db, statement=stat);
        #dates = res$date;
        #rm(res);
        #if (length(dates) != numDates) {
        #    stop0(paste0("Index has ", length(dates), " dates, should have ", numDates));
        #}

        stat = paste0("select distinct startDate as date1, endDate as date2 from marketWeight where indexId = \"", index, "\" and startDate < ", finalDate,  " and endDate > ", initialDate, " order by startDate");
        capDates = .query(dbHandle=db, statement=stat);
        numCapDates = nrow(capDates);    
        
        indexCapDates1 = numeric(numCapDates);
        indexCapDates2 = numeric(numCapDates);
        currentCapDate = 1;
        for (i in 1 : numDates) {
            if (indexCapDates1[currentCapDate] == 0 && dates[i] >= capDates$date1[currentCapDate]) {
                indexCapDates1[currentCapDate] = i;
            }
            if (indexCapDates2[currentCapDate] == 0 && (i == numDates || dates[i] >= capDates$date2[currentCapDate])) {
                if (i == numDates) {
                    indexCapDates2[currentCapDate] = i;
                } else {
                    indexCapDates2[currentCapDate] = i-1;
                }
                currentCapDate = currentCapDate + 1;
                if (currentCapDate <= numCapDates) {
                    indexCapDates1[currentCapDate] = i;
                }
            }
        }

        for (i in 1 : numCapDates) {
            #printf("CAP DATES: %d (ind %d = %d) --> %d (ind %d = %d)\n", capDates$date1[i], indexCapDates1[i], dates[indexCapDates1[i]], capDates$date2[i], indexCapDates2[i], dates[indexCapDates2[i]]);
            for (j in 1 : numCompanies) {
                if (!has.key(paste0(ids[j], capDates$date1[i], sep="-"), existCompanyDate)) {
                    prices[indexCapDates1[i]:indexCapDates2[i], j] = -1;
                    #printf("No company %s -> %d\n", companies[j], j);
                }
            }
        }
        rm(capDates);
        TIMES3 = (proc.time() - TIMES3)[3];
    }
    #### REMOVE PRICES FROM COMPANIES THAT ARE NOT TOP
    ##################################################

    rm(existCompanyDate);
    rm(existCompany);


    printf("Function finished in          %.2fs\n", (proc.time() - TIME)[3]);
    printf("Time loading companies:       %.2fs\n", TIMES1);
    printf("Time loading prices:          %.2fs\n", TIMES2);
    if (limitToTopCompanies) printf("Time computing top companies: %.2fs\n", TIMES3);

    return(list("dates"=dates, "companies"=companies, "companiesIds"=ids, "prices"=prices));
}


# FUNCTION ----> .selectPricesGivenCompanies
#
# Given a list of companies, select date adjusted prices
#
# Parameters
# db                  -> open db handle that contains market data
# index               -> Index for which strategy will be computed such as FTSE or SP500 
# companies           -> list of companies ids.
# initialDate         -> Initial date for the tests, YYYYMMDD, if 0 then get the oldest date in the database.
# finalDate           -> Final date for the tests, YYYYMMDD, if 0 then get the most recent date in the database.
#
# Return:
#  - prices:         matrix of prices, the prices on column "i" match the company of index "i" in the companies vector
#  - error:          if 1 then there was an error
#  - msg:            if error=1 then this holds the error message
.selectPricesGivenCompanies <- function(db, companies=numeric(), index="", initialDate=0, finalDate=0) {

    
    TIME = proc.time();
    printf("\nLoading market data...\n");

    suppressMessages(library("hash"));
    
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);
    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);

    .errorCheckCharacterVector("Companies", companies);

    ############################################
    #### SELECT PRICES
    stat = paste0("select date from datesAdjustedPrices where companyId=\"", companies[1], "\" and indexId=\"", index, "\" and date >= ", initialDate,  " and date <= ", finalDate, " order by date");
    res = .query(dbHandle=db, statement=stat);
    dates = res$date;
    numDates = length(dates);
    rm(res);
   
    numCompanies = length(companies);
    #printf("NUM DATES: %d\n", numDates);
    prices = matrix(-1, nrow=numDates, ncol=numCompanies);
    low    = matrix(-1, nrow=numDates, ncol=numCompanies);
    high   = matrix(-1, nrow=numDates, ncol=numCompanies);
    open   = matrix(-1, nrow=numDates, ncol=numCompanies);

    for (i in 1 : numCompanies) {
    
        #TIME = proc.time();
        #printf("%03d/%03d) Loading prices for %s in... \n", i, numCompanies, companies[i]);
        
        stat = paste0("select close as prices, high, low, open from datesAdjustedPrices where companyId=\"", companies[i], "\" and indexId=\"", index, "\" and date >= ", initialDate,  " and date <= ", finalDate, " order by date");
        #printf("%s\n", stat);
        res = .query(dbHandle=db, statement=stat);
        
        if (nrow(res) != numDates) {
            stop0(paste0("Company ", companies[i], " has ", nrow(res), " prices, should have ", numDates));
        }
        prices[,i] = res$prices;
        low[,i]    = res$low;
        high[,i]   = res$high;
        open[,i]   = res$open;
        
        #printf(" %.2fs\n", (proc.time() - TIME)[3]);
    }
    #### END SELECT PRICES
    ############################################

    printf("Function finished in          %.2fs\n", (proc.time() - TIME)[3]);

    return(list("dates"=dates, "prices"=prices, "high"=high, "low"=low, "open"=open));

}





#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################




# FUNCTION ----> .selectCompanies
#
# Given an index and a time interval, this function select the companies that were part of the index 
# for at least part of the specified dates. 
#
# Parameters
# db                  -> open db handle that contains market data
# index               -> Index for which strategy will be computed such as FTSE or SP500 
# initialDate         -> Initial date for the tests, YYYYMMDD, if 0 then get the oldest date in the database.
# finalDate           -> Final date for the tests, YYYYMMDD, if 0 then get the most recent date in the database.
# inSample            -> Number of days for in-sample, default = 0. This is used to check, in case the dates 
#                        interval is less than the in-sample period.
#
# limitToTopCompanies -> - If FALSE, all available companies from the index will be selected.
#                        - If TRUE, then a limited number of companies will be selected. The companies to be selected
#                        are decided based on the highest Market Value.                 
#
# limitByPercentageMV -> Only valid if limitToTopCompanies is TRUE
#                        - If FALSE, then we choose the valueTopCompanies with the highest market value.
#                        - If TRUE, then we choose all companies such that we cover valueTopCompanies percent 
#                        of the total market value.
#
# valueTopCompanies   -> Only valid if limitToTopCompanies is TRUE
#                        - If limitByPercentageMV is FALSE, than this should be the number of companies with the highest 
#                        market value
#                        - If limitByPercentageMV is TRUE, than this should be the desired percentage 
#                        of total market value - the percentage should be between 0 and 100
#
# Return:
#  - companies:      list of companies, by name
#  - companiesIds:   list of companies, by code
#  - error:          if 1 then there was an error
#  - msg:            if error=1 then this holds the error message
.selectCompanies <- function(db, index="", initialDate=0, finalDate=0, inSample=0, limitToTopCompanies=FALSE, 
                            limitByPercentageMV=FALSE, valueTopCompanies=100) {

    
    TIME = proc.time();
    printf("\nLoading companies data...\n");

    suppressMessages(library("hash"));
    .errorCheckTopCompanies(limitToTopCompanies, limitByPercentageMV, valueTopCompanies);
    
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);

    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);

    ############################################
    #### SELECT COMPANIES
    stat = .createQuerySelectCompanies(index, initialDate, finalDate, limitToTopCompanies, limitByPercentageMV, valueTopCompanies);
    #printf("%s\n", stat);
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) {
        stop0("Either the index argument is empty or no companies were found for the parameters chosen, a possible reason could be that there is no market cap information."); 
    }
    
    existCompany = hash();
    companies      = character();
    companiesCodes = character();
    ids = character();

    for (i in 1 : nrow(res)) {
        if (has.key(res$id[i], existCompany)) {
            next;
        }
        existCompany[res$id[i]] = 1;
        companies = append(companies, res$name[i]);
        ids = append(ids, res$id[i]);
    }
    #for (i in 1 : length(companies)) {
    #    printf("Company %d - %s - %s\n", i, ids[i], companies[i]);
    #}
    
    rm(res);
    #### END SELECT COMPANIES
    ############################################
    
    rm(existCompany);


    printf("Function finished in          %.2fs\n", (proc.time() - TIME)[3]);

    return(list("companies"=companies, "companiesIds"=ids));

}

# FUNCTION ----> wasCompanyInIndex
#
# Given a company, an index and a date, check if company was in index in that date
#
# Parameters
# db                  -> open db handle that contains market data
# index               -> Index id
# company             -> Company id
# date                -> Date to be checked
#
# Return:
#  - wasInIndex:      1 if company was in the index at that date
#  - error:           if 1 then there was an error
#  - msg:             if error=1 then this holds the error message
.wasInTheIndex <- function(db, index="", company="", date=0) {

    
    if (!is.numeric(date)) stop0(paste0("Date ", date, " is invalid"));

    stat = paste0("SELECT count(*) as c FROM companyIndex WHERE indexId = \"", index, "\" and companyId = \"", company, "\" AND startDate <= ", date, " AND endDate > ", date);
    e = .query(dbHandle=db, statement=stat);
    wasInIndex = e$c[1];

    return(wasInIndex);
}

.selectLotsFutures <- function(db, index="", dates=numeric()) {

    if (!is.numeric(dates)) stop0(paste0("Dates are invalid in .selectLots"));

    firstDate = addDate(dates[1],            -35);
    lastDate  = addDate(dates[length(dates)], 35);
    lots = rep(-1, length(dates));

    stat = paste0("select date, lot from lotsFutures where indexId = \"", index, "\" and date >= ", firstDate, " and Date <= ", lastDate, " order by date");
    lt = .query(dbHandle=db, statement=stat);
    
    if (nrow(lt) == 0) return(lots);

    for (i in 1 : nrow(lt)) {
        lt$date[i] = findLastThursdayOfPreviousMonth(lt$date[i]);
    }

    currentLotIndex = 1;
    lastThursday = -1;
    for (i in 1: length(lots)) {
        if (lastThursday != -1 && dates[i] %% 100 < 20) {
            thursday = lastThursday;
        } else {
            thursday = findLastThursdayOfPreviousMonth(dates[i]);
        }
        lastThursday = thursday;
        while (currentLotIndex < nrow(lt) && lt$date[currentLotIndex] < thursday) {
            currentLotIndex = currentLotIndex + 1;
        }
        if (thursday == lt$date[currentLotIndex]) lots[i] = lt$lot[currentLotIndex];
    }
    return (lots);
}


.selectLots <- function(db, index="", company="", dates=numeric()) {

    if (!is.numeric(dates)) stop0(paste0("Dates are invalid in .selectLots"));

    firstDate = addDate(dates[1],            -35);
    lastDate  = addDate(dates[length(dates)], 35);
    lots = rep(-1, length(dates));

    stat = paste0("select date, lot from lots where indexId = \"", index, "\" and companyId = \"", company, "\" and date >= ", firstDate, " and Date <= ", lastDate, " order by date");
    lt = .query(dbHandle=db, statement=stat);
    
    if (nrow(lt) == 0) return(lots);

    for (i in 1 : nrow(lt)) {
        lt$date[i] = findLastThursdayOfPreviousMonth(lt$date[i]);
    }

    currentLotIndex = 1;
    lastThursday = -1;
    for (i in 1: length(lots)) {
        if (lastThursday != -1 && dates[i] %% 100 < 20) {
            thursday = lastThursday;
        } else {
            thursday = findLastThursdayOfPreviousMonth(dates[i]);
        }
        lastThursday = thursday;
        while (currentLotIndex < nrow(lt) && lt$date[currentLotIndex] < thursday) {
            currentLotIndex = currentLotIndex + 1;
        }
        if (thursday == lt$date[currentLotIndex]) lots[i] = lt$lot[currentLotIndex];
    }
    return (lots);
}



# FUNCTION ----> .selectCompaniesAndTopMatrix
#
# Given an index and a time interval, this function select the companies that were part of the index 
# for at least part of the specified dates. 
#
# Parameters
# db                  -> open db handle that contains market data
# index               -> Index for which strategy will be computed such as FTSE or SP500 
# initialDate         -> Initial date for the tests, YYYYMMDD, if 0 then get the oldest date in the database.
# finalDate           -> Final date for the tests, YYYYMMDD, if 0 then get the most recent date in the database.
# inSample            -> Number of days for in-sample, default = 0. This is used to check, in case the dates 
#                        interval is less than the in-sample period.
#
# limitToTopCompanies -> - If FALSE, all available companies from the index will be selected.
#                        - If TRUE, then a limited number of companies will be selected. The companies to be selected
#                        are decided based on the highest Market Value.                 
#
# limitByPercentageMV -> Only valid if limitToTopCompanies is TRUE
#                        - If FALSE, then we choose the valueTopCompanies with the highest market value.
#                        - If TRUE, then we choose all companies such that we cover valueTopCompanies percent 
#                        of the total market value.
#
# valueTopCompanies   -> Only valid if limitToTopCompanies is TRUE
#                        - If limitByPercentageMV is FALSE, than this should be the number of companies with the highest 
#                        market value
#                        - If limitByPercentageMV is TRUE, than this should be the desired percentage 
#                        of total market value - the percentage should be between 0 and 100
#
# Return:
#  - companies:      list of companies, by name
#  - companiesIds:   list of companies, by code
#  - error:          if 1 then there was an error
#  - msg:            if error=1 then this holds the error message
.selectCompaniesAndTopMatrix <- function(db, index="", initialDate=0, finalDate=0, inSample=0, limitToTopCompanies=FALSE, 
                                        limitByPercentageMV=FALSE, valueTopCompanies=100, dates=numeric()) {

    
    TIME = proc.time();
    printf("\nLoading companies data...\n");

    suppressMessages(library("hash"));
    .errorCheckTopCompanies(limitToTopCompanies, limitByPercentageMV, valueTopCompanies);
    
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);
    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);

    ############################################
    #### SELECT COMPANIES
    finalDate2   = finalDate;
    initialDate2 = initialDate;
    finalDate2   = 29999999;
    initialDate2 = 20000000;
    stat = .createQuerySelectCompanies(index, initialDate2, finalDate2, limitToTopCompanies, limitByPercentageMV, valueTopCompanies);
    #printf("%s\n", stat);
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) {
        stop0("Either the index argument is empty or no companies were found for the parameters chosen."); 
    }
    
    existCompany = hash();
    existCompanyDate = hash();
    companies      = character();
    companiesCodes = character();
    ids = character();

    for (i in 1 : nrow(res)) {
        if (limitToTopCompanies) existCompanyDate[paste0(res$id[i], res$date1[i], sep="-")] = 1; 
        if (has.key(res$id[i], existCompany)) {
            next;
        }
        existCompany[res$id[i]] = 1;
        companies = append(companies, res$name[i]);
        ids = append(ids, res$id[i]);
    }

    #for (i in 1 : length(companies)) {
    #    printf("Company %d - %s - %s\n", i, ids[i], companies[i]);
    #}
    
    rm(res);
    #### END SELECT COMPANIES
    ############################################
    
    numDates = length(dates);
    numCompanies = length(companies);

    ##################################################
    #### REMOVE PRICES FROM COMPANIES THAT ARE NOT TOP
    topCompanies = matrix(1, nrow=numDates, ncol=numCompanies);
    if (limitToTopCompanies) {
        TIMES3 = proc.time();
        
        stat = paste0("select distinct startDate as date1, endDate as date2 from marketWeight where indexId = \"", index, "\" and startDate < ", finalDate,  " and endDate > ", initialDate, " order by startDate");
        capDates = .query(dbHandle=db, statement=stat);
        numCapDates = nrow(capDates);    
        
        indexCapDates1 = numeric(numCapDates);
        indexCapDates2 = numeric(numCapDates);
        currentCapDate = 1;
        for (i in 1 : numDates) {
            if (indexCapDates1[currentCapDate] == 0 && dates[i] >= capDates$date1[currentCapDate]) {
                indexCapDates1[currentCapDate] = i;
            }
            if (indexCapDates2[currentCapDate] == 0 && (i == numDates || dates[i] >= capDates$date2[currentCapDate])) {
                if (i == numDates) {
                    indexCapDates2[currentCapDate] = i;
                } else {
                    indexCapDates2[currentCapDate] = i-1;
                }
                currentCapDate = currentCapDate + 1;
                if (currentCapDate <= numCapDates) {
                    indexCapDates1[currentCapDate] = i;
                }
            }
        }

        for (i in 1 : numCapDates) {
            #printf("CAP DATES: %d (ind %d = %d) --> %d (ind %d = %d)\n", capDates$date1[i], indexCapDates1[i], dates[indexCapDates1[i]], capDates$date2[i], indexCapDates2[i], dates[indexCapDates2[i]]);
            for (j in 1 : numCompanies) {
                if (!has.key(paste0(ids[j], capDates$date1[i], sep="-"), existCompanyDate)) {
                    topCompanies[indexCapDates1[i]:indexCapDates2[i], j] = -1;
                    #printf("No company %s -> %d\n", companies[j], j);
                }
            }
        }
        rm(capDates);
        TIMES3 = (proc.time() - TIMES3)[3];
    }
    #### REMOVE PRICES FROM COMPANIES THAT ARE NOT TOP
    ##################################################

    rm(existCompanyDate);
    rm(existCompany);


    printf("Function finished in          %.2fs\n", (proc.time() - TIME)[3]);

    return(list("companies"=companies, "companiesIds"=ids, "topCompanies"=topCompanies));
}





.companiesAsHash <- function(db) {
    suppressMessages(library("hash"));
    
    stat = "select ravenpackId as id, name, listingTicker as tick from company";
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) stop0("No companies were found."); 
    
    names   = hash();
    tickers = hash();
    for (i in 1 : nrow(res)) {
        names[res$id[i]]   = res$name[i];
        tickers[res$id[i]] = res$tick[i];
    }
    rm(res);
    
    return(list("names"=names, "tickers"=tickers));
}




#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################





#
# Load news from ravenpack
#
# Parameters:
#  companiesIds        -> Vector of companies codes from which news will be selected.
#  index               -> If not empty then exclude news from companies outside the index.
#  initialDate         -> YYYYMMDD, if 0 then oldest date from the database will be selected.
#  finalDate           -> YYYYMMDD, if 9 then the most recent date from the database will be selected.
#  relevance           -> 0-100, mininum relevance filter 
#  novelty             -> 0-100, minimum novelty filter
#  numberExtraDays     -> Number of extra days to select news before the initial date (usually this account
#                         for the initial time from which impact will be calculated).
#  debug               -> If 1 print debug information
#
# 
# Return
# - newsData:    Data frame of news data for all companies, sorted by date and hour, containing:
#                company, date, hour, score, categoryGroup
#  - error:      if 1 then there was an error
#  - msg:        if error=1 then this holds the error message
.loadNewsDataRavenpack <- function(db, companiesIds, index="", initialDate=0, finalDate=0, 
                                  positiveImpactThreshold=51, negativeImpactThreshold=49,
                                  relevance=100, novelty=70, numberExtraDays=0, debug=0) {
    
    TIME = proc.time();
    printf("\nLoading news data from Ravenpack...\n");
    
    timeMerge   = 0;
    timeNews    = 0;
    timeProcess = 0;
    
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);
    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);
    initialDate = as.numeric(addDate(initialDate, -1*numberExtraDays));

    .checkIndex(db, index);

    if (debug) printf("\nLoading news data\nInitial date = %d and final date = %d\n", initialDate, finalDate);
 
    TIME2 = proc.time();
    if (debug) printf("Calculating number and preallocating data...");
    numCompanies =  length(companiesIds);
    filters = character(numCompanies);
    numNews = numeric(numCompanies);
    currentIndex = 1;
    for (i in 1 : numCompanies) {
        filters[i] = "";
        if (index != "") {
            stat = paste0("select startDate as date1, endDate as date2 from companyIndex where indexId=\"", index, "\" and companyId = \"", companiesIds[i], "\" and endDate >= ", initialDate, " and startDate <= ", finalDate, " order by startDate");
            #printf("%s\n", stat);
            dates = .query(dbHandle=db, statement=stat);
            if (nrow(dates) == 0) next;
            filters[i] = " AND (";
            for (j in 1 : nrow(dates)) {
                if (j > 1) { 
                    filters[i] = paste0(filters[i], " OR ");
                }
                filters[i] = paste0(filters[i], "(date >= ", dates$date1[j], " AND date <= ", dates$date2[j], ")")
            }
            filters[i] = paste0(filters[i], ")");
        }

        stat = paste0("SELECT count(*) as num FROM news",  companiesIds[i], " as n LEFT JOIN newsCategory as c ON c.category == n.category WHERE date >= ", initialDate, " and date <= ", finalDate, filters[i], " AND relevance >= ", relevance, " AND (ens >= ", novelty, " OR ens == NULL) ");
        if (positiveImpactThreshold != negativeImpactThreshold) {
            stat = paste0(stat, sprintf("AND (ess >= %d OR (ess == NULL AND css >= %d) OR ess <= %d OR (ess == NULL AND css <= %d))", 
                                         positiveImpactThreshold,  positiveImpactThreshold, negativeImpactThreshold, negativeImpactThreshold));
        }
        stat = paste0(stat, "ORDER BY date ASC, hour ASC");
        news = .query(dbHandle=db, statement=stat);
        numNews[i] = news$num[1];
    }
    sumNews = sum(numNews);
    newsData = data.frame(company=character(sumNews), date=numeric(sumNews), hour=numeric(sumNews), score=numeric(sumNews), css=numeric(sumNews), categoryGroup=character(sumNews), relevance=numeric(sumNews), stringsAsFactors=FALSE)
    timeNews = timeNews + (proc.time() - TIME2)[3]; 
    if (debug) printf(" %.2fs\n", timeNews);


    for (i in 1 : numCompanies) {
        TIME2 = proc.time();
        if (debug) printf("%03d/%03d Company %s: ", i, length(companiesIds), companiesIds[i]); 
        
        if (numNews[i] > 0) {
            stat = paste0("SELECT '", companiesIds[i], "' as company, n.date as date, n.hour as hour, n.ess as score, n.css as css, c.categoryGroup as categoryGroup, n.relevance as relevance FROM news",  companiesIds[i], " as n LEFT JOIN newsCategory as c ON c.category == n.category WHERE date >= ", initialDate, " and date <= ", finalDate, filters[i], " AND relevance >= ", relevance, " AND (ens >= ", novelty, " OR ens == NULL) ");
            if (positiveImpactThreshold != negativeImpactThreshold) {
                stat = paste0(stat, sprintf("AND (ess >= %d OR (ess == NULL AND css >= %d) OR ess <= %d OR (ess == NULL AND css <= %d))", 
                                             positiveImpactThreshold,  positiveImpactThreshold, negativeImpactThreshold, negativeImpactThreshold));
            }

            stat = paste0(stat, "ORDER BY date ASC, hour ASC");

            news = .query(dbHandle=db, statement=stat);
            timeNews = timeNews + (proc.time() - TIME2)[3]; 
            
            timeMerge1 = proc.time();
            #newsData = merge(newsData, news, by=c("company","date", "hour", "score", "css", "categoryGroup", "relevance"), all=TRUE)
            #newsData = rbind(newsData, news);
            newsData[currentIndex:(currentIndex+numNews[i]-1),] = news;
            currentIndex = currentIndex + numNews[i];
            timeMerge = timeMerge + (proc.time() - timeMerge1)[3];
        }
        
        if (debug) printf(" %d news, in %.2fs\n", numNews[i], (proc.time() - TIME2)[3]);
    }
       
    timeProcess = proc.time();
    if (relevance < 100) {
        newsData$score[newsData$relevance < 100] <- newsData$css[newsData$relevance < 100];
    }
    newsData = newsData[,!(names(newsData) %in% c("css", "relevance"))]
    timeProcess = (proc.time() - timeProcess)[3];
    
    timeOrder = proc.time();
    newsData = newsData[with(newsData, order(date, hour)), ];
    timeOrder = (proc.time() - timeOrder)[3];
    
    
    #print(newsData);
    
    TIME = (proc.time() - TIME)[3];
    printf("Number of news found: %d\n", nrow(newsData));
    printf("News data loaded in   %.2fs\n", TIME);
    printf("Time in database      %.2fs\n", timeNews);
    printf("Time in merging       %.2fs\n", timeMerge);
    printf("Time processing news: %.2fs\n", timeProcess);
    printf("Time sorting news:    %.2fs\n", timeOrder);

    return(newsData);
}



#
# Load news from thomsonReuters
#
# Parameters:
#  companiesIds        -> Vector of companies codes from which news will be selected.
#  index               -> If not empty then exclude news from companies outside the index.
#  initialDate         -> YYYYMMDD, if 0 then oldest date from the database will be selected.
#  finalDate           -> YYYYMMDD, if 9 then the most recent date from the database will be selected.
#  relevance           -> 0-100, mininum relevance filter 
#  novelty             -> maximum number of related news in the past 3 days
#  numberExtraDays     -> Number of extra days to select news before the initial date (usually this account
#                         for the initial time from which impact will be calculated).
#  debug               -> If 1 print debug information
#
# 
# Return
# - newsData:    Data frame of news data for all companies, sorted by date and hour, containing:
#                company, date, hour, score, categoryGroup
#  - error:      if 1 then there was an error
#  - msg:        if error=1 then this holds the error message
.loadNewsDataThomsonReuters <- function(db, companiesIds, index="", initialDate=0, finalDate=0, 
                                       positiveImpactThreshold=51, negativeImpactThreshold=49,
                                       relevance=100, novelty=3, numberExtraDays=0, debug=0) {
    
    TIME = proc.time();
    if (debug >= 0) printf("\nLoading news data from Thomson Reuters...\n");
    
    timeMerge   = 0;
    timeNews    = 0;
    timeProcess = 0;
    
    datesDB = .selectAndCheckDates(db, initialDate, finalDate);
    initialDate = datesDB$date1;
    finalDate   = datesDB$date2;
    rm(datesDB);
    initialDate = as.numeric(addDate(initialDate, -1*numberExtraDays));

    .checkIndex(db, index);

    if (debug > 0) printf("\nLoading news data\nInitial date = %d and final date = %d\n", initialDate, finalDate);
 
    TIME2 = proc.time();
    if (debug > 0 ) printf("Calculating number and preallocating data...");
    numCompanies =  length(companiesIds);
    filters = character(numCompanies);
    numNews = numeric(numCompanies);
    currentIndex = 1;
    for (i in 1 : numCompanies) {
        filters[i] = "";
        if (index != "") {
            stat = paste0("select startDate as date1, endDate as date2 from companyIndex where indexId=\"", index, "\" and companyId = \"", companiesIds[i], "\" and endDate >= ", initialDate, " and startDate <= ", finalDate, " order by startDate");
            #printf("%s\n", stat);
            dates = .query(dbHandle=db, statement=stat);
            if (nrow(dates) == 0) next;
            filters[i] = " AND (";
            for (j in 1 : nrow(dates)) {
                if (j > 1) { 
                    filters[i] = paste0(filters[i], " OR ");
                }
                filters[i] = paste0(filters[i], "(date >= ", dates$date1[j], " AND date <= ", dates$date2[j], ")")
            }
            filters[i] = paste0(filters[i], ")");
        }

        stat = paste0("SELECT count(*) as num FROM newsThomsonReuters",  companiesIds[i], " WHERE date >= ", initialDate, " and date <= ", finalDate, filters[i], " AND relevance >= ", relevance, " AND novelty3d <= ", novelty);
        if (positiveImpactThreshold != negativeImpactThreshold) {
            stat = paste0(stat, sprintf(" AND (sentimentCombined >= %d OR sentimentCombined <= %d)", positiveImpactThreshold, negativeImpactThreshold));
        }
        stat = paste0(stat, " ORDER BY date ASC, hour ASC");
        
        result <- tryCatch({
            news = .query(dbHandle=db, statement=stat);
            numNews[i] = news$num[1];
        }, error = function(err) {
            if (grepl("no such table", err, perl=TRUE)) {
                numNews[i] = 0;
            } else {
                stop0(err);
            }
        });

    }
    sumNews = sum(numNews);
    newsData = data.frame(company=character(sumNews), date=numeric(sumNews), hour=numeric(sumNews), score=numeric(sumNews), stringsAsFactors=FALSE)
    timeNews = timeNews + (proc.time() - TIME2)[3]; 
    if (debug > 0) printf(" %.2fs\n", timeNews);


    for (i in 1 : numCompanies) {
        TIME2 = proc.time();
        if (debug > 0) printf("%03d/%03d Company %s: ", i, length(companiesIds), companiesIds[i]); 
        
        if (numNews[i] > 0) {
            stat = paste0("SELECT '", companiesIds[i], "' as company, date, hour, sentimentCombined as score FROM newsThomsonReuters",  companiesIds[i], " WHERE date >= ", initialDate, " and date <= ", finalDate, filters[i], " AND relevance >= ", relevance, " AND novelty3d <= ", novelty);
            if (positiveImpactThreshold != negativeImpactThreshold) {
                stat = paste0(stat, sprintf(" AND (sentimentCombined >= %d OR sentimentCombined <= %d)", positiveImpactThreshold, negativeImpactThreshold));
            }
            stat = paste0(stat, " ORDER BY date ASC, hour ASC");

            news = .query(dbHandle=db, statement=stat);
            timeNews = timeNews + (proc.time() - TIME2)[3]; 
            
            timeMerge1 = proc.time();
            newsData[currentIndex:(currentIndex+numNews[i]-1),] = news;
            currentIndex = currentIndex + numNews[i];
            timeMerge = timeMerge + (proc.time() - timeMerge1)[3];
        }
        
        if (debug > 0) printf(" %d news, in %.2fs\n", numNews[i], (proc.time() - TIME2)[3]);
    }
       
    timeOrder = proc.time();
    newsData = newsData[with(newsData, order(date, hour)), ];
    timeOrder = (proc.time() - timeOrder)[3];
    
    
    #print(newsData);
    
    TIME = (proc.time() - TIME)[3];
     if (debug >= 0) {
        printf("Number of news found: %d\n", nrow(newsData));
        printf("News data loaded in   %.2fs\n", TIME);
        printf("Time in database      %.2fs\n", timeNews);
        printf("Time in merging       %.2fs\n", timeMerge);
        printf("Time sorting news:    %.2fs\n", timeOrder);
    }
    return(newsData);

}


.loadNewsDataRavenpackCompany <- function(db, company) {
    
    TIME = proc.time();
    printf("Loading news data for %s in... ", company);
    
    stat = paste0("SELECT date, hour, ess, css, relevance, ens, nip, category, categoryGroup FROM news",  company, " ORDER BY date ASC, hour ASC");
    news = .query(dbHandle=db, statement=stat);
    
    TIME = (proc.time() - TIME)[3];
    printf("%.2fs\n", TIME);

    return(news);
}


#
# Select difference of an index time to UTC 
# 
# Parameters:
#  - db     -> Database connection
#  - index  -> Name of the index
#
# Return:
#  - utcDifference -> self explanatory
#  - error:        -> if 1 then there was an error
#  - msg:          -> if error=1 then this holds the error message
.getUTCDifference <- function(db, index="") {

    .checkIndex(db, index);

    stat = paste0("select timezone from marketIndex where id=\"", index, "\"");
    res = .query(dbHandle=db, statement=stat);
    
    utcDifference = res$timezone[1];
    return(utcDifference);
}

# 
# Get market caps for list of companies.
#
# db            -> Database connection
# companies     -> list of companies (Ravenpack code)
# initialDate   -> Initial date for the tests, YYYYMMDD, if 0 then get the oldest date in the database.
# finalDate     -> Final date for the tests, YYYYMMDD, if 0 then get the most recent date in the database.
#
# Return:
#  - marketCap     -> Matrix of market caps, first two columns are initial Date and final Date
#  - error:        -> if 1 then there was an error
#  - msg:          -> if error=1 then this holds the error message
.getMarketCapMatrix <- function(db, index="", companies=character(), date1=0, date2=0) {

    datesDB = .selectAndCheckDates(db, date1, date2);
    date1 = datesDB$date1;
    date2 = datesDB$date2;
    rm(datesDB);

    .checkIndex(db, index);
    if (length(companies) == 0) stop0("No companies found");

    stat = paste0("select distinct startDate from marketWeight where indexId = \"", index, "\" and startDate < ", date2, " and endDate > ", date1);
    res = .query(dbHandle=db, statement=stat);
    dates = res$startDate;

    numCompanies = length(companies);
    numDates = length(dates);
    
    cps = hash();
    for (j in 1 : numCompanies) {
        cps[companies[j]] = j;
        #printf("QQ %s %d\n", companies[j], cps[[companies[j]]]);
    }
    
    marketCap = matrix(0, nrow = numDates, ncol = (numCompanies + 1));
    
    if (numDates > 0) {
        for (i in 1 : numDates) {
            stat = paste0("select companyId as cp, marketWeight as cap from marketWeight where indexId = \"", index, "\" and startDate = ", dates[i], " and companyId IN (");
            for (j in 1 : numCompanies) {
                if (j > 1) stat = paste0(stat, ", ");
                stat = paste0(stat, "\"", companies[j], "\"");
            }
            stat = paste0(stat, ")");
            #print(stat);
            res = .query(dbHandle=db, statement=stat);
        
            marketCap[i, (numCompanies+1)] = dates[i];
            for (j in 1 : nrow(res)) {
                marketCap[i, cps[[res$cp[j]]]] = res$cap[j];
            }
        }
    }
    
   
    return(marketCap);
 
}



# 
# Get list of companies by cumulative market cap including news flow
#
# db            -> Database connection
# index         -> index
# date          -> YYYYMMDD
#
# Return:
#  - data
.getListOfCompaniesMarketCapNewsFlow <- function(db, index="", date=0, relevance=90, novelty=80) {

    stat = paste0("select distinct startDate from marketWeight where indexId = \"", index, "\" and startDate >= ", date," limit 1");
    res = .query(dbHandle=db, statement=stat);
    if (nrow(res) == 0) {
        stop0();
    }
    date1 = res$startDate[1];
    print(date1);

    
    stat = paste0("select c.ravenpackId as id, c.name as name, m.position as position, m.cumulative as cumulative ",
                 "from company as c, companyIndex as i, marketWeight as m ",
                 "where i.indexId=\"", index, "\" ",
                 "and i.companyId = c.ravenpackId ",
                 "and i.companyId = m.companyId ",
                 "and i.indexId = m.indexId ",
                 "and i.startDate < ", date1, " ",
                 "and i.endDate > ", date1, " ",
                 "and m.startDate = ", date1, " ",
                 "order by m.startDate, m.position;");
    res = .query(dbHandle=db, statement=stat);
    res$newsflow <- 0;
    res$newsflowTR <- 0;
    dateA = addDate(date1, -7);

    for (i in 1 : nrow(res)) {
        stat = paste0("SELECT count(*) as num FROM news",  res$id[i], " WHERE date >= ", dateA, " and date <= ", date1, " AND relevance >= ", relevance, " AND (ens >= ", novelty, " OR ens == NULL) ");
        stat = paste0(stat, "ORDER BY date ASC, hour ASC");
        news = .query(dbHandle=db, statement=stat);
        res$newsflow[i] = news$num[1];

        stat = paste0("SELECT count(*) as num FROM newsThomsonReuters",  res$id[i], " WHERE date >= ", dateA, " and date <= ", date1, " AND relevance >= ", relevance, " AND novelty3d <= 3 ");
        stat = paste0(stat, "ORDER BY date ASC, hour ASC");
        news = .query(dbHandle=db, statement=stat);
        #res$newsflowTR[i] = news$num[1];

    }
    
    data = data.frame(name = res$name, position=res$position, cumulative=res$cumulative, newsflow=res$newsflow);
    print(head(data));

    return(data);
 
}

# 
# Get list of companies by cumulative market cap including news flow
#
# db            -> Database connection
# index         -> index
# date          -> YYYYMMDD
#
# Return:
#  - data
.getListOfCompaniesMarketCapNewsFlowWeeks <- function(db, index="", date=0, weeks=1, relevance=90, novelty=80) {

    suppressMessages(library("hash"));

    cpsName  = hash();
    cpsData  = hash();
    allDates = numeric(0);

    dates = numeric(weeks);
    dates[weeks] = date;
    for (i in seq(weeks-1, 1, -1)) {
        dates[i] = addDate(dates[i+1], -7);
    }
    print(dates);

    for (i in 1 : weeks) {
        stat = paste0("select distinct startDate from marketWeight where indexId = \"", index, "\" and startDate <= ", dates[i]," order by startDate desc limit 1");
        res = .query(dbHandle=db, statement=stat);
        if (nrow(res) == 0) {
            stop0();
        }
        date1 = res$startDate[1];
        print(dates[i]);
        allDates = c(allDates, dates[i], dates[i]);
    
    
        stat = paste0("select c.ravenpackId as id, c.name as name, m.position as position, m.cumulative as cumulative ",
                     "from company as c, companyIndex as i, marketWeight as m ",
                     "where i.indexId=\"", index, "\" ",
                     "and i.companyId = c.ravenpackId ",
                     "and i.companyId = m.companyId ",
                     "and i.indexId = m.indexId ",
                     "and i.startDate < ", date1, " ",
                     "and i.endDate > ", date1, " ",
                     "and m.startDate = ", date1, " ",
                     "order by m.startDate, m.position;");
        res = .query(dbHandle=db, statement=stat);
        dateA = addDate(dates[i], -6);

        for (j in 1 : nrow(res)) {
            id = res$id[j];
            if (!has.key(id, cpsName)) {
                cpsName[id] = res$name[j];
            }
            if (!has.key(id, cpsData)) {
                cpsData[id] = numeric(0);
            }
        }        
        allKeys = keys(cpsData);
        for (j in 1 : length(allKeys)) {
            a = cpsData[[allKeys[j]]];
            while (length(a) < 2*(i-1)) {
                cpsData[allKeys[j]] = c(cpsData[[allKeys[j]]], 0, 0, 0, 0);
                a = cpsData[[allKeys[j]]];
            }
        }
        for (j in 1 : nrow(res)) {
            id = res$id[j];
            stat = paste0("SELECT count(*) as num FROM news",  res$id[j], " WHERE date >= ", dateA, " and date <= ", dates[i], " AND relevance >= ", relevance, " AND (ens >= ", novelty, " OR ens == NULL) ");
            stat = paste0(stat, "ORDER BY date ASC, hour ASC");
            news = .query(dbHandle=db, statement=stat);
            totalNEWS = news$num[1];

            stat = paste0("SELECT count(*) as num FROM news",  res$id[j], " WHERE date >= ", dateA, " and date <= ", dates[i], " AND relevance >= ", relevance, " AND (ens >= ", novelty, " OR ens == NULL) AND ((relevance == 100 AND ess > 50) || (relevance < 100 AND css > 50))");
            stat = paste0(stat, "ORDER BY date ASC, hour ASC");
            news = .query(dbHandle=db, statement=stat);
            posNEWS = news$num[1];

            stat = paste0("SELECT count(*) as num FROM news",  res$id[j], " WHERE date >= ", dateA, " and date <= ", dates[i], " AND relevance >= ", relevance, " AND (ens >= ", novelty, " OR ens == NULL) AND ((relevance == 100 AND ess < 50) || (relevance < 100 AND css < 50))");
            stat = paste0(stat, "ORDER BY date ASC, hour ASC");
            news = .query(dbHandle=db, statement=stat);
            negNEWS = news$num[1];

            cpsData[id] = c(cpsData[[id]], res$position[j], totalNEWS, posNEWS, negNEWS);
        
        }
        for (j in 1 : length(allKeys)) {
            a = cpsData[[allKeys[j]]];
            while (length(a) < 2*i) {
                cpsData[allKeys[j]] = c(cpsData[[allKeys[j]]], 0, 0, 0, 0);
                a = cpsData[[allKeys[j]]];
            }
        }
    }
   
    allKeys = keys(cpsName);
    names = character(0);
    for (i in 1 : length(allKeys)) {
        printf("%3d) ", i);
        a = cpsData[[allKeys[i]]];
        if (length(a) > 0) {
            for (j in 1 : length(a)) {
                printf("%3d ", a[j]);
            }
        }
        printf("%40s", cpsName[[allKeys[i]]]);
        printf("\n");
        names = c(names, cpsName[[allKeys[i]]]);
    }
    

    data = data.frame(name = names);
    for (d in 1 : length(dates)) {
        mcap = numeric(0);
        flow = numeric(0);
        posf = numeric(0);
        negf = numeric(0);
        for (i in 1 : length(allKeys)) {
            a = cpsData[[allKeys[i]]];
            mcap = c(mcap, a[(d-1)*4+1]);
            flow = c(flow, a[(d-1)*4+2]);
            posf = c(posf, a[(d-1)*4+3]);
            negf = c(negf, a[(d-1)*4+4]);
        }
        p1 = paste0(dates[d], "mcap");
        p2 = paste0(dates[d], "totalNews");
        p3 = paste0(dates[d], "posNews");
        p4 = paste0(dates[d], "negNews");
        data[, p1] = mcap;
        data[, p2] = flow;
        data[, p3] = posf;
        data[, p4] = negf;
    }
    

    print(head(data));

    return(data);
 
}


.checkConsistencyOfPrices <- function(db, index, date1 = 0, date2 = 0) {

    stat = .createQuerySelectCompanies(index, date1, date2, FALSE, FALSE, 100);

    res = .query(dbHandle=db, statement=stat);
    
    companies = character();
    ids       = character();

    for (i in 1 : nrow(res)) {
        companies = append(companies, res$name[i]);
        ids       = append(ids,       res$id[i]);
    }


    for (i in 1 : length(companies)) {
        
        stat = paste0("select date, close from marketData where companyId=\"", ids[i], "\" and indexId = \"", index, "\" and date >= ", date1,  " and date <= ", date2, " order by date");
        res = .query(dbHandle=db, statement=stat);
        dates = res$date;
        close = as.numeric(res$close);
        numDates = length(dates);
        rm(res);
        printedCompany = 0;
        if (numDates > 0) {
            for (j in 1 : (numDates-1)) {
                d = (close[j+1] - close[j]) / close[j];
                if (d >= 0.4 || d <= -0.4) {
                    if (printedCompany == 0) {
                        printf("Company %s - %d\n", companies[i], numDates);
                        printedCompany = 1;
                    }
                    printf("  %d -> %d: %7.2f to %7.2f\n", dates[j], dates[j+1], close[j], close[j+1]);
                }
            }
        }
    }

}




# FUNCTION ----> computeImpact
#
.computeImpact <- function(db, index="", initialDate, finalDate, companiesIds, dates, relevance=100, 
                          computeImpact="companies", newsSource = "ravenpack",
                          minutesDecay = 120, includeMarketCap = 0, includeCategories = 0, 
                          positiveImpactThreshold=51, negativeImpactThreshold=49) {
   
    numDates = length(dates);
    numCompanies = length(companiesIds);

    impact      = numeric(numDates);
    impactPos   = numeric(numDates);
    impactNeg   = numeric(numDates);
    newsFlow    = numeric(numDates);
    newsFlowPos = numeric(numDates);
    newsFlowNeg = numeric(numDates);
    if (computeImpact == "companies") {
        impact      = matrix(0, nrow=numDates, ncol=numCompanies);
        impactPos   = matrix(0, nrow=numDates, ncol=numCompanies);
        impactNeg   = matrix(0, nrow=numDates, ncol=numCompanies);
        newsFlow    = matrix(0, nrow=numDates, ncol=numCompanies);
        newsFlowPos = matrix(0, nrow=numDates, ncol=numCompanies);
        newsFlowNeg = matrix(0, nrow=numDates, ncol=numCompanies);
    } else if (computeImpact == "both") {
        impact      = matrix(0, nrow=numDates, ncol=numCompanies+1);
        impactPos   = matrix(0, nrow=numDates, ncol=numCompanies+1);
        impactNeg   = matrix(0, nrow=numDates, ncol=numCompanies+1);
        newsFlow    = matrix(0, nrow=numDates, ncol=numCompanies+1);
        newsFlowPos = matrix(0, nrow=numDates, ncol=numCompanies+1);
        newsFlowNeg = matrix(0, nrow=numDates, ncol=numCompanies+1);
    }

    
    ######################################
    ### COMPUTE IMPACT (includes market cap, utc difference)
    marketCap = .getMarketCapMatrix(db=db, index=index, companies=companiesIds, date1=initialDate, date2=finalDate);
    utcDifference = .getUTCDifference(db=db, index=index);

    
    if (newsSource == "ravenpack") {
        # RAVENPACK
        
        ######################################
        ### LOADING NEWS DATA
        newsData = .loadNewsDataRavenpack(db=db, companiesIds=companiesIds, index=index, debug=0, relevance=relevance,
                                         positiveImpactThreshold=positiveImpactThreshold, negativeImpactThreshold=negativeImpactThreshold,
                                         initialDate=initialDate, finalDate=finalDate, numberExtraDays=3);
        ######################################
    
        catFile = "../data/categories/categories1.csv"; 
        if (includeCategories) {
            catFile = "../data/categories/categories2.csv"; 
        }
        categoryGroups = .importCategories(filename=catFile);
    
        impact = NULL;
        if (includeMarketCap) {
            impact = .calculateImpactRavenpack(newsData=newsData, dates=dates, utcDifference=utcDifference, minutesDecay=minutesDecay, marketCap=marketCap,
                                               computeImpact=computeImpact, companies=companiesIds, debug=0, categoryData=categoryGroups);
        } else {
            impact = .calculateImpactRavenpack(newsData=newsData, dates=dates, utcDifference=utcDifference, minutesDecay=minutesDecay,
                                               computeImpact=computeImpact, companies=companiesIds, debug=0, categoryData=categoryGroups);
        }
        impactPos    = impact$impactPos;
        impactNeg    = impact$impactNeg;
        newsFlow     = impact$newsFlow;
        newsFlowPos  = impact$newsFlowPos;
        newsFlowNeg  = impact$newsFlowNeg;
        impact       = impact$impact;
        ######################################
    } else {
        # THOMSON REUTERS

        ######################################
        ### LOADING NEWS DATA
        newsData = .loadNewsDataThomsonReuters(db=db, companiesIds=companiesIds, index=index, debug=0, relevance=relevance, 
                                              positiveImpactThreshold=positiveImpactThreshold, negativeImpactThreshold=negativeImpactThreshold,
                                              initialDate=initialDate, finalDate=finalDate, numberExtraDays=3);
        ######################################
   
        impact = NULL;
        if (includeMarketCap) {
            impact = .calculateImpactThomsonReuters(newsData=newsData, dates=dates, utcDifference=utcDifference, minutesDecay=minutesDecay, 
                                                   marketCap=marketCap, computeImpact=computeImpact, companies=companiesIds, debug=0);
        } else {
            impact = .calculateImpactThomsonReuters(newsData=newsData, dates=dates, utcDifference=utcDifference, minutesDecay=minutesDecay,
                                                   computeImpact=computeImpact, companies=companiesIds, debug=0);
        }
        impactPos   = impact$impactPos;
        impactNeg   = impact$impactNeg;
        newsFlow    = impact$newsFlow;
        newsFlowPos = impact$newsFlowPos;
        newsFlowNeg = impact$newsFlowNeg;
        impact      = impact$impact;
        ######################################

    }


    return(list("impact"=impact, "impactPos"=impactPos, "impactNeg"=impactNeg, "newsFlow"=newsFlow, "newsFlowPos"=newsFlowPos, "newsFlowNeg"=newsFlowNeg));
  
}


