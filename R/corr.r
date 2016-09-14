# Settings trading
index       = "FTSE100";
initialDate = 20160105;  # initial date, YYYYMMDD
finalDate   = 20160105;  # initial date, YYYYMMDD

# Settings strategy
inSample               = 500; # Number of in-sample days
limitNumberOfCompanies = 20;
tradeOnCashAsset       = 0;

# Load package
script.dir <- dirname(sys.frame(1)$ofile);
setwd(script.dir);
library("devtools");
load_all("FinancialNetworks");

settingsTrading = initialiseSettingsTrading(index       = index,
                                            initialDate = initialDate,
                                            finalDate   = finalDate);

settingsStrategy = initialiseSettingsStrategy(inSample               = inSample,
                                              tradeOnCashAsset       = tradeOnCashAsset,
                                              limitNumberOfCompanies = limitNumberOfCompanies);

createCorrelationFile(settingsTrading  = settingsTrading,
                      settingsStrategy = settingsStrategy);

