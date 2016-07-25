
numRandomTests    = 100;
verticesInTree    = 50;
minVerticesInTree = 30;

# Settings trading
index       = "FTSE100";
initialDate = 20160101;  # initial date, YYYYMMDD
finalDate   = 20160201;  # initial date, YYYYMMDD

# Settings strategy
inSample               = 500; # Number of in-sample days
rebalanceFrequency     = 5;   # Rebalance every 1 day, or 5 days, etc.
tradeOnCashAsset       = 0;   # 1-yes, 0-no
tradeOnFutures         = 0;   # 1-yes, 0-no
limitNumberOfCompanies = 0;

# Load package
script.dir <- dirname(sys.frame(1)$ofile);
setwd(script.dir);
library("devtools");
load_all("FinancialNetworks");

settingsTrading = initialiseSettingsTrading(index       = index,
                                            initialDate = initialDate,
                                            finalDate   = finalDate);

settingsStrategy = initialiseSettingsStrategy(inSample               = inSample,
                                              rebalanceFrequency     = rebalanceFrequency,
                                              limitNumberOfCompanies = limitNumberOfCompanies,
                                              tradeOnFutures         = tradeOnFutures,
                                              tradeOnCashAsset       = tradeOnCashAsset);

settingsTest = initialiseSettingsTest(numRandomTests    = numRandomTests,
                                      verticesInTree    = verticesInTree,
                                      minVerticesInTree = minVerticesInTree)

settingsNews = initialiseSettingsNews(loadNews = 0)

testAssortativity(settingsTrading  = settingsTrading,
                  settingsStrategy = settingsStrategy,
                  settingsNews     = settingsNews, 
                  settingsTest     = settingsTest);

