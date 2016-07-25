

.prepareDataForScenarios <- function(returns = matrix(0, nrow=0, ncol=0), topCompanies=numeric(), wasInIndex=numeric()) {


    TIME = proc.time();
    
    temp = numeric();
    if (!is.matrix(returns))                      stop0("Returns must be a matrix.");
    if (nrow(returns) == 0 || ncol(returns) == 0) stop0("Returns matrix has a dimension of size zero.");
    
    if (is.numeric(wasInIndex) && length(wasInIndex) > 0) {
        if (ncol(returns) != length(wasInIndex)) stop0("returns must have the same number of columns as the length of wasInIndex.");
    } else {
        wasInIndex = rep(1, ncol(returns));
    }

    if (is.numeric(topCompanies) && length(topCompanies > 0)) {
        if (ncol(returns) != length(topCompanies)) stop0("If specified top companies have the same value as the number of columns of the return matrix.");
    } else {
        topCompanies = rep(1, ncol(returns));
    }

    # TILMAN ZRYAN If you want to use the threshold of 2%, change this line to zero
    useZerosInARow = 1;

    # Used for the threshold, if you want to try others (like 4%) change here
    maximumZeros = max(1, 0.02 * nrow(returns));

    scenarios = returns;
    columns = integer();
    for (i in seq(from=ncol(returns), to=1, by=-1)) {
        a = returns[,i];
        
        accepted = length(a[a == 0]) <= maximumZeros && wasInIndex[i] == 1 && topCompanies[i] == 1;
        if (useZerosInARow) accepted = maximumZerosInARow(a) <= 2 && wasInIndex[i] == 1 && topCompanies[i] == 1;
        
        if (accepted) {
            columns = c(columns, i);
        } else {
            scenarios <- scenarios[,-i];
        }
        
    }

    columns = rev(columns);

    return(list("columns"=columns, "scenarios"=scenarios));
}

