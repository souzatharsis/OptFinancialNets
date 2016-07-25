.errorCheckSettingsObjNotNull <- function(param, name = deparse(substitute(param))) {
    if (!class(param) == paste0(name, "Object")) stop0(paste0("Error: Wrong type for ", name, " (should be ", name, "Object."));
}

.errorCheckSettingsObjCanBeNull <- function(param, name = deparse(substitute(param))) {
    if (!is.null(param) && !class(param) == paste0(name, "Object")) stop0(paste0("Error: Wrong type for ", name, " (should be ", name, "Object."));
}


.errorCheckBinaryParameter <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || (param != 0 && param != 1)) stop0(paste0("Error: Parameter ", name, " should be numeric and 0 or 1."));
}

.errorCheckTernaryParameter <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || !(param %in% c(0, 1, 2))) stop0(paste0("Error: Parameter ", name, " should be numeric and 0, 1 or 2."));
}

.errorCheckStrictlyPositive <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param <= 0) stop0(paste0("Error: Parameter ", name, " must be strictly positive"));
}

.errorCheckLessThanOneAndMoreThanMinusOne <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param <= -1 || param >= 1) stop0(paste0("Error: Parameter ", name, " must be > -1 and < 1."));
}

.errorCheckNonNegativeLessThanOne <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param < 0 || param >= 1) stop0(paste0("Error: Parameter ", name, " must be >= 0 and < 1."));
}

.errorCheckNonNegative <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param < 0) stop0(paste0("Error: Parameter ", name, " must be nonnegative."));
}

.errorCheckBetweenZeroAndOne <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param < 0 || param > 1) stop0(paste0("Error: Parameter ", name, " must be >= 0 and <= 1."));
}

.errorCheckPositiveInteger <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param <= 0 || param%%1 != 0) stop0(paste0("Error: Parameter ", name, " must be a positive integer."));
}

.errorCheckNonNegativeInteger <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param < 0 || param%%1 != 0) stop0(paste0("Error: Parameter ", name, " must be a nonnegative integer."));
}

.errorCheckBinaryParameter <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || (param != 0 && param != 1)) stop0(paste0("Error: Parameter ", name, " should be numeric and 0 or 1."));
}

.errorCheckNumeric <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param)) stop0(paste0("Error: Parameter ", name, " must be numeric."));
}

.errorCheckGreaterThanMinusOne <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param <= -1) stop0(paste0("Error: Parameter ", name, " must be > -1."));
}

.errorCheckGreaterOrEqualToOne <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param < 1) stop0(paste0("Error: Parameter ", name, " must be >= 1."));
}

.errorCheckIntegerBetweenZeroAnd100 <- function(param, name = deparse(substitute(param))) {
    if (!is.numeric(param) || param < 0 || param > 100 || param%%1 != 0) stop0(paste0("Error: Parameter ", name, " must be integer >= 0 and <= 100."));
}




.errorCheckNewsDataRavenpack <- function(newsData) {
    if (!is.data.frame(newsData))stop0("Error: News Data is not a data frame.");
    if(!("company" %in% colnames(newsData)))stop0("Error: News Data does not have column \"company\".");
    if(!("date" %in% colnames(newsData)))stop0("Error: News Data does not have column \"date\".");
    if(!("hour" %in% colnames(newsData)))stop0("Error: News Data does not have column \"hour\".");
    if(!("score" %in% colnames(newsData)))stop0("Error: News Data does not have column \"score\".");
    if(!("categoryGroup" %in% colnames(newsData)))stop0("Error: News Data does not have column \"categoryGroup\".");
}

.errorCheckNewsDataThomsonReuters <- function(newsData) {
    
    if (!is.data.frame(newsData))stop0("Error: News Data is not a data frame.");
    if(!("company" %in% colnames(newsData)))stop0("Error: News Data does not have column \"company\".");
    if(!("date" %in% colnames(newsData)))stop0("Error: News Data does not have column \"date\".");
    if(!("hour" %in% colnames(newsData)))stop0("Error: News Data does not have column \"hour\".");
    if(!("score" %in% colnames(newsData)))stop0("Error: News Data does not have column \"score\".");
}



.errorCheckCategoryData <- function(categoryData) {
    if (!is.hash(categoryData)) stop0("Error: Category data is not a hash.");
}

.errorCheckNumericVector <- function(name="Data", numVector) {
    if (!is.numeric(numVector))stop0(paste0("Error: ", name, " is not a numeric vector."));
    if (length(numVector) == 0)stop0(paste0("Error: ", name, " vector has a length of zero."));
}

.errorCheckCharacterVector <- function(name="Data", charVector) {
    if (!is.character(charVector))stop0(paste0("Error: ", name, " is not a character vector."));
    if (length(charVector) == 0)stop0(paste0("Error: ", name, " vector has a length of zero."));
}


.errorCheckCompanies <- function(companies) {
    if (!is.character(companies))stop0("Error: Companies is not a character vector.");
    if (length(companies) == 0)stop0("Error: Companies vector has a length of zero.");
}

.errorCheckUTC <- function(utc) {
    if (!is.numeric(utc)) stop0("Error in UTC difference: Value must be numeric.");
    if (utc <= -15 || utc >= 15) stop0("Error in UTC difference: Value is invalid.");
}

.errorCheckHour <- function(hour) {
    if (length(grep("^[0-2][0-9][0-5][0-9][0-9][0-9]$", as.character(hour), perl=TRUE)) == 0) {
       stop0("Error in hour: Format must be HHMM.");
    }
    if (as.numeric(hour) >= 200000) {
        if (length(grep("^2[0-3]", as.character(hour), perl=TRUE)) == 0) {
           stop0("Error in hour: Format must be HHMM from 0000 to 2359.");
        }
    }
}

.errorCheckTopCompanies <- function(limitToTopCompanies, limitByPercentageMV, valueTopCompanies) {
    if (limitToTopCompanies && limitByPercentageMV && valueTopCompanies > 100) {
       stop0("Error in valueTopCompanies: Percentage must be less or equal to 100.");
    }
    if (valueTopCompanies <= 0) {
       stop0("Error in valueTopCompanies: Number must be positive.");
    }
}

.errorCheckClassPortfolio <- function(port) {
    if (class(port) != "portfolio")
        stop0("Port object is not of class portfolio");
}
