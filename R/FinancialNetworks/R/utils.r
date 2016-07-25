
# Function printf
printf <- function(...) invisible(cat(sprintf(...)))

stop0 <- function(...) stop(..., call.=FALSE);

# TODO
# This function is not beautiful, it is full of workarounds and
# at some point we should redo it properly. It should find the base path of the
# script being called in these situations:
#
# - Running an experiment (like ssd.r) from Rstudio
# - Calling a function from the package straight from Rstudio
# - Running something like runManyExperiments using Rscript
# - From the dashboard
# - Running a new experiment with the dashboard (which uses Rscript internally)
findBasePath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE);
  needle <- "--file=";
  match <- grep(needle, cmdArgs);
  folder = "";

  if (length(match) > 0) {
    #Rscript
    f = sub(needle, "", cmdArgs[match]);
    leave = 0;
    while (!leave) {
        folder = suppressWarnings(normalizePath(f));
        if (!file.exists(folder)) {
            if (grepl("^\\.\\.\\/", f, perl=TRUE)) {
                 f = gsub("^\\.\\.\\/", "", f, perl=TRUE);
            } else {
                printf("This should never have happened\n");
                leave = 1; 
            }
        } else {
            leave = 1;
        }
    }
  } else {
    error = tryCatch( { 
        folder = normalizePath(sys.frames()[[1]]$ofile); # 'source'd via R console
    }, error = function (e) {
        folder = getSrcDirectory(function(x) {x});
    });

  }
  folder = gsub("[a-zA-Z0-9][a-zA-Z0-9]*\\.[rR]$", "", folder, perl=TRUE);
  return (folder);
}

insertRow <- function(existingDF, newrow, r) {
    existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
    existingDF[r,] <- newrow
    return(existingDF);
}


getCurrentDate <- function() {
    date = Sys.Date();
    date = convertDate2(date);
}

getCurrentHour <- function() {
    time  = Sys.time();
    time2 = format(strptime(time, "%Y-%m-%d %H:%M:%S"));
    time2 = gsub("^.* ", "", time2);
    time2 = gsub(":", "", time2);
    return(time2);
}

checkPackage <- function(package) {
    
    if (!package %in% rownames(installed.packages())) {
        install.packages(package);
    }
}

annualRateToDaily <- function(rfr) {
    return((rfr + 1)^(1/252) - 1);
}

differenceBetweenSameNumbersInTwoVectors <- function(v1, v2) {
    if (!is.numeric(v1)) stop0("v1 is not numeric");
    if (!is.numeric(v2)) stop0("v1 is not numeric");
    if (length(v1) != length(v2)) stop0("v1 and v2 have different lengths");

    diff = 0;
    for (i in 1 : length(v1)) {
        for (j in 1 : length(v1)) {
            if (v1[i] == v2[j]) diff = diff + abs(i - j);
        }
    }
    return(diff);
}


skewnessUnbiased <- function (x, na.rm = FALSE) {
    if (is.matrix(x)) 
        apply(x, 2, skewness, na.rm = na.rm)
    else if (is.vector(x)) {
        if (na.rm) 
            x <- x[!is.na(x)]
        n <- length(x)
        (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/(n-1))^(3/2)
    }
    else if (is.data.frame(x)) 
        sapply(x, skewness, na.rm = na.rm)
    else skewness(as.vector(x), na.rm = na.rm)
}

checkDateFormat <- function(date) {
    
    grepl("^[0-9]{8}$", as.character(date), perl=TRUE)
}

dateInPrintFormat <- function(date) {
    date2 = format(strptime(date, "%Y%m%d"),"%d/%m/%Y");
    return(date2);
}

convertDate <- function(date) {
    date2 = format(strptime(date,"%d/%m/%Y"),"%Y%m%d");
    return(date2);
}

convertDate2 <- function(date) {
  date2 = format(strptime(date,"%Y-%m-%d"),"%Y%m%d");
  return(as.numeric(date2));
}


formatYahoo <- function(date) {
    date2 = format(strptime(date,"%Y%m%d"),"%Y-%m-%d");
    return(date2);
}

addDate2 <- function(date, numDays) {
  
    if (numDays < 0) return(subtractDate(date, numDays));

    year = floor(date / 10000);
    month = floor((date - (year*10000)) / 100);
    day = date - year*10000 - month*100;
    
    ## do for non leap years every 400 years
    for (i in 1 : numDays) {
        day = day + 1;
        if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) {
            if (day > 31) {
                day = 1;
                month = month + 1;
                if (month == 13) {
                    month = 1;
                    year = year + 1;
                }
            }
        } else if (month == 4 || month == 6 || month == 9 || month == 11) {
            if (day > 30) {
                day = 1;
                month = month + 1;
            }
        } else {
            if (year %% 4 == 0) {
                if (day > 29) {
                    day = 1;
                    month = month + 1;
                }
            } else {
                if (day > 28) {
                    day = 1;
                    month = month + 1;
                }
            }
        }
    }
    
    date2 = year * 10000 + month * 100 + day;
    return(date2);
}

subtractDate <- function(date, numDays) {
   
    year = floor(date / 10000);
    month = floor((date - (year*10000)) / 100);
    day = date - year*10000 - month*100;
    
    ## do for non leap years every 400 years
    if (numDays < 0) numDays = -numDays;
    for (i in 1 : numDays) {
        day = day - 1;
        if (day == 0) {
            month = month - 1;
        }
        if (month == 0) {
            year = year - 1;
            month = 12;
        }
        
        if (day == 0) {
            if (month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12) {
                day = 31;
            } else if (month == 4 || month == 6 || month == 9 || month == 11) {
                day = 30;
            } else {
                if (year %% 4 == 0) {
                    day = 29;
                } else {
                    day = 28;
                }
            }
        }
    }
    
    date2 = year * 10000 + month * 100 + day;
    return(date2);
}




addDate <- function(date, numDays) {
    date2 = as.Date(as.character(date), format="%Y%m%d") + numDays;
    date2 = format(strptime(date2,"%Y-%m-%d"),"%Y%m%d");
    return(as.numeric(date2));
}

getYear <- function(date) {
    return (floor(date/10000));
}

getMonth <- function(date) {
    return (floor (date / 100) - floor(date/10000) * 100);
}

dayOfTheWeek <- function(date) {
    return (weekdays( as.Date(as.character(date), format="%Y%m%d") ));
}

isThursday <- function(date) {
    day = dayOfTheWeek(date);
    if (day == "Thursday" || day == "quinta" || day == "Donnerstag") return(1);
    return(0);
}

isExpirationDate <- function(date, dateAfter = 0) {
    if (isThursday(date) && getMonth(date) != getMonth(addDate2(date, 7))) return(1);
   
    if (dateAfter > 0) {
        lastThursday = findLastThursdayOfPreviousMonth(addDate2(date, 7));
        if (date < lastThursday && dateAfter > lastThursday) return (1);
    }

    # TODO case where no dateAfter is given and expirationDate is not on a Thursday.
    return(0);
}

findLastThursdayOfPreviousMonth <- function(date) {
   
    # Check if date is the last thursday of the month
    if (isThursday(date) && getMonth(date) != getMonth(addDate2(date, 7))) return(date);

    # Find the last thursday prior to date
    thursday = date;
    while (!isThursday(thursday)) thursday = addDate2(thursday, -1);
    
    # Check if this thursday is the last one of the month
    # That would mean that date is between the last thursday of the month 
    # and the end of the month
    if (getMonth(thursday) != getMonth(addDate2(thursday, 7))) return(thursday);
    
    # Otherwise find the last thursday of the previous month
    month = getMonth(date);
    while (getMonth(thursday) == month) thursday = addDate2(thursday, -7);

    return(thursday);
}


maximumZerosInARow <- function(data) {
    max = 0;
    currentFirstZero = -1;
    for (i in 1 : length(data)) {
        if (data[i] == 0 && currentFirstZero == -1) {
            currentFirstZero = i;
        }
        if (currentFirstZero != -1 && (i == length(data) || data[i] != 0)) {
            temp = i - currentFirstZero;
            if (i == length(data)) temp = temp + 1;
            if (temp > max) max = temp;
            currentFirstZero = -1;
        }
    }
    return(max);
}

#
# Dates format YYYYMMDD
# Hours format HHMMSS
#
# 
differenceMinutes <- function(date1, hour1, date2, hour2) {
    if (date2 < date1 || (date2 == date1 && hour2 < hour1)) {
        tempH = hour1;
        tempD = date1;
        hour1 = hour2;
        date1 = date2;
        hour2 = tempH;
        date2 = tempD;
    }

    hour2 = floor (hour2/100);
    hour1 = floor (hour1/100);
    #days = differenceInDays(date1, date2);
    days = differenceInDays2(date1, date2);
    #days = 0;

    minutes = 0;
    if (days > 0) {
        if (hour2 >= hour1) {
            minutes = days * 24 * 60;
        } else {
            minutes = (days-1) * 24 * 60;
        }
    }

    hh2 = floor (hour2 / 100);
    mm2 = hour2 - hh2*100;
    
    hh1 = floor(hour1/100);
    mm1 = hour1 - hh1*100;

    if (hour2 < hour1) {
        hour2 = hour2 + 2400;
        hh2 = hh2 + 24;
    }        
    minutes = minutes + (hh2 - hh1)*60;
    
    if (mm2 < mm1) {
        minutes = minutes - (mm1 - mm2);
    } else {
        minutes = minutes + (mm2 - mm1);
    }
    return(minutes);
}

differenceMinutes2 <- function(date1, hour1, date2, hour2) {
    d1 = strptime(sprintf("%d %06d", date1, hour1), format="%Y%m%d %H%M%S");
    d2 = strptime(sprintf("%d %06d", date2, hour2), format="%Y%m%d %H%M%S");
    return(abs(as.numeric(difftime(d1, d2, units="mins"))));
}

differenceMinutes3 <- function(dateStrptime, date2, hour2) {
    d2 = strptime(sprintf("%d %06d", date2, hour2), format="%Y%m%d %H%M%S");
    return(abs(as.numeric(difftime(dateStrptime, d2, units="mins"))));
}

differenceMinutes4 <- function(dateStrptime1, dateStrptime2) {
    return(abs(as.numeric(difftime(dateStrptime1, dateStrptime2, units="mins"))));
}

differenceMinutes5 <- function(date1, hour1, date2, hour2) {
    if (date2 < date1 || (date2 == date1 && hour2 < hour1)) {
        tempH = hour1;
        tempD = date1;
        hour1 = hour2;
        date1 = date2;
        hour2 = tempH;
        date2 = tempD;
    }

    hour2 = hour2/100;
    hour1 = hour1/100;
    

    days = differenceInDays(date1, date2);

    minutes = 0;
    if (days > 0) {
        if (hour2 >= hour1) {
            minutes = days * 24 * 60;
        } else {
            minutes = (days-1) * 24 * 60;
        }
    }

    hh2 = floor (hour2 / 100);
    mm2 = hour2 - hh2*100;
    
    hh1 = floor(hour1/100);
    mm1 = hour1 - hh1*100;

    if (hour2 < hour1) {
        hour2 = hour2 + 2400;
        hh2 = hh2 + 24;
    }        
    minutes = minutes + (hh2 - hh1)*60;
    
    if (mm2 < mm1) {
        minutes = minutes - (mm1 - mm2);
    } else {
        minutes = minutes + (mm2 - mm1);
    }
    return(minutes);
}

.plotPortfolioGraphic <- function(portfolio, benchmark, dates, nameBenchmark="Benchmark", title="Plot", namePortfolio="Portfolio") {
    
    if (!is.numeric(portfolio)) stop0("plotPortfolio: portfolio is not numeric");
    if (!is.numeric(benchmark)) stop0("plotPortfolio: benchmark is not numeric");
    if (!is.numeric(dates))    stop0("plotPortfolio: dates is not numeric");
    
    lengthLine = 2;
    datesForm = format(strptime(dates, "%Y%m%d"),"%d/%m/%Y");
    xx = seq(from=1, to=length(dates), by=1);
    ylim = c(min(portfolio, benchmark)*0.997, max(portfolio, benchmark)*1.003);
    xlim = c(1, dates[length(dates)]);
    xlim = c(1, xx[length(xx)]);
    plot(x=portfolio, type="l", col="blue", lwd=lengthLine, ylab="Value", main=title, ylim=ylim, xlim=xlim, xaxt='n', xaxs="i", yaxs="i");
    lines(x=benchmark, col="red", lwd=lengthLine);
    axis(side=1, at=xx, label=datesForm);
    legend(x='topleft', legend=c(nameBenchmark, namePortfolio), lty=1, lwd=lengthLine, col=c('red', 'blue'), bty='n');
}

.plotTwoPortfoliosGraphic <- function(portfolio1, portfolio2, benchmark, dates, nameBenchmark="Benchmark", title="Plot", 
                                     namePortfolio1="Portfolio 1", namePortfolio2="Portfolio 2") {
    
    if (!is.numeric(portfolio1)) stop0("portfolio1 is not numeric");
    if (!is.numeric(portfolio2)) stop0("portfolio2 is not numeric");
    if (!is.numeric(benchmark))  stop0("benchmark is not numeric");
    if (!is.numeric(dates))      stop0("dates is not numeric");
    
    lengthLine = 2;
    datesForm = format(strptime(dates, "%Y%m%d"),"%d/%m/%Y");
    xx = seq(from=1, to=length(dates), by=1);
    ylim = c(min(portfolio1, portfolio2, benchmark)*0.997, max(portfolio1, portfolio2, benchmark)*1.003);
    xlim = c(1, dates[length(dates)]);
    xlim = c(1, xx[length(xx)]);
    plot(portfolio1, type="l", col="blue", lwd=lengthLine, ylab="Value", main=title, ylim=ylim, xlim=xlim, xaxt='n', xaxs="i", yaxs="i");
    lines(benchmark, col="red", lwd=lengthLine);
    lines(portfolio2, col="black", lwd=lengthLine);
    axis(side=1, at=xx, label=datesForm);
    legend(x='topleft', legend=c(nameBenchmark, namePortfolio1, namePortfolio2), lty=1, lwd=lengthLine, col=c('red', 'blue', 'black'), bty='n');
}





differenceInDays <- function(date1, date2) {
    date1 = as.Date(as.character(date1), format="%Y%m%d");
    date2 = as.Date(as.character(date2), format="%Y%m%d");
    d = as.numeric(date2 - date1);
    return(d);
}

differenceInDays2 <- function(date1, date2) {
    year1 = floor(date1 / 10000);
    month1 = floor((date1 - (year1*10000)) / 100);
    day1 = date1 - year1*10000 - month1*100;
    
    year2 = floor(date2 / 10000);
    month2 = floor((date2 - (year2*10000)) / 100);
    day2 = date2 - year2*10000 - month2*100;
    
    days = 0;
    while (day1 < day2 || month1 < month2 || year1 < year2) {
        days = days + 1;
        day2 = day2 - 1;
        if (day2 == 0) {
            month2 = month2 - 1;
            if (month2 == 0) {
                year2 = year2 - 1;
                month2 = 12;
                day2 = 31;
            } else {
                if (month2 == 1 || month2 == 3 || month2 == 5 || month2 == 7 || month2 == 8 || month2 == 10 || month2 == 12) {
                    day2 = 31;
                } else if (month2 == 4 || month2 == 6 || month2 == 9 || month2 == 11) {
                    day2 = 30;
                } else { 
                    if (year2 %% 4 == 0) {
                        day2 = 29;
                    } else {
                        day2 = 28;
                    }
                }
            }
        }
    }
    return(days);
}


#
#
#
#
#
subtractMinutes <- function(date1, hour1, minutes) {
    
    hour1 = as.numeric(hour1);
    date1 = as.numeric(date1);

    minutes = round(minutes);
    minutesSub = minutes %% 60;
    hoursSub = round((minutes - minutesSub) / 60);
    #printf("Hours sub = %d and %d\n", hoursSub, minutesSub);
    
    hour2 = hour1;
    date2 = date1;

    hour2 = hour2 - (hoursSub*10000);
    while (hour2 < 0) {
        date2 = addDate(date2, -1);
        hour2 = hour2 + 240000;
    }

   
    #printf("Hour 1 = %06d and Hour 2 = %06d, dates are %d and %d\n", hour1, hour2, date1, date2);
    
    hh2 = floor(hour2/10000);
    minutes2 = round((hour2 - (hh2*10000))/100);
    hour2 = hour2 - (minutesSub*100);
    if (minutes2 < minutesSub) {
        hour2 = hour2 - 4000;
    }
    if (hour2 < 0) {
        date2 = addDate(date2, -1);
        hour2 = hour2 + 240000;
    }
    #printf("Hour 1 = %06d and Hour 2 = %06d, dates are %d and %d (minutes2 %d, minutesSub %d)\n", hour1, hour2, date1, date2, minutes2, minutesSub);

    return(list("date"=date2, "hour"=hour2));
}


updateHourTimezone <- function(hour, utcDifference) {
   
    hour = as.numeric(hour);
    hh = floor(hour/10000);
    mm = (floor(hour/100) - (hh*100))/60;
    hh = hh+mm;
    
    hour2 = hh - utcDifference;
    

    
    daysAdjusted = 0;
    if (hour2 >= 24) {
        daysAdjusted = 1;
        hour2 = hour2 - 24;
    } else if (hour2 < 0) {
        daysAdjusted = -1;
        hour2 = hour2 + 24;
    }
    
    mm = (hour2 - floor(hour2)) * 60;
    hour3 = floor(hour2)*10000 + round(mm)*100;
    
    #printf("Hour = %d - %.2f became %d, daysAdjusted = %d\n", hour, utcDifference, hour3, daysAdjusted); 
    return(list("hour"=hour3, "daysAdjusted"=daysAdjusted)); 
}

# existing Data frame
# row to be inserted
# position
insertRowInDataFrame <- function(existingDF, newrow, pos) {
  existingDF[seq(pos+1,nrow(existingDF)+1),] <- existingDF[seq(pos,nrow(existingDF)),]
  existingDF[pos,] <- newrow
  return(existingDF);
}

#
# Calculate lambda for the impact function
# 
# Parameters:
# - minutes   -> Lambda will be the value such that the impact decays
#                to half its value in "minutes" minutes.
calculateLambda <- function(minutes) {
    if (!is.numeric(minutes)) return (0);
    x = log(0.5) / (-1*minutes/90);
    return (x);
}

#
# Given lambda and a lower limit, calculates when to stop calculating 
# impact 
# 
# Parameters:
# - lambda    -> the value which will decide how long the impact
#                needs to decay to half its value
# - threshold -> desired threshold level to not include news in the
#                impact calculation (in percentage of the original value)
#
# Return:
# - minutes -> Minutes before current date so as to stop including news in 
#              the impact calculation
#
thresholdForImpactCalculation <- function(lambda = log(2), threshold = 0.001) {
    if (!is.numeric(lambda)) return (0);
    if (!is.numeric(threshold)) return (0);
    if (threshold <= 0 || threshold >= 1) return (0);

    minutes = round(log(threshold) / (-1*lambda/90));
    return (minutes);
}


# 
#
#
#
flipSignals <- function(x, seed = 20, amountFlipped = 1.0) {

    if (amountFlipped < 0 || amountFlipped > 1) stop0("Amount flipped must be between 0 and 1");
    if (amountFlipped == 1) {
        x = -1*x;
    } else if (amountFlipped > 0) {
        set.seed(seed);
        temp = seq(from=1, to=length(x));
        num = floor(amountFlipped * length(temp));
        temp = sample(temp, num, replace = FALSE);
        x[temp] = x[temp]*-1;
    }
    return (x);
}

rightOrWrong <- function(a, b, tolerance = 0) {
    if (tolerance < 0) stop0("Tolerance must be greater or equal to zero");

    pos = 0 + tolerance;
    neg = 0 - tolerance;
    if ((a > pos && b > 0) || (a < neg && b < 0)) {
        return (1);
    } 
    if ((a > pos && b < 0) || (a < neg && b > 0)) {
        return(-1);
    } 
    return (0);
}

computeDominance <- function(x, y) {
    if (!is.numeric(x) || !is.numeric(y)) stop0("computeDominance: x and y must be numeric.");
    if (length(x) != length(y)) stop0("computeDominance: length of x and y must be equal.");

    dominance = 999999999;
    sumX = 0;
    sumY = 0;
    for (i in 1 : length(x)) {
        sumX = sumX + x[i];
        sumY = sumY + y[i];
        if ((sumX - sumY) < dominance) {
            dominance = sumX - sumY;
        }
    }

    return(dominance);
}

computeNumDominance <- function(x, y) {
    if (!is.numeric(x) || !is.numeric(y)) stop0("computeDominance: x and y must be numeric.");
    if (length(x) != length(y)) stop0("computeDominance: length of x and y must be equal.");

    numDom = 0;
    sumX = 0;
    sumY = 0;
    for (i in 1 : length(x)) {
        sumX = sumX + x[i];
        sumY = sumY + y[i];
        if (sumX > sumY) {
            numDom = numDom+1;
        }
    }

    return(numDom);
}
