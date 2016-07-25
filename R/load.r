# Initial stuff
thisFile <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE);
  needle <- "--file=";
  match <- grep(needle, cmdArgs);
  folder = "";
  if (length(match) > 0) {
    folder = normalizePath(sub(needle, "", cmdArgs[match])); # Rscript
  } else {
    folder = normalizePath(sys.frames()[[1]]$ofile); # 'source'd via R console
  }
  folder = gsub("[a-zA-Z0-9][a-zA-Z0-9]*\\.[rR]", "", folder, perl=TRUE);
  return (folder);
}

setwd(thisFile());

# Get current folder
# Read folder of this script.
#script.dir <- dirname(sys.frame(1)$ofile);
#setwd(script.dir);


if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools");
suppressMessages(library("devtools"));
suppressMessages(library("methods"));
load_all("FinancialNetworks");

