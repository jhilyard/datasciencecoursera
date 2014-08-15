corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        f = double()
        for (filename in list.files(directory)) {
                filecontents <- read.csv(file = sprintf("%s/%s",directory,filename),head=TRUE,sep=",")
                if (sum(complete.cases(filecontents)) > threshold) {
                        f <- c(f,cor(unlist(filecontents$sulfate),unlist(filecontents$nitrate),use="complete.obs"))
                }
        }
        f
}