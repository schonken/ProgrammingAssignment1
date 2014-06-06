corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    files <- list.files(pattern = "\\.csv$", path = directory)
    #files <- list.files(pattern = "324.csv$", path = directory)
    result <- numeric()
    
    for(file in files)
    {
        
        #Get the data into our data frame
        df <- read.csv(paste0(directory, "/", file))
        
        #Clean it up a bit
        df <- df[complete.cases(df), ]
        
        if (nrow(df) > threshold)
        {
            #print(file)
            #print(nrow(df))
            
            result <- c(result, cor(df$nitrate, df$sulfate))
        }
    }
    
    result
}

# source("complete.R")
# cr <- corr("specdata", 150)
# head(cr)
# summary(cr)
# cat("\n\n")