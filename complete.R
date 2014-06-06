complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    #Let's make sure the directory ends in a '/'
    if(!substr(directory, nchar(directory), nchar(directory))=="/")
    {
        directory <- paste0(directory, "/")
    }
    
    #Create Matrix Store
    first_row = TRUE
    
    #Loop though files, populate Vector Store 
    for(i in id)
    {
        x <- paste0("00", as.character(i), ".csv")
        x <- substr(x, nchar(x)-6, nchar(x))
        x <- paste0(directory, x)
        
        #Get the data into our data frame
        df <- read.csv(x)
        
        #Clean it up a bit
        df <- df[complete.cases(df), ]
        
        #Append result to matrix
        if (!first_row)
        {
            mt <- rbind(mt,c(i,nrow(df)))    
        }
        else
        {
            first_row = FALSE
            mt <- data.frame(id=i, nobs=nrow(df))
        }
    }
    
    mt
}

# complete("specdata", 1)
# complete("specdata", c(2, 4, 8, 10, 12))
# complete("specdata", 30:25)
# complete("specdata", 3)