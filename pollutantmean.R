pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    #Let's make sure the directory end in a '/'
    if(!substr(directory, nchar(directory), nchar(directory))=="/")
    {
        directory <- paste0(directory, "/")
    }

    files <- paste0("00", as.character(id), ".csv")
    files <- sapply(files, 
                function(x) {
                    x <- substr(x, nchar(x)-6, nchar(x))
                    })
    files <- paste0(directory, files)
    
    #Create Vector Store
    vs <- numeric(0)
    
    #Loop though files, populate Vector Store 
    for(x in files)
    {
        #print(x)
        
        #Get the data into our data frame
        df <- read.csv(x)
        
        #Clean it up a bit
        df <- df[!is.na(df[pollutant]), ]
        
        #Add the data to our vector store
        vs <- c(vs, df[[pollutant]])
    }
    
                    
    mean(vs)
}

# pollutantmean("specdata", "sulfate", 1:10)
# pollutantmean("specdata", "nitrate", 70:72)
# pollutantmean("specdata", "nitrate", 23)
