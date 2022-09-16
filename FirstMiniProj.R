1.
pollutantmean <- function(directory, pollutant, id = 1:332){
  directory <- paste(getwd(),"/",directory,"/",sep="") #set working directory by getting the current directory
  files_list <- list.files(directory, full.names=TRUE) #makes a list of files in the directory
  dat <- data.frame() #makes an empty data frame
  for (i in id){ #this loop binds all csv files into one data frame
    dat <- rbind(dat, read.csv(files_list[i]))
  }

  mean(dat[,pollutant], na.rm=TRUE) #calculates the mean given the pollutant, with NA values removed
}

pollutantmean("R Files/rprog_data_specdata/specdata", "sulfate", 1:10)
pollutantmean("R Files/rprog_data_specdata/specdata", "nitrate", 70:72)
pollutantmean("R Files/rprog_data_specdata/specdata", "nitrate", 23)
   
2. 
complete <- function(directory, id = 1:332) {
  directory <- paste(getwd(),"/",directory,"/",sep="") #set working directory by getting the current directory
  count_complete <- function(fname){ #creates a function that calls a file
    sum(complete.cases(read.csv(fname)), na.rm=TRUE) #sums up all observed cases in each file that is called
  }
  fnames <- list.files(directory, full.names=TRUE)[id] #lists all the files in the directory by the given id
  data.frame(id = id, nobs = unlist(lapply(fnames, count_complete))) #creates a data frame that computes the sum of the cases in each file specified
}

complete("R Files/rprog_data_specdata/specdata", 1)
complete("R Files/rprog_data_specdata/specdata", c(2,4,8,10,12))
complete("R Files/rprog_data_specdata/specdata", 30:25)
complete ("R Files/rprog_data_specdata/specdata", 3) 
complete ("R Files/rprog_data_specdata/specdata") 

3.
corr <- function(directory, threshold = 0){
  files_full <- list.files(directory, full.names = TRUE) #makes a list of all the files in the directory
  # create empty data set
  dat <- vector(mode = "numeric", length = 0 )#creates an empty numeric data set
  for(i in 1:length(files_full)) #loop that reads the file
  {
    # Read File
    tmp <- read.csv(files_full[i])
    
    #Calculate csum    
    csum <- sum((!is.na(tmp$sulfate)) & (!is.na(tmp$nitrate)))
    if (csum > threshold)
    {
      #Extract data of nitrate and sulfate then calculate correlation between them
      sul <- tmp[which(!is.na(tmp$sulfate)), ]
      nit <- sul[which(!is.na(sul$nitrate)), ]
      dat <- c(dat, cor(nit$sulfate, nit$nitrate))
    }
  }
  return(dat)
}

cr <- corr("rprog_data_specdata/specdata", 150)
head(cr);summary(cr)

cr <- corr("rprog_data_specdata/specdata", 400)
head(cr);summary(cr)

cr <- corr("rprog_data_specdata/specdata", 5000)
head(cr);summary(cr);length(cr)

cr <- corr("rprog_data_specdata/specdata",)
head(cr);summary(cr);length(cr)



4. 
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") #read the outcome data
head(outcome) #looks at the first rows
ncol(outcome) #counts the columns
outcome[, 11] <- as.numeric(outcome[, 11]) #converts column to numeric
hist(outcome[, 11], 
     main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
     xlab = "Deaths") #plots the histogram and edits the labels
