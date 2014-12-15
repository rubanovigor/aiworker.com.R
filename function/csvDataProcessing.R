# -- input: path to *.csv files 
# -- return: eegData.RData
#*****created by aiworker.com 2014*****
csvDataProcessing <- function(directory) {
  
  filenames <- list.files(directory, pattern="*.csv", full.names=T)
  Data = data.frame()
  for (i in seq_along(filenames)) {
    temp = read.csv(filenames[i], sep=";")
    Data = rbind(Data, temp)
  }
  
  
  return (Data) 
}
