corr = function(directory, threshold = 0){     
  files_list = list.files(directory, full.names = TRUE)
  correlation = c()
  for (i in 1:332){
    csvdata = read.csv(files_list[i])
    if (sum(complete.cases(csvdata))>threshold){
      correlation = c(correlation, cor(csvdata$sulfate,csvdata$nitrate, use = "pairwise.complete.obs"))   
    }
  }
  correlation
}