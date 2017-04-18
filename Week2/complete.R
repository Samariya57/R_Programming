complete = function(directory, id = 1:332){
  files_list = list.files(directory, full.names = TRUE)
  data = data.frame()
  for (i in id){
    csvdata = read.csv(files_list[i])
    index = is.na(csvdata$sulfate) | is.na(csvdata$nitrate)
    dat_subset = csvdata[!index,]
    data = rbind(data, c(i,nrow(dat_subset)))
  }
  names(data) = c("id", "nobs")
  data
}