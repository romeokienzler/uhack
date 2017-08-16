#set the working directory specific to my machine
setwd("/Users/romeokienzler/Documents/proj/1watsoniot/yt/clustering/HMP_Dataset")


#create a data frame from all files in specified folder
create_activity_dataframe = function(activityFolder,classId) {
  file_names = dir(activityFolder)
  file_names = lapply(file_names, function(x){ paste(".",activityFolder,x,sep = "/")})
  your_data_frame = do.call(rbind,lapply(file_names,function(x){read.csv(x,header = FALSE,sep = " ")}))
  your_data_frame = cbind(data.frame(rep(classId,nrow(your_data_frame))),your_data_frame)
  your_data_frame = cbind(data.frame(1:nrow(your_data_frame)),your_data_frame)
  colnames(your_data_frame) = c("timestep","class","x","y","z")
  your_data_frame = cbind(mean(your_data_frame$x),mean(your_data_frame$y),mean(your_data_frame$z),your_data_frame)
  colnames(your_data_frame) = c("mx","my","mz","timestep","class","x","y","z")
  your_data_frame
}

df1 = create_activity_dataframe("Brush_teeth",1)
df2 = create_activity_dataframe("Climb_stairs",2)
df = rbind(df1,df2)
View(df)
library(e1071)

#SPLIT
sample <- sample.int(nrow(df), floor(.75*nrow(df)), replace = F)
dftrain <- df[sample, ]
dftest <- df[-sample, ]
xtrain <- subset(dftrain, select=-dftrain$class)
ytrain <- dftrain$class
xtest <- subset(dftest, select=-dftest$class)
ytest <- dftest$class
svm_model <- svm(xtrain,ytrain)
pred <- predict(svm_model,xtest)
truthVector = round(pred) == dftest$class
good = length(truthVector[truthVector==TRUE])
bad = length(truthVector[truthVector==FALSE])
good/(good+bad)

