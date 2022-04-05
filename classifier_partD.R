classifier_without=function(Image){
  library(OpenImageR)
  load("data_functionNO_PCA.RData")
  Image=readImage(Image)
  red=Image[,,1]
  green=Image[,,2]
  blue=Image[,,3]
  
  data_image=NULL
  data_image=cbind(data_image,as.vector(red))  
  data_image=cbind(data_image,as.vector(green))
  data_image=cbind(data_image,as.vector(blue))
  new_image=c(data_image[,1],data_image[,2],data_image[,3])
  new_image=new_image-mean_train
  
  #now we check whether it is a person from our dataset
  prediction=KNN_NORMAL(data_tr=data_original,data_tst=as.matrix(t(new_image)),train_labels = people,k=k,distance = distance)
  
  #now we check the threshold
  min_distance=as.matrix(dist(rbind(new_image,data_original[which(prediction==(people)),]),method="euclidean"))
  if (min_distance[1,1]>=5000){
    print(-1)
  }
  else{
    return(str_c("The person belongs to the data set, corresponds to person number ",prediction))
  }
}
