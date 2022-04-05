classifier=function(Image){
  
  load("data_files.RData")
  library(stringr)
  #transformation of the image into a vector
  Image=readImage(Image)
  red=Image[,,1]
  green=Image[,,2]
  blue=Image[,,3]
  
  data_image=NULL
  data_image=cbind(data_image,as.vector(red))  
  data_image=cbind(data_image,as.vector(green))
  data_image=cbind(data_image,as.vector(blue))
  new_image=c(data_image[,1],data_image[,2],data_image[,3])
  
  #projection of the image and standarization
  new_image=new_image-mean_train
  new_vector=new_image%*%as.matrix(P[,1:40])  #projection into the new axis
  colnames(new_vector)=colnames(newdata[,-41])
  
  #now we check whether it is a person from our dataset
  prediction=KNN(data_tr=newdata[,-41],data_tst=new_vector,train_labels = newdata[,41],k=k,distance = distance)
  
  #now we check the threshold
  min_distance=as.matrix(dist(rbind(new_vector,newdata[which(prediction==(newdata$labels)),-41]),method="euclidean"))
  if (min_distance[1,1]>=1000){
    print(-1)
  }
  else{
    return(str_c("The person belongs to the data set, corresponds to person number ",prediction))
  }
  
  
}
classifier("11AT.jpg")