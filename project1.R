
#########PART A

PCA=function(Images=list.files(getwd())){
  library(OpenImageR)
  vector=matrix(0, nrow=108000, ncol=length(Images))
  #108000 is the number of pixels
  
  #creation of the labels
  people=rep(0,length(Images))
  for(i in 1:length(Images)){
    if (nchar(Images[i])==8){
      people[i]=substr(Images[i],0,2)
    }
    else{
      people [i]=substr(Images[i],0,1)
    }
    
  }
  
  #Transformation of the iamges into vectors, and then
  #all of them contatenated crating a matrix
  for (i in 1:length(Images)){
    Im=readImage(Images[i])
    red=Im[,,1]
    green=Im[,,2]
    blue=Im[,,3]
    
    data=NULL
    data=cbind(data,as.vector(red))  
    data=cbind(data,as.vector(green))
    data=cbind(data,as.vector(blue))
    vector[,i]=c(data[,1],data[,2],data[,3]) #Creation of an image into a vector
    
  }
  
  data_original=t(as.data.frame(vector))#people as rows
  mean_train=apply(data_original,2,mean)
  #we standarize our data substracing the mean
  for (i in 1:nrow(data_original)){
    data_original[i,]=data_original[i,]-mean_train
  }

  data_transposed=t(data_original) #people as cols
  
  #Since the number of observations is much bigger than the number of variables:
  G=data_original
  small=(G%*%t(G))/(nrow(data_original-1))
  
  # Calculation of the eigenvectors and eigenvalues of the small matrix.
  Eigen = eigen(small)
  Eigenvalues = Eigen$values
  
  #the variance explained by each of the principal components.
  Cumulative.Var = cumsum(Eigenvalues)/sum(Eigenvalues)
  Cumulative.Var=round(Cumulative.Var,4)
  Eigenvectors = Eigen$vectors  #our eigenvectors are our new principal axis

  #P is going to be the projection matrix, that will multiply the original 
  #data in order to obtain the projected one.
  P=as.matrix(t(G))%*%as.matrix(Eigenvectors[,1:150]) #matrix to multiply the people
  
  newdata=as.matrix(data_original)%*%as.matrix(P)  #people projected in the new variables
  
  #Selection of 40 first principal components, retaining 95% of the original variance
  newdata=data.frame(newdata=newdata[,1:40], labels=as.integer(people))
  
  #representation
  library(ggplot2)
  ggplot(newdata, aes(x=(newdata[,1]), y=(newdata[,2]),color=factor(newdata$labels)), label = rownames(newdata))+
    geom_point() +xlab("PCA1")+ylab("PCA2")+labs("Representation first 2 principal axis") #people prpojected in the first two pcas, as we can observe, we can distinguish little groups
  #people prpojected in the first two pcas, as we can observe, we can distinguish little groups
  #that represents the people
  
  
  colnames(newdata)=c("PC.1","PC.2","PC.3","PC.4","PC.5","PC.6","PC.7","PC.8","PC.9","PC.10",
                      "PC.11","PC.12","PC.13","PC.14","PC.15","PC.16","PC.17","PC.18","PC.19","PC.20",
                      "PC.21","PC.22","PC.23","PC.24","PC.25","PC.26","PC.27","PC.28","PC.29","PC.30",
                      "PC.31","PC.32","PC.33","PC.34","PC.35","PC.36","PC.37","PC.38","PC.39","PC.40","labels")
  
  return(list(mean=mean_train,D=Eigenvalues,P=P,Var=Cumulative.Var,train=newdata,data_original=data_original,people=people))
}

#onCe the function is created we will use it to do PCA with our dataset, train our algorithm
#an save into an .RData file the objects we will need

partA=PCA()
mean_train=partA$mean
newdata=partA$train
P=partA$P
data_original=partA$data_original
people=partA$people

########DISTANCE MATRIX
#matrix that stores the distance of one photo with the rest.It is used to compute the threshold.
matrix_dist=matrix(0, nrow=nrow(partA$train), ncol=nrow(partA$train))
for (j in 1:nrow(partA$train)){
  for( i in 1:nrow(partA$train)){
    matrix_dist[i,j]=sqrt(sum((partA$train[i,-41] - partA$train[j,-41])^2))
  }
}

ggplot()+aes(x=matrix_dist,color="red",fill="red")+geom_histogram(bins = 25)+geom_vline(xintercept = 5000,color="black")+
  xlab("distances")+labs("distances matrix")
  #As we can observe, the distance between photos of the same person is smaller that the
#distance with one photo of a different person. 

#PART B

#####KNN
#function to predict
KNN=function(data_tr,data_tst,train_labels,k,distance){
  predictions=rep(0,nrow(data_tst))
  for (j in 1:nrow(data_tst)){ #aqui predices cada row del test (25 predicciones)
    dmatrix=dist(rbind(data_tst[j,],data_tr), method = distance, diag = TRUE, upper = TRUE)
    dmatrix=as.matrix(dmatrix)
    dmatrix=dmatrix[1,2:(nrow(data_tr)+1)]
    ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE) #ordea las distancias de menor a mayor
    labels_sel=train_labels[ordenados$ix[1:k]]
    uniqv <- unique(labels_sel)
    predictions[j]=uniqv[which.max(tabulate(match(labels_sel, uniqv)))]}
  return(predictions)
}

#TRAINING OF THE ALGORITHM
nfolds=10
n=nrow(newdata)
folds=sample(cut(1:n,breaks=nfolds,labels=FALSE))

best_accs=NULL #aqui vas a almacenar la mejor acc de cada fold

k=c(3,5,7)
distances=c("euclidean","manhattan","minkowski")
params=expand.grid(k=k,dist=distances)
result=data.frame(params,acc=0)

for (i in 1:nfolds){
  data_tr=newdata[which(folds!=i),-41]
  data_tst=newdata[which(folds==i),-41]
  train_labels=newdata[folds!=i,41]
  test_labels=newdata[folds==i,41]
  
  result$acc=0 #poner a 0 por cada fold
  
  for (p in 1:nrow(result)){ #15 combinaciones
    final=KNN(data_tr,data_tst,train_labels,result$k[p],result$dist[p])
    
    result$acc[p]= mean(final==test_labels) #esto te da la accuracy jeje
    
  }
  
  best=result[which.max(result$acc!=1),] #por cada divison del data una accuracy
  best_accs=rbind(best_accs,best)
}

opt_params=best_accs[which.max(best_accs$acc),]  #the combination that provides the best accuracy
best_accs#optimal parameters
k=opt_params$k
distance=opt_params$dist
save(newdata,P,mean_train,k,distance,people,KNN,file ="data_files.RData" )

#in the classifier function, there is an input for the photo to classify and 
#also another one for the working directory where the rest of photos are.



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
  if (min_distance[1,1]>=5000){
    print(-1)
  }
  else{
    return(str_c("The person belongs to the data set, corresponds to person number ",prediction))
  }
  
  
}
classifier("11AT.jpg")
#"The person belongs to the data set, corresponds to person number 11"

#PART D WITHOUT PCA

#TRAINING OF THE ALGORITHM
KNN_NORMAL=function(data_tr,data_tst,train_labels,k,distance){
  predictions=rep(0,nrow(data_tst))
  for (j in 1:nrow(data_tst)){ #aqui predices cada row del test (25 predicciones)
    dmatrix=matrix(0,(nrow(data_tr)+1),(nrow(data_tr)+1))
    
    dmatrix=dist(rbind(data_tst[j,],data_tr), method = distance) 
    
    dmatrix=as.matrix(dmatrix)
    dmatrix=dmatrix[1,2:(nrow(data_tr)+1)]
    ordenados=sort(dmatrix,index.return=TRUE,decreasing=FALSE) #ordea las distancias de menor a mayor
    labels_sel=train_labels[ordenados$ix[1:k]]
    uniqv <- unique(labels_sel)
    predictions[j]=uniqv[which.max(tabulate(match(labels_sel, uniqv)))]}
  return(predictions)
}

nfolds=10
n=nrow(data_original)
folds=sample(cut(1:n,breaks=nfolds,labels=FALSE))

best_accs=NULL #aqui vas a almacenar la mejor acc de cada fold

k=c(3,5,7)
distances=c("euclidean","manhattan","minkowski")
params=expand.grid(k=k,dist=distances)
result=data.frame(params,acc=0)

for (i in 1:nfolds){
  data_tr=data_original[which(folds!=i),]
  data_tst=data_original[which(folds==i),]
  train_labels=people[folds!=i]
  test_labels=people[folds==i]
  
  result$acc=0 #poner a 0 por cada fold
  
  for (p in 1:nrow(result)){ #15 combinaciones
    final=KNN_NORMAL(data_tr,data_tst,train_labels,result$k[p],result$dist[p])
    
    result$acc[p]= mean(final==test_labels) #esto te da la accuracy jeje
    
  }
  
  best=result[which.max(result$acc),] #por cada divison del data una accuracy
  best_accs=rbind(best_accs,best)
}
k=3
distance="euclidean"
save(k,distance,data_original,mean_train,KNN_NORMAL, people,file = "data_functionNO_PCA.RData")

##FINAL FUNCTION
classifier_without=function(Image){
  load("function_no_pca.RData")
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

classifier_without("8ET.jpg")
#[1] "The person belongs to the data set, corresponds to person number 8"
