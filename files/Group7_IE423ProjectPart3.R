library(jpeg)
library(EBImage)
library(class)
library(kernlab)
library(wvtool)

image_dir <- "/Users/mac/Desktop/2019-2020/IE423/Images"

showlines<-function(img){
img_temp<- img[,,1]
columnmean<-colMeans(img_temp)
rowmean<-rowMeans(img_temp)
plot(c(0:512), ylim=c(0:1),xlab="Columns",ylab="Density(Mean of each column)", col='black', main = "Average of cols and means")
lines(columnmean, col="blue")
lines(rowmean)
}



img1 <- readImage(file.path(image_dir, "Fabric1.jpg"))
grayimg1 <- channel(img1, "gray")

img2 <- readImage(file.path(image_dir, "Fabric2.jpg"))
grayimg2 <- channel(img2, "gray")

img3 <- readImage(file.path(image_dir, "Fabric3.jpg"))
grayimg3 <- channel(img3, "gray")

img4 <- readImage(file.path(image_dir, "Fabric4.jpg"))
grayimg4 <- channel(img4, "gray")

img5 <- readImage(file.path(image_dir, "Fabric5.jpg"))
grayimg5 <- channel(img5, "gray")

img6 <- readImage(file.path(image_dir, "Fabric6.jpg"))
grayimg6 <- channel(img6, "gray")

img7 <- readImage(file.path(image_dir, "Fabric7.jpg"))
grayimg7 <- channel(img7, "gray")

img8 <- readImage(file.path(image_dir, "Fabric8.jpg"))
grayimg8 <- channel(img8, "gray")

img9 <- readImage(file.path(image_dir, "Fabric9.jpg"))
grayimg9 <- channel(img9, "gray")

img10 <- readImage(file.path(image_dir, "Fabric10.jpg"))
grayimg10 <- channel(img10, "gray")

img11 <- readImage(file.path(image_dir, "Fabric11.jpg"))
grayimg11 <- channel(img11, "gray")

img12 <- readImage(file.path(image_dir, "Fabric12.jpg"))
grayimg12 <- channel(img12, "gray")

img13 <- readImage(file.path(image_dir, "Fabric13.jpg"))
grayimg13 <- channel(img13, "gray")

img14 <- readImage(file.path(image_dir, "Fabric14.jpg"))
grayimg14 <- channel(img14, "gray")

img15 <- readImage(file.path(image_dir, "Fabric15.jpg"))
grayimg15 <- channel(img15, "gray")

img16 <- readImage(file.path(image_dir, "Fabric16.jpg"))
grayimg16 <- channel(img16, "gray")

img17 <- readImage(file.path(image_dir, "Fabric17.jpg"))
grayimg17 <- channel(img17, "gray")

img18 <- readImage(file.path(image_dir, "Fabric18.jpg"))
grayimg18 <- channel(img18, "gray")

img19 <- readImage(file.path(image_dir, "Fabric19.jpg"))
grayimg19 <- channel(img19, "gray")

img20 <- readImage(file.path(image_dir, "Fabric20.jpg"))
grayimg20 <- channel(img20, "gray")





gabor1<-gabor.filter(grayimg1, lamda=25, theta=90, bw=1.5, phi=0, asp=0, disp=TRUE)

gabor2<-gabor.filter(grayimg2, lamda=30, theta=90, bw=3, phi=0, asp=0, disp=TRUE)

gabor3<-gabor.filter(grayimg3, lamda=20, theta=90, bw=5, phi=0, asp=1, disp=TRUE)

gabor4<-gabor.filter(grayimg4, lamda=20, theta=90, bw=5, phi=0, asp=0, disp=TRUE)

gabor5<-gabor.filter(grayimg5, lamda=20, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor6<-gabor.filter(grayimg6, lamda=30, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor7<-gabor.filter(grayimg7, lamda=30, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor8<-gabor.filter(grayimg8, lamda=30, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor9<-gabor.filter(grayimg9, lamda=30, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor10<-gabor.filter(grayimg10, lamda=30, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor11<-gabor.filter(grayimg11, lamda=90, theta=90, bw=5, phi=0, asp=0, disp=TRUE)

gabor12<-gabor.filter(grayimg12, lamda=30, theta=90, bw=5, phi=0, asp=0, disp=TRUE)

gabor13<-gabor.filter(grayimg13, lamda=50, theta=90, bw=5, phi=0, asp=0, disp=TRUE)

gabor14<-gabor.filter(grayimg14, lamda=30, theta=90, bw=5, phi=0, asp=0, disp=TRUE)

gabor15<-gabor.filter(grayimg15, lamda=20, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor16<-gabor.filter(grayimg16, lamda=20, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor17<-gabor.filter(grayimg17, lamda=40, theta=0, bw=5, phi=0, asp=0, disp=TRUE)

gabor18<-gabor.filter(grayimg18, lamda=35, theta=45, bw=5, phi=0, asp=1, disp=TRUE)

gabor19<-gabor.filter(grayimg19, lamda=30, theta=45, bw=5, phi=0, asp=1, disp=TRUE)

gabor20<-gabor.filter(grayimg20, lamda=40, theta=60, bw=1.5, phi=0, asp=1, disp=TRUE)


Gaborcevir<- function(gabor){
filtgab<-gabor[4]
output <- matrix(unlist(filtgab), ncol=512, byrow = TRUE)
output<-normalize(output)
return(output)
}


filt1<-Gaborcevir(gabor1)
filt2<-Gaborcevir(gabor2)
filt3<-Gaborcevir(gabor3)
filt4<-Gaborcevir(gabor4)
filt5<-Gaborcevir(gabor5)
filt6<-Gaborcevir(gabor6)
filt7<-Gaborcevir(gabor7)
filt8<-Gaborcevir(gabor8)
filt9<-Gaborcevir(gabor9)
filt10<-Gaborcevir(gabor10)
filt11<-Gaborcevir(gabor11)
filt12<-Gaborcevir(gabor12)
filt13<-Gaborcevir(gabor13)
filt14<-Gaborcevir(gabor14)
filt15<-Gaborcevir(gabor15)
filt16<-Gaborcevir(gabor16)
filt17<-Gaborcevir(gabor17)
filt18<-Gaborcevir(gabor18)
filt19<-Gaborcevir(gabor19)
filt20<-Gaborcevir(gabor20)

controlchartedphoto<-function(grayimage, filteredimage){

  tempmat<-matrix(c(1), nrow = 512, ncol=512)
  tempmat2<-matrix(c(1), nrow = 512, ncol=512)


for (i in 1:512) {
  tempmat[i,]<-tempmat[i,]*(1-(filteredimage[i,]>mean(filteredimage[i,])+3*sd(filteredimage[i,])))*(1-(filteredimage[i,]<mean(filteredimage[i,])-3*sd(filteredimage[i,])))
}

for (i in 1:512) {
  tempmat2[,i]<-tempmat2[,i]*(1-(filteredimage[,i]>mean(filteredimage[,i])+3*sd(filteredimage[,i])))*(1-(filteredimage[,i]<mean(filteredimage[,i])-3*sd(filteredimage[,i])))
}

contchartedimg<-(t(grayimage)*tempmat2*tempmat)
plot(c(0,1300), c(0,512), xlab = "Width", ylab = "Height")
rasterImage(grayimage, 0, 0, 512, 512)
rasterImage(contchartedimg,788,0,1300,512)

}


controlchartedphoto(grayimg1,filt1)
controlchartedphoto(grayimg2,filt2)
controlchartedphoto(grayimg3,filt3)
controlchartedphoto(grayimg4,filt4)
controlchartedphoto(grayimg5,filt5)
controlchartedphoto(grayimg6,filt6)
controlchartedphoto(grayimg7,filt7)
controlchartedphoto(grayimg8,filt8)
controlchartedphoto(grayimg9,filt9)
controlchartedphoto(grayimg10,filt10)
controlchartedphoto(grayimg11,filt11)
controlchartedphoto(grayimg12,filt12)
controlchartedphoto(grayimg13,filt13)
controlchartedphoto(grayimg14,filt14)
controlchartedphoto(grayimg15,filt15)
controlchartedphoto(grayimg16,filt16)
controlchartedphoto(grayimg17,filt17)
controlchartedphoto(grayimg18,filt18)
controlchartedphoto(grayimg19,filt19)
controlchartedphoto(grayimg20,filt20)

controlchartedphoto2sigma<-function(grayimage, filteredimage){
  
  tempmat<-matrix(c(1), nrow = 512, ncol=512)
  tempmat2<-matrix(c(1), nrow = 512, ncol=512)
  
  
  for (i in 1:512) {
    tempmat[i,]<-tempmat[i,]*(1-(filteredimage[i,]>mean(filteredimage[i,])+2*sd(filteredimage[i,])))*(1-(filteredimage[i,]<mean(filteredimage[i,])-2*sd(filteredimage[i,])))
  }
  
  for (i in 1:512) {
    tempmat2[,i]<-tempmat2[,i]*(1-(filteredimage[,i]>mean(filteredimage[,i])+2*sd(filteredimage[,i])))*(1-(filteredimage[,i]<mean(filteredimage[,i])-2*sd(filteredimage[,i])))
  }
  
  contchartedimg<-(t(grayimage)*tempmat2*tempmat)
  plot(c(0,1300), c(0,512), xlab = "Width", ylab = "Height")
  rasterImage(grayimage, 0, 0, 512, 512)
  rasterImage(contchartedimg,788,0,1300,512)
  
}

controlchartedphoto2sigma(grayimg5,filt5)
controlchartedphoto2sigma(grayimg11,filt11)


controlchartedphoto4sigma<-function(grayimage, filteredimage){
  
  tempmat<-matrix(c(1), nrow = 512, ncol=512)
  tempmat2<-matrix(c(1), nrow = 512, ncol=512)
  
  
  for (i in 1:512) {
    tempmat[i,]<-tempmat[i,]*(1-(filteredimage[i,]>mean(filteredimage[i,])+4*sd(filteredimage[i,])))*(1-(filteredimage[i,]<mean(filteredimage[i,])-4*sd(filteredimage[i,])))
  }
  
  for (i in 1:512) {
    tempmat2[,i]<-tempmat2[,i]*(1-(filteredimage[,i]>mean(filteredimage[,i])+4*sd(filteredimage[,i])))*(1-(filteredimage[,i]<mean(filteredimage[,i])-4*sd(filteredimage[,i])))
  }
  
  contchartedimg<-(t(grayimage)*tempmat2*tempmat)
  plot(c(0,1300), c(0,512), xlab = "Width", ylab = "Height")
  rasterImage(grayimage, 0, 0, 512, 512)
  rasterImage(contchartedimg,788,0,1300,512)
  
}

controlchartedphoto4sigma(grayimg18,filt18)
controlchartedphoto4sigma(grayimg19,filt19)
controlchartedphoto4sigma(grayimg20,filt20)