library(av)
library(jpeg)

av_video_images(
  video="Bad-apple.mp4",
  destdir = "Images",
  format = "jpg",
  fps = 30
)


wbdetect <- function(matrix,scale,precision)
{
  dimensions <- floor(scale*dim(matrix)[1:2])
  
  booleanswer <- matrix(FALSE,nrow=floor(dimensions[1]),ncol = floor(dimensions[2]))
  
  for(k in c(1:dimensions[1]))
  {
    for(i in c(1:dimensions[2]))
    {
      if(sum(matrix[k/scale,i/scale,]) < precision*3)
      {
        booleanswer[dimensions[1]-k + 1 ,i] <- TRUE
      }
    }
  }
  return(booleanswer)
}

imagecapturer <- function(n)
{
  number0 <- as.character(rep(0,5-floor(log10(n))))
  vec <- paste(c("Images\\image_",number0,n,".jpg"),collapse="")
  return(readJPEG(vec))
}

lyricfinder <- function(frame,framerate)
{
  seconds <- frame/framerate
  for(i in 1:49)
  {
    if(seconds >= lyrics[i,1]&& seconds < lyrics[i,2])
    {
      return(lyrics[i,3])
    }
  }
  return("")
}

plotting <- function(frames,fps,scaling,...)
{
  for(i in frames[1]:frames[2])
    {
    jpegmatrix <- imagecapturer(i)
    
    resposta <- wbdetect(jpegmatrix,scale=scaling,...)
    
    resposta1 <- which(resposta,arr.ind = T)
    
    lyric <- lyricfinder(i,fps)
    
    plot(0,type="n",xlim=c(0,480*scaling),ylim=c(0,360*scaling),asp=1,main=lyric,xlab="",ylab="")
    
    points(resposta1[,2],resposta1[,1],pch=16,cex=0.0001)
    }
}

lyrics <- read.csv("timestamps.csv",header=T,sep=",")

av_capture_graphics(
  plotting(frames=c(1,6572),scaling=9/10,precision=8/10,fps=30),
  output = "output.mp4",
  width = 480,
  height = 360,
  framerate = 30)

