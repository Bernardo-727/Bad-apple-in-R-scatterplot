<center>
<h1>
Bad apple
</h1>
</center>

https://github.com/user-attachments/assets/87912457-ccdc-4533-9ee2-43159dfad575

# Libraries used

- [av](https://cran.r-project.org/web/packages/av/av.pdf "av library Reference manual")
  for extracting frames in the original mp4

- [jpeg](https://cran.r-project.org/web/packages/jpeg/jpeg.pdf "jpeg library Reference manual")
  for reading the pixels in every frame

# Extracting frames

This part of the code just creates jpegs in the folder “Images” on my
work directory, the frames are specifically spaced as 30 fps.

``` r
av_video_images(
  video="Bad-apple.mp4",
  destdir = "Images",
  format = "jpg",
  fps = 30
)
```

# Reading frames

I created a function that inputs the number of the frame, and outputs a
readjpeg of that specific frame.

The images are named like “image_000001” for the first, and
“image_005100” for the 5100th.

So i had to find a way to convert the frame number into a string that
was the directory for each image.

- number0: stores the specific amount of 0’s in any number (for example:
  “00000” when n=1 and “00” when n=5100).

- vec: puts the whole directory in one string.

``` r
imagecapturer <- function(n)
{
  number0 <- as.character(rep(0,5-floor(log10(n))))
  vec <- paste(c("Images\\image_",number0,n,".jpg"),collapse="")
  return(readJPEG(vec))
}
```

# Detecting black pixels

This function receives the “readjpeg” matrix for that frame, a scale
(used to scale the output to be smaller), and a precision for how
lenient you want to define what the color black is.

For example, if there’s a grayish pixel that’s R: 0.79 G: 0.79 B: 0.79,
a precision of 8/10 would consider it black, but a precision of 78/100
wouldn’t.

The output is intended to be a boolean matrix that will tell for “every”
pixel if it’s black (not every pixel, if scale \< 1).

- dimensions: scales the dimensions down, and rounds it.

- booleanswer creates a boolean matrix with the scaled down dimensions,
  this will be a TRUE if the pixel is black, FALSE if it’s white.

Then i go around through every coordinate in my booleanswer and check
for the sums of each pixel’s colors in the original matrix, and if that
sum is within the range i created with precision\*3.

THIS ONLY WORKS BECAUSE THE ORIGINAL VIDEO IS IN BLACK AND WHITE!!!

It’s basically detecting if the coordinates are outside a certain volume
near the color white in the RGB cube color space. But it’s not detecting
any color.

This function also aligns the boolean matrix’s coordinates with the
plot’s coordinates, because JPEG’s are written left to rightand
downwards, while the cartesian plane on the screen is left to right and
upwards. So, after the adjustment, if booleanswer\[25,50\] is true, then
i just need to draw a black dot in the coordinates (25,50).

``` r
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
```

# Lyrics

Before actually getting into plotting there’s an extra (useless) thing i
did, i
“[wrote](https://touhou.fandom.com/wiki/Lyrics:_Bad_Apple!! "copied off of the wiki")”
the lyrics and their timestamps in a CSV file.

- timestamps.csv: is a dataframe with 3 variables
  - time1 the start in seconds of that lyric
  - time2 the end in seconds of that lyric
  - lyric string containing the lyric
- lyricfinder: function that takes the recent frame and framerate,
  transforms into seconds,and check if it’s in any interval in the file,
  then returns the lyrics (or none, if it’s not in any interval)

(Maybe i could have done some implementation of binary search since it’s
sorted? there’s a possible outcome of it not being in the list too, so
idk if it’s possible to make a workaround to that)

``` r
lyrics <- read.csv("timestamps.csv",header=T,sep=",")

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
```

# Plotting

Gets the range of frames in the output, fps, and scaling (the same scale
in the black and white detection), and extra inputs for wbdetect that
will only be used there.

- jpegmatrix: uses in this frame the first function i created.

- result: uses the second function i created.

- result1: gets every pair of indices where “result” is TRUE.

- lyric: gets the string for whatever lyric should be in this frame.

``` r
plotting <- function(frames,fps,scaling,...)
{
  for(i in frames[1]:frames[2])
    {
    jpegmatrix <- imagecapturer(i)
    
    result <- wbdetect(jpegmatrix,scale=scaling,...)
    
    result1 <- which(result,arr.ind = T)
    
    lyric <- lyricfinder(i,fps)
    
    plot(0,type="n",xlim=c(0,480*scaling),ylim=c(0,360*scaling),asp=1,main=lyric,xlab="",ylab="")
    
    points(result1[,2],result1[,1],pch=16,cex=0.0001)
    }
}
```

# Exporting to mp4

These are the final inputs i used.

``` r
av_capture_graphics(
  plotting(frames=c(1,6572),scaling=9/10,precision=8/10,fps=30),
  output = "output.mp4",
  width = 480,
  height = 360,
  framerate = 30)
```
