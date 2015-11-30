# efg, 28 June 2013

###############################################################################
# Helper functions to work with 96-by-96 pixel images with image command

grays <- gray((0:255)/255)

showImage <- function(imageMatrix, PALETTE=grays)
{
  image(1:ncol(imageMatrix),
        1:nrow(imageMatrix),
        imageMatrix, col=PALETTE,
        xlab="", ylab="",
        xaxt="n", yaxt="n")
}

showPoint <- function(x, y, COLOR="red", PCH=19)
{
  points(96-x, 96-y, col=COLOR, pch=PCH, cex=0.5)
}

showLines <- function(x, y, COLOR="red", PCH=19)
{
  lines(96-x, 96-y, col=COLOR)
}

showTrainEyeLine <- function(index)
{
  showLines(c( dTrain[index, "left_eye_center_x"],
                dTrain[index, "left_eye_center_y"]),
             c(dTrain[index, "right_eye_center_x"],
               dTrain[index, "left_eye_center_y"]))
}


testImage <- function(index)
{
  invisible( matrix(data=rev(imTest[index,]), nrow=96, ncol=96) )
}

trainImage <- function(index)
{
  invisible(  matrix(data=rev(imTrain[index,]), nrow=96, ncol=96) )
}

showFeatures <- function(imageSet, index, COLOR="red")
{
  showPoint(imageSet[index, "left_eye_center_x"],         imageSet[index, "left_eye_center_y"],         COLOR=COLOR, PCH=4)
  showPoint(imageSet[index, "left_eye_inner_corner_x"],   imageSet[index, "left_eye_inner_corner_y"],   COLOR=COLOR, PCH=")")
  showPoint(imageSet[index, "left_eye_outer_corner_x"],   imageSet[index, "left_eye_outer_corner_y"],   COLOR=COLOR, PCH="(")

  showPoint(imageSet[index, "right_eye_center_x"],        imageSet[index, "right_eye_center_y"],        COLOR=COLOR, PCH=4)
  showPoint(imageSet[index, "right_eye_inner_corner_x"],  imageSet[index, "right_eye_inner_corner_y"],  COLOR=COLOR, PCH="(")
  showPoint(imageSet[index, "right_eye_outer_corner_x"],  imageSet[index, "right_eye_outer_corner_y"],  COLOR=COLOR, PCH=")")

  showPoint(imageSet[index, "left_eyebrow_inner_end_x"],  imageSet[index, "left_eyebrow_inner_end_y"],  COLOR=COLOR, PCH="]")
  showPoint(imageSet[index, "left_eyebrow_outer_end_x"],  imageSet[index, "left_eyebrow_outer_end_y"],  COLOR=COLOR, PCH="[")

  showPoint(imageSet[index, "right_eyebrow_inner_end_x"], imageSet[index, "right_eyebrow_inner_end_y"], COLOR=COLOR, PCH="[")
  showPoint(imageSet[index, "right_eyebrow_outer_end_x"], imageSet[index, "right_eyebrow_outer_end_y"], COLOR=COLOR, PCH="]")

  showPoint(imageSet[index, "nose_tip_x"],                imageSet[index, "nose_tip_y"],                COLOR=COLOR, PCH=3)

  showPoint(imageSet[index, "mouth_left_corner_x"],       imageSet[index, "mouth_left_corner_y"],       COLOR=COLOR, PCH="[")
  showPoint(imageSet[index, "mouth_right_corner_x"],      imageSet[index, "mouth_right_corner_y"],      COLOR=COLOR, PCH="]")

  showPoint(imageSet[index, "mouth_center_top_lip_x"],    imageSet[index, "mouth_center_top_lip_y"],    COLOR=COLOR, PCH=15)
  showPoint(imageSet[index, "mouth_center_bottom_lip_x"], imageSet[index, "mouth_center_bottom_lip_y"], COLOR=COLOR, PCH=17)
}

###############################################################################

EBITrainMatrix <- function(index)
{
  m <- trainImage(index) / 255
  m <- m[,96:1]
  invisible(m)
}

###############################################################################
### Added 24 Oct 2013

### Usually preceded by something like:
### par(mfrow=c(2,2), mar=c(2,2,2,2))

ShowTrainImages <- function(pick, DistanceLabel="Correlation", Distance=NULL,
                            RMSE=NULL, FEATURE.COLOR="red")
{
  for (i in 1:length(pick))
  {
    TRAIN <- pick[i]
    showImage(trainImage(TRAIN))
    showFeatures(dTrain, TRAIN, COLOR=FEATURE.COLOR)
    title(paste("Train", TRAIN), col.main="blue")
    if (length(Distance) > 0)
    {
      mtext(paste(DistanceLabel, sprintf("%.3f", Distance[i])),
            adj=0, line=-1.5, col="green")
    }
    if (length(RMSE) > 0)
    {
      mtext(paste("RMSE", sprintf("%.3f", RMSE[i])),
            adj=0, line=-2.5, col="red")

    }
  }
}

ShowTestImages <- function(pick)
{
  for (i in 1:length(pick))
  {
    TEST <- pick[i]
    showImage(testImage(TEST))
    title(paste("Test", TEST), col.main="blue")
  }
}

ShowTrainTrain <- function(pickTrain, pickTrainFeatures)
{
  stopifnot(length(pickTrain) == length(pickTrainFeatures))

  for (i in 1:length(pickTrain))
  {
    TRAIN1 <- pickTrain[i]
    showImage(trainImage(TRAIN1))
    showFeatures(dTrain, TRAIN1, COLOR="red")
    title(paste("Train", TRAIN1), col.main="blue")

    TRAIN2 <- pickTrainFeatures[i]
    showFeatures(dTrain, TRAIN2, COLOR="green")
  }
}

ShowTestTrain <- function(Test, Train, DistanceLabel="Correlation", Distance=NULL)
{
  stopifnot (length(Test) == 1)
  showImage(testImage(Test))
  showFeatures(dTrain, Train[1], COLOR="green")  # Show Train features on Test
  title(paste("Test", Test), col.main="blue")

  for (i in 1:length(Train))
  {
    showImage(trainImage(Train[i]))
    showFeatures(dTrain, Train[i],  COLOR=ifelse(i==1, "green", "red"))
    title(paste("Train", Train[i]), col.main="blue")
    if (length(Distance) > 0)
    {
      mtext(paste(DistanceLabel, sprintf("%.3f", Distance[i])),
            adj=0, line=-1.5, col="green")
    }
  }
}


