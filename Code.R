#make sure you have these packages installed 
#Install from source 

install.packages("R.cache_0.16.0.tar.gz", type = "source")
install.packages("styler_1.9.1.tar.gz", type = "source")
install.packages("labelled_2.11.0.tar.gz", type = "source")
install.packages("questionr_0.7.8.tar.gz", type = "source")
install.packages("klaR_1.7-2.tar.gz", type = "source")
install.packages("AlgDesign_1.2.1.tar.gz", type = "source")
install.packages("agricolae_1.3-5.tar.gz", type = "source")


#Set up disease severity percent measurements; 
#change these in subsequent analyses to see how it affects
#the AUDPC
ds0<-1
ds1<-2
ds2<-7
ds3<-7.5

#Put these values into a vector without making any changes
disease.severity<-c(ds0,ds1,ds2,ds3)

#Time points at which disease severity
#   measurements are made, 
#change these in subsequent analyses to
#see how it affects the AUDPC Value
t0<-0
t1<-2
t2<-5
t3<-6

#Put time period into a vector
## Do not change these values
time.period<-c(t0,t1,t2,t3)

#Refresh your memory about how the plot function works
help(plot)

#Create the plot of disease severity over time
plot(time.period,
  disease.severity,
  ylim=c(0,(ds3+1)),
  xlim=c(0,(t3+0.5)),
  xlab="Time",
  ylab="Disease Severity (%)",
  type="o",
  pch=19,
  col="mediumblue")

#Add a title and subtitle to our plot
title(main="Illustration of AUDPC Calculation",
      sub="Figure 1")

#Add text to x labels defining time periods
#   defined in text
mtext("=t0",1,at=0.3,1)
mtext("=t1",1,at=2.3,1)
mtext("=t2",1,at=5.3,1)
mtext("=t3",1,at=6.3,1)

#Illustrate the area under disease progress
#   curve with rectangles.
## Do not change these values
rect(t0,0,t1,((ds0+ds1)/2),border="orange")
# Add text to rectangle to describe rectangle
text(1,1,"A1")
#Add segment to Y axis
#And so-on
rect(t1,0,t2,((ds1+ds2)/2),border="orange")
text(((t1+t2)/2),(((ds1+ds2)/2)/2),"S2")
#Draw line to axis and label with value
segments(.4,((ds1+ds2)/2),t2,((ds1+ds2)/2),
         col="black",lty="18")
text(0,((ds1+ds2)/2),((ds1+ds2)/2))
rect(t2,0,t3,((ds2+ds3)/2),border="orange")
text(((t2+t3)/2),(((ds2+ds3)/2)/2),"S3")
segments(0.4,((ds2+ds3)/2),t2,((ds2+ds3)/2),
         col="black",lty="18")
text(0,((ds2+ds3)/2),((ds2+ds3)/2))

#Build a function for AUDPC calculation
#the left curly bracket indicates the beginning
#   of the function
audpc <- function(disease.severity,time.period){

  #n is the length of time.period, or
  #  the total number of sample dates
  n <- length(time.period)

  #meanvec is the vector (matrix with one dimension)
  #that will contain the mean percent infection
  #it is initialized containing -1 for all entries
  #this sort of initialization is sometimes useful
  #  for debugging
  meanvec <- matrix(-1,(n-1))

  #intvec is the vector that will contain the length of
  #   time between sampling dates
  intvec <- matrix(-1,(n-1))

  #the loop goes from the first to the penultimate entry
  #the left curly bracket indicates the beginning of
  #   commands in the loop
  for(i in 1:(n-1)){

    #the ith entry in meanvec is replaced with the
    #   mean percent infection
    #between sample time i and sample time i+1
    meanvec[i] <- mean(c(disease.severity[i],
                         disease.severity[i+1]))

    #the ith entry in intvec is replaced with the length
    # of the time interval between time i and time i+1
    intvec[i] <- time.period[i+1] - time.period[i]

    #the right curly bracket ends the loop
  }

  #the two vectors are multiplied together
  #  one entry at a time
  infprod <- meanvec * intvec

  #the sum of the entries in the resulting vector
  #   gives the AUDPC
  sum(infprod)

  #the right curly bracket ends the function
}


#Now apply the function to the example data and put
# the result in a new object called 'AUDPCexample'
audpc(disease.severity,time.period) -> AUDPCexample
#Display AUDPC Value
#Draw rectangle around value
rect(0.1,(ds3+.3),2,(ds3+1),border="black")
#AUDPC Text
text(1.05,(ds3+0.8),"AUDPC")
text(1.05,(ds3+0.5),AUDPCexample)

#https://www.apsnet.org/edcenter/disimpactmngmnt/topc/EcologyAndEpidemiologyInR/DiseaseProgress/Pages/AUDPC.aspx


