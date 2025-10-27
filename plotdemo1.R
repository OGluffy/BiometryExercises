library(sciplot) #load sciplot
names(mtcars) #check out the mtcars data

#How to add letters with text locator in base R
  plot1<-bargraph.CI(x.factor=gear,response=mpg,
                     data=mtcars,ylab="Miles per gallon (mpg) ± SE",
                     xlab="Number of gears",ylim=c(0,30)) #see ?bargraph.CI
  text(locator(3),labels=c("A","B","AB"))  #check out ?text for more options
  dev.copy(png,file='cardata1.png', width = 5, height = 6, units = 'in', res = 800) #if this fails, then lower the resolution (res) 
  dev.off()

#How to add letters with text coordinates in base R
  plot2<-bargraph.CI(x.factor=gear,response=mpg,data=mtcars,
                     ylab="Miles per gallon (mpg) ± SE",
                     xlab="Number of gears",ylim=c(0,30))
  text(x=plot2$xvals,y=(plot2$vals+4),c("A","B","AB")) 
  #x coords are from sciplot object, y coordinates are equal to the height of the bar +4, but you can also do this...
  plot2<-bargraph.CI(x.factor=gear,response=mpg,data=mtcars,
                     ylab="Miles per gallon (mpg) ± SE",
                     xlab="Number of gears",ylim=c(0,30))
  text(x=plot2$xvals,y=c(20,28,26),c("A","B","AB")) #... and just put in three manual coordinates like I did here
  dev.copy(png,file='cardata2.png', width = 5, height = 6, units = 'in', res = 800)
  dev.off()

#How to add lines and asterisk, for contrasts
  plot3<-bargraph.CI(x.factor=gear,response=mpg,data=mtcars,
                     ylab="Miles per gallon (mpg) ± SE",
                     xlab="Number of gears",ylim=c(0,30))
  x<-plot3$xvals #pull the x coordinates of the bars from the scipolot object
  y <- 27 # create the y coordinate of the line
  offset <- 0.2 # set an offset for tick lengths
  lines(x[1:2],c(y, y)) # draw first horizontal line between bars 1 and 2
  lines(x[c(1,1)],c(y, y-offset))## draw tick over bar 1
  lines(x[c(2,2)],c(y, y-offset))# draw tick over bar 2
  text(x[1]+((x[2]-x[1])/2),y+0.4,"***")# draw asterisks in between the two bars
  
  #Want to add another line?
    y2 <-28 #set a new y for the line so they don't overlap
    lines(x[2:3],c(y2, y2)) # draw second horizontal line between bars 2 and 3
    lines(x[c(2,2)],c(y2, y2-offset))# draw tick over bar 2
    lines(x[c(3,3)],c(y2, y2-offset))# draw tick over bar 3 
    text(x[2]+((x[3]-x[2])/2),y2+0.4,"*")# draw asterisks in between the two bars
    
  #Still want more?
    y3 <-29 #set a new y for the line so they don't overlap
    lines(x[1:3],c(y3,y3,y3)) # draw third horizontal line between bars 1 and 3 (you need 3 y coords here)
    lines(x[c(1,1)],c(y3, y3-offset))# draw tick over bar 2
    lines(x[c(3,3)],c(y3, y3-offset))# draw tick over bar 3 
    text(x[1]+((x[3]-x[1])/2),y3+0.4,"**")# draw asterisks in between the two bars
    dev.copy(png,file='cardata3.png', width = 5, height = 6, units = 'in', res = 1200)
    dev.off()

#Expressions for text customizations in R
      example<- barplot(c(1,4,3,5)) #make generic barplot
      labels <- c(expression("Control", bold(BOLD), italic("L. cyanellus"),hello^hello^hello)) #Make species name in italics
      axis(side=1,at=example[1:4],labels=labels) #Add an axis with species labels
      # Note: to do this successfully in most situations, you will need to figure out how to
      # supress the default X-axis labels, or else these will be written over those
      # for example, if you make a ggplot graph, you can eliminate x-axis labels with something like: labs(x="", y="Mean +/- SE")
      # for more in ggplot, see also: https://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
      # supressing axis and tick labels works a bit differently in different graphing packages, so you will need to investigate
      
    #Change bar colors - Colors in R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
      example<- barplot(c(1,4,3,5),col = c("violetred4","slategray4","yellowgreen","snow")) #make generic barplot
      axis(side=1,at=example[1:4],labels=labels) #Add an axis with species labels
      
    #Mathematical annotation in R: http://astrostatistics.psu.edu/su07/R/library/grDevices/html/plotmath.html
      text(locator(2), labels=c(expression(y^2 + x[i]),expression(y^2+x != y + (x >= y) ) ) ) # click twice on the graph to add this text
    
    #Save a high-resolution copy in the working directory:
      dev.copy(png,file='colorexample.png', width = 5, height = 6, units = 'in', res = 1200)
      dev.off() # see working directory for the result of this one
      