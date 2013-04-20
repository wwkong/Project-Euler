require(stringr)
require(ggplot2)
require(reshape2)

# Define the upper bound on our total time (in seconds)
upperBound = 60

# Initialize the hooks and lists
statsFiles <- list.files(pattern=".(.hstats)")
pNum <- c()
INIT <- c()
MUT <- c()
GC <- c()
EXIT <- c()
TOTAL <- c()

# Main loop
for(file in statsFiles) {
  fName <- file
  pNumStr <- substr(fName,6,9)
  pNum <- c(pNum, pNumStr) # add to project name list
  conn <- file(fName, 'r')
  contents <- readLines(conn)
  close(conn)
  # Find and add the appropriate times
  vINIT <- as.double(str_extract(grep("INIT.*time.*([0-9]+)\\.([0-9][0-9])s",contents, value=TRUE),
                                 "([0-9]+)\\.([0-9][0-9])"))
  vMUT <- as.double(str_extract(grep("MUT.*time.*([0-9]+)\\.([0-9][0-9])s",contents, value=TRUE),
                                "([0-9]+)\\.([0-9][0-9])"))
  vGC <- as.double(str_extract(grep("GC.*time.*([0-9]+)\\.([0-9][0-9])s",contents, value=TRUE),
                               "([0-9]+)\\.([0-9][0-9])"))
  vEXIT <- as.double(str_extract(grep("EXIT.*time.*([0-9]+)\\.([0-9][0-9])s",contents, value=TRUE),
                                 "([0-9]+)\\.([0-9][0-9])"))
  vTOTAL <- as.double(str_extract(grep("Total.*time.*([0-9]+)\\.([0-9][0-9])s",contents, value=TRUE),
                                  "([0-9]+)\\.([0-9][0-9])"))
  INIT <- c(INIT, vINIT) 
  MUT <- c(MUT, vMUT) 
  GC <- c(GC, vGC) 
  EXIT <- c(EXIT, vEXIT) 
  TOTAL <- c(TOTAL, vTOTAL) 
  rm(pNumStr,vINIT,vMUT,vGC,vEXIT,vTOTAL)
}

# Do some cleaning up
rm(file,conn,contents,statsFiles,fName)

# Generate our df and apply our upper bound
pNum = as.factor(pNum)
peData <- data.frame(pNum,INIT,MUT,GC,EXIT,TOTAL)
peData <- subset(peData,TOTAL < upperBound)
peData <- peData[,1:5]
rm(pNum,INIT,MUT,GC,EXIT,TOTAL,upperBound)

# Melt our data
peDataMelted = melt(peData, id.vars=c("pNum"), measure.vars=c("INIT","MUT","GC","EXIT"),
                    variable.name="timeType", value.name="time")


# Plot
p <-  ggplot(peDataMelted, aes(x=pNum, y=time)) + 
  geom_bar(aes(fill=timeType),data=peDataMelted, stat='identity') +
  xlab(label = 'Project Euler Problem Number') +
  ylab(label = 'Total Run Time (seconds)') +
  ggtitle(label = 'Project Euler Run Time Comparisons') +
  theme(plot.title = element_text(size = 24),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)) +
  scale_fill_discrete(name = "Component")
print(p)
rm(p)

# Avoid an RStudio bug
Sys.sleep(1)

# Export plot to pdf
ggsave(paste("peComparisons",Sys.Date(),".pdf") , width=12, height=8, dpi=100)

# Patch the data with today's date
peData$Date = Sys.Date()

# Export data to csv
write.table(peData,"peData.csv",sep=",",row.names=FALSE,append=TRUE)