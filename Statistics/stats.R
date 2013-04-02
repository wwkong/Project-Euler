require(stringr)
require(ggplot2)

# Initialize the hooks and lists
statsFiles <- list.files(pattern=".(.hstats)")
pNums <- c()
times <- c()

# Main loop
for(file in statsFiles) {
  fName <- file
  pNum <- substr(fName,6,9)
  pNums <- c(pNums, pNum) # add to project name list
  conn <- file(fName, 'r')
  contents <- readLines(conn)
  close(conn)
  totalTime <- as.double(str_extract(contents, "([0-9]+)\\.([0-9][0-9])")[16])
  times <- c(times, totalTime) # add to time list
}

# Do some cleaning up
rm(pNum,fName,totalTime,file,conn,contents,statsFiles)

# Generate df and plot
peData <- data.frame(pNums,times)
rm(pNums,times)
p <- ggplot() + geom_bar(aes(y = times,x = pNums),data=peData) +
                xlab(label = 'Project Euler Problem Number') +
                ylab(label = 'Total Run Time (seconds)') +
                ggtitle(label = 'Project Euler Run Time Comparisons') +
                theme(plot.title = element_text(size = 24),
                      axis.text.x = element_text(angle = 90, hjust = 1),
                      axis.text.x = element_text(size = 18),
                      axis.text.y = element_text(size = 18))
print(p)
rm(p)

# Export plot to pdf
ggsave("peComparisons.pdf", width=12, height=8, dpi=100)

# Export data to csv
write.csv(peData,"peData.csv")