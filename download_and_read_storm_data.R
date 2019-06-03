# This part downloads the project data - a large (46.9 Mb) zipped csv file -
# and stores is locally for further processing.
# Unzipped file is about 500 Mb.
# Running this step may take a while, is system dependent.
# Excel should be able to open the file (again, may take time)

if (!file.exists("data"))  dir.create("data")

if (!file.exists("./data/stormData.csv")) {
  temp <- tempfile()
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(fileUrl, temp)
  data <- read.csv(bzfile(temp))
  write.csv(data, file="./data/stormData.csv", row.names=FALSE)
  unlink(temp)
}
# data <- read.csv("./data/stormData.csv")
# head(data)