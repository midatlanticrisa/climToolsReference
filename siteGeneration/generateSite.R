##a script to automatically pull apart input data and present that data in a website

##libraries
library(readxl)
library(pbapply)

##find data file, assumed to be on computer
baseDir <- "/Users/mdl5548/Documents/GitHub/"
dataDir <- paste0(baseDir, "climToolsReference/siteGeneration/")
#siteDir <- paste0(dataDir, "docs/")
siteDir <- paste0(baseDir, "climToolsReference/content/english/")
toolsDir <- paste0(siteDir, "posts/")
source(paste0(dataDir, "siteGeneratingFunctions.R"))

#toolInventoryName <- "Inventory_2020-01-21_v2.xlsx"
toolInventoryName <- "COPY-of-Inventory_Ben"
templatePageName <- "pageTemplate.md"

##line break character
el <- "\n"
tab <- "  "

##read in the tool description document
toolInventoryFile <- list.files(dataDir, toolInventoryName, recursive=T, full.names=T)
#toolInventoryFile <- descFile[grep(paste0("/",toolInventoryFile), descFile)]

##get number of sheets, to load all of them
sheetNames <- excel_sheets(toolInventoryFile)

##read in data
toolInventory <- lapply(sheetNames, read_excel, path=toolInventoryFile, col_names=F)

##inventory, cleaning headers to be useful
inventory <- toolInventory[[1]]
baseNames <- as.character(inventory[2,])
##for now, hard coded
baseNames[15:23] <- paste(baseNames[15], as.character(inventory[4,][15:23]), sep="-")
baseNames[24:27] <- paste(baseNames[24], as.character(inventory[4,][24:27]), sep="-")
colnames(inventory) <- baseNames
inventory <- inventory[which(inventory$`Tool ID`=="1.0"):nrow(inventory),]
invValsOnly <- inventory[which(is.na(inventory$`Group ID`)==F),]
invValsOnly <- invValsOnly[,1:28]

##read in the template files to be editted
findTempFile <- list.files(dataDir, templatePageName, recursive=T, full.names=T)
templatePage <- paste(readLines(findTempFile), collapse="\n")

##split by group id, seems to be coorolated with developers
toolIDs <- unique(invValsOnly$`Tool ID`)
toolIDs <- toolIDs[is.na(toolIDs)==F]
splitByToolID <- lapply(toolIDs, function(x){invValsOnly[which(invValsOnly$`Tool ID`==x & is.na(invValsOnly$`Tool ID`)==F),]})


##########################################################################

##create the various subdirectories for the site
if(dir.exists(toolsDir)==F){
  dir.create(toolsDir, recursive=T)
}

pblapply(splitByToolID, generateToolPage, tempPage=templatePage, el=el, siteDir=toolsDir, updateContent=T)

##need to set up the about file
##first, remove thumnail, categories, and tags
aboutPage <- paste0(sapply(strsplit(templatePage, "\ncategories"), "[[", 1), "\nurl: /about/\n---")
aboutPage <- gsub("page-template", "About Climate Tool Lookup", aboutPage)
aboutPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), aboutPage)
aboutPage <- gsub("post", "page", aboutPage)

##fill the about file
abSection1Title <- "### Climate Tools"
ab1Text <- paste0("The changing climate of our world is one of the largest concerns of any generation. Due to the scope of climate change, and its affects on human societies, it would be almost impossible to collect, analyze, and communicate the results of an analysis without some helpful tools. However, not every tool is the same, and selecting the correct tool is a critical decision.", el)

abSection2Title  <- "### A Tool for Tools"
ab2Text <- paste0("This site is a service for you to be able to view a summary of the available climate tools, and compare the various tools in one similar format. Our goal is to be the advisor on which climate tools would best suit the goals of your project.", el)

abSection3Title  <- "### EESI at Penn State, and the people involved"

aboutPage <- paste(aboutPage, abSection1Title, ab1Text, abSection2Title, ab2Text, abSection3Title, sep=el)
writeFile <- paste0(siteDir, "/about.md")
writeLines(aboutPage, file(writeFile))


