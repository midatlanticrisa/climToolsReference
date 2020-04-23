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
baseNames[15:23] <- paste(baseNames[15], as.character(inventory[3,][15:23]), sep="-")
baseNames[24:27] <- paste(baseNames[24], as.character(inventory[3,][24:27]), sep="-")
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

##need to set up the index and about files
indexPage <- gsub("page-template", "Climate Tools are Coolz", templatePage)
indexPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), indexPage)
isect1Title <- "### Climate Tools and what they do"
ip1 <- paste0("Blurb about what climate tools are and things they do.", el)
isect2Title <- "### Why Climate Tools are important"
ip2 <- paste0("Blurb about why this list is useful and important", el)
indexPage <- paste(indexPage, isect1Title, ip1, isect2Title, ip2, sep=el)
writeFile <- paste0(siteDir, "/_index.md")
writeLines(indexPage, file(writeFile))


