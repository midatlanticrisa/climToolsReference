##a script to automatically pull apart input data and present that data in a website

##libraries
library(readxl)
library(pbapply)
library(stringi)
library(webshot)
##webshot::install_phantomjs()

##find data file, assumed to be on computer
baseDir <- "/Users/mdl5548/Documents/GitHub/"
dataDir <- paste0(baseDir, "climToolsReference/siteGeneration/")
#siteDir <- paste0(dataDir, "docs/")
siteDir <- paste0(baseDir, "climToolsReference/content/")
toolsDir <- paste0(siteDir, "tools/")
discovDir <- paste0(baseDir, "climToolsReference/public/data/")
imageDir <- paste0(baseDir, "climToolsReference/public/images/")
toolSearchDir <- paste0(baseDir, "climToolsReference/themes/soho/layouts/page/")
source(paste0(dataDir, "siteGeneratingFunctions.R"))

#toolInventoryName <- "Inventory_2020-01-21_v2.xlsx"
toolInventoryName <- "Tool Inventory - 5.1.20.xlsx"
templatePageName <- "pageTemplate.md"
templateIndex <- "templateIndex.md"
searchCodePage <- "searchTemplate.html"

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
catNames <- as.character(inventory[1,])
baseNames <- as.character(inventory[2,])
##for now, hard coded
baseNames[6:10] <- paste(catNames[6], baseNames[6:10], sep="-")
baseNames[11:14] <- paste(catNames[11], baseNames[11:14], sep="-")
baseNames[15:23] <- paste(catNames[15], baseNames[15], as.character(inventory[3,][15:23]), sep="-")
baseNames[24:27] <- paste(catNames[15], baseNames[24], as.character(inventory[3,][24:27]), sep="-")
baseNames[28] <- paste(catNames[15], baseNames[28], sep="-")
baseNames[29:33] <- paste(catNames[29], baseNames[29:33], sep="-")
baseNames[34:35] <- paste(catNames[34], baseNames[34:35], sep="-")
baseNames[36:38] <- paste(catNames[36], baseNames[36:38], sep="-")
baseNames[44] <- paste(catNames[36], baseNames[44], sep="-")
baseNames[45:49] <- paste(catNames[45], baseNames[45:49], sep="-")
baseNames[50:52] <- paste(catNames[50], baseNames[50:52], sep="-")
baseNames[53:59] <- paste(catNames[53], baseNames[53], as.character(inventory[3,][53:59]), sep="-")
baseNames[60:65] <- paste(catNames[53], baseNames[60], as.character(inventory[3,][60:65]), sep="-")
baseNames[66:72] <- paste(catNames[53], baseNames[66], as.character(inventory[3,][66:72]), sep="-")
baseNames[73:75] <- paste(catNames[53], baseNames[73], as.character(inventory[3,][73:75]), sep="-")
baseNames[76:79] <- paste(catNames[53], baseNames[76], as.character(inventory[3,][76:79]), sep="-")
baseNames[80:83] <- paste(catNames[53], baseNames[80], as.character(inventory[3,][80:83]), sep="-")
baseNames[84:88] <- paste(catNames[53], baseNames[84], as.character(inventory[3,][84:88]), sep="-")
baseNames[89:91] <- paste(catNames[53], baseNames[89], as.character(inventory[3,][89:91]), sep="-")
baseNames[92:93] <- paste(catNames[92], baseNames[92:93], sep="-")
baseNames[94:98] <- paste(catNames[92], baseNames[94:98], sep="-")

colnames(inventory) <- baseNames
inventory <- inventory[which(inventory$`Tool ID`=="1"):nrow(inventory),]
invValsOnly <- inventory[which(is.na(inventory$`Group ID`)==F),]

##needed with tool id with new file, may not be needed in future versions
invValsOnly$`Tool ID` <- as.character(format(round(as.numeric(invValsOnly$`Tool ID`),1)),nsmall=1)

invValsOnly <- invValsOnly[,c(1:38, 44:98)]

##read in the template files to be editted
findTempFile <- list.files(dataDir, templatePageName, recursive=T, full.names=T)
templatePage <- paste(readLines(findTempFile), collapse="\n")
findIndFile <- list.files(dataDir, templateIndex, recursive=T, full.names=T)
templateIndexPg <- paste(readLines(findIndFile), collapse="\n")
findSearchTempFile <- list.files(dataDir, searchCodePage, recursive=T, full.names=T)
templateSearchTemp <- paste(readLines(findSearchTempFile), collapse="\n")

##split by group id, seems to be coorolated with developers
toolIDs <- unique(invValsOnly$`Tool ID`)
toolIDs <- toolIDs[is.na(toolIDs)==F]
splitByToolID <- lapply(toolIDs, function(x){invValsOnly[which(invValsOnly$`Tool ID`==x & is.na(invValsOnly$`Tool ID`)==F),]})


##########################################################################

##create the various subdirectories for the site
if(dir.exists(toolsDir)==F){
  dir.create(toolsDir, recursive=T)
}
if(dir.exists(discovDir)==F){
  dir.create(discovDir, recursive=T)
}
if(dir.exists(imageDir)==F){
  dir.create(imageDir, recursive=T)
}

searchTags <- data.frame(toolName=NA, toolLink=NA, tag=NA)
searchSoftReqs <- data.frame(toolName=NA, toolLink=NA, softReqs=NA)

pblapply(splitByToolID, generateToolPage, tempPage=templatePage, el=el, siteDir=toolsDir, updateContent=T)
searchTags <- searchTags[-1,]
searchSoftReqs <- searchSoftReqs[-1,]

##set up tool index file
toolIndex <- gsub("TemplateIndex", "Tools", templateIndexPg)
toolIndex <- paste0(toolIndex, el, el, "A collection of articles, presentations or talks, most likely on stuff and stuff.")
writeFile <- paste0(toolsDir, "_index.md")
writeLines(toolIndex, file(writeFile))



##site index file
siteIndex <- gsub("TemplateIndex", "Home", templateIndexPg)
siteIndex <- gsub("\nlink: NA\nimage: NA\ndescription: NA\nweight: 10", "", siteIndex)
siteIndex <- gsub("\n  weight: 0.5", "", siteIndex)
siteIndex <- gsub("0.6", "1.0\n\noutputs:\n- html\n- rss\n- json", siteIndex)

##fill the about file
abSection1Title <- "### Climate Tools"
ab1Text <- paste0("The changing climate of our world is one of the largest concerns of any generation. Due to the scope of climate change, and its affects on human societies, it would be almost impossible to collect, analyze, and communicate the results of an analysis without some helpful tools. However, not every tool is the same, and selecting the correct tool is a critical decision.", el)

abSection2Title  <- "### A Tool for Tools"
ab2Text <- paste0("This site is a service for you to be able to view a summary of the available climate tools, and compare the various tools in one similar format. Our goal is to be the advisor on which climate tools would best suit the goals of your project.", el)

abSection3Title  <- "### EESI at Penn State, and the people involved"
ab3Text <- paste0("This is who we are. We do cool stuff!")


siteIndex <- paste(siteIndex, abSection1Title, ab1Text, abSection2Title, ab2Text, abSection3Title, ab3Text, sep=el)
writeFile <- paste0(siteDir, "_index.md")
#writeFile <- paste0(homeDir, "_index.md")
writeLines(siteIndex, file(writeFile))


##create the tool search object jscript for tags
createRecs <- paste('{"tag":"', searchTags$tag, '","name":"', searchTags$toolName, '","link":"', searchTags$toolLink, '"}', sep="", collapse=",\n")
tagData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchTags.json")
writeLines(tagData, file(writeFile))
##create the tool search object jscript for software requirements
createRecs <- paste('{"softReq":"', searchSoftReqs$softReqs, '","name":"', searchSoftReqs$toolName, '","link":"', searchSoftReqs$toolLink, '"}', sep="", collapse=",\n")
softReqsData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchSoft.json")
writeLines(softReqsData, file(writeFile))


##fill out the tool search page code
uniqueTags <- unique(searchTags$tag)
createBoxes <- paste('<label><input type="checkbox" name="tags" value="', uniqueTags, '">', uniqueTags, '</label>', sep="", collapse="<br>\n")
templateSearchTemp <- gsub("<inputTags>", createBoxes, templateSearchTemp)
writeFile <- paste0(toolSearchDir, "toolsearch.html")
writeLines(templateSearchTemp, file(writeFile))








##need to set up the about file
##first, remove thumnail, categories, and tags
# aboutPage <- paste0(sapply(strsplit(templatePage, "\ncategories"), "[[", 1), "\nurl: /about/\n---")
# aboutPage <- gsub("page-template", "About Climate Tool Lookup", aboutPage)
# aboutPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), aboutPage)
# aboutPage <- gsub("post", "page", aboutPage)
# 
# ##fill the about file
# abSection1Title <- "### Climate Tools"
# ab1Text <- paste0("The changing climate of our world is one of the largest concerns of any generation. Due to the scope of climate change, and its affects on human societies, it would be almost impossible to collect, analyze, and communicate the results of an analysis without some helpful tools. However, not every tool is the same, and selecting the correct tool is a critical decision.", el)
# 
# abSection2Title  <- "### A Tool for Tools"
# ab2Text <- paste0("This site is a service for you to be able to view a summary of the available climate tools, and compare the various tools in one similar format. Our goal is to be the advisor on which climate tools would best suit the goals of your project.", el)
# 
# abSection3Title  <- "### EESI at Penn State, and the people involved"
# 
# aboutPage <- paste(aboutPage, abSection1Title, ab1Text, abSection2Title, ab2Text, abSection3Title, sep=el)
# writeFile <- paste0(siteDir, "/about.md")
# writeLines(aboutPage, file(writeFile))


