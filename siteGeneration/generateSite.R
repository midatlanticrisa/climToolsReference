##a script to automatically pull apart input data and present that data in a website

##libraries
library(readxl)
library(pbapply)
library(stringi)
# install.packages("webshot")
library(webshot)
##webshot::install_phantomjs()

#/Volumes/GoogleDrive/Shared drives/MARISA/Coastal Climate Extension Pilot/Tool Inventory/Tool Screenshots_resized_156_250/TOOLID_1.0_ScreenCapture-1.png
##find data file, assumed to be on computer
# baseDir <- "/Users/mdl5548/Documents/GitHub/"
# dataDir <- paste0(baseDir, "climToolsReference/siteGeneration/")
# #siteDir <- paste0(dataDir, "docs/")
# siteDir <- paste0(baseDir, "climToolsReference/content/")
# toolsDir <- paste0(siteDir, "tools/")
# discovDir <- paste0(baseDir, "climToolsReference/public/data/")
# imageDir <- paste0(baseDir, "climToolsReference/public/images/")
# toolSearchDir <- paste0(baseDir, "climToolsReference/themes/soho/layouts/page/")

baseDir <- "~/Documents/Github/"
dataDir <- paste0(baseDir, "climToolsReference/siteGeneration/")
source(paste0(dataDir, "siteGeneratingFunctions.R"))

#toolInventoryName <- "Inventory_2020-01-21_v2.xlsx"
toolInventoryName <- "Tool Inventory - 10.23.20.xlsx"
templatePageName <- "pageTemplate.md"
templateIndex <- "templateIndex.md"
searchCodePage <- "searchTemplate.html"
stcntyFile <- paste0(dataDir, "MARISAstateFIPS.csv")

##line break character
el <- "\n"
tab <- "  "

##read in the tool description document
toolInventoryFile <- list.files(dataDir, toolInventoryName, recursive=T, full.names=T)
toolInventoryFile <- toolInventoryFile[grep("~",toolInventoryFile, invert=TRUE)]
#toolInventoryFile <- descFile[grep(paste0("/",toolInventoryFile), descFile)]

##get number of sheets, to load all of them
sheetNames <- excel_sheets(toolInventoryFile)

##read in data
# Each list represents a seperate excel sheet
toolInventory <- lapply(sheetNames, read_excel, path=toolInventoryFile, col_names=F)
stcntyTab <- read.csv(stcntyFile) # Read the fips csv file

##inventory, cleaning headers to be useful
inventory <- toolInventory[[1]]
catNames <- as.character(inventory[1,]) # Top line header for sheet 1: Inventory
baseNames <- as.character(inventory[2,]) # Secondary header for sheet 1: Inventory

# Combine the top line and secondary headers to set as the column names
##for now, hard coded
baseNames[6:10] <- paste(catNames[6], baseNames[6:10], sep="-") # Description
baseNames[11:15] <- paste(catNames[11], baseNames[11:15], sep="-") # Geographic Scope
baseNames[16:24] <- paste(catNames[16], baseNames[16], as.character(inventory[3,][16:24]), sep="-") # Purpose - Tool Function
baseNames[25:28] <- paste(catNames[16], baseNames[25], as.character(inventory[3,][25:28]), sep="-") # Purpose - Future Projections
baseNames[29] <- paste(catNames[16], baseNames[29], sep="-") # Purpose - Description
baseNames[30:34] <- paste(catNames[30], baseNames[30:34], sep="-") # Tool type
baseNames[35:36] <- paste(catNames[35], baseNames[35:36], sep="-") # Tool Inputs
baseNames[37:39] <- paste(catNames[37], baseNames[37:39], sep="-") # Tool Outputs
baseNames[45] <- paste(catNames[37], baseNames[45], sep="-")# Tool Outputs
baseNames[46:50] <- paste(catNames[46], baseNames[46:50], sep="-") # Software Requirements
baseNames[51:53] <- paste(catNames[51], baseNames[51:53], sep="-") # Effort / Skill
baseNames[54:58] <- paste(catNames[54], baseNames[54], as.character(inventory[3,][54:58]), sep="-") # Topic Filters - Main Topics
baseNames[59:64] <- paste(catNames[54], baseNames[59], as.character(inventory[3,][59:64]), sep="-") # Topic Filters - Climate
baseNames[65:70] <- paste(catNames[54], baseNames[65], as.character(inventory[3,][65:70]), sep="-") # Topic Filters - Ecosystems
baseNames[71:78] <- paste(catNames[54], baseNames[71], as.character(inventory[3,][71:78]), sep="-") # Topic Filters - Agriculture / Built Environment
baseNames[79:82] <- paste(catNames[54], baseNames[79], as.character(inventory[3,][79:82]), sep="-") # Topic Filters - Society
baseNames[83:90] <- paste(catNames[54], baseNames[83], as.character(inventory[3,][83:90]), sep="-") # Topic Filters - Water / Flooding
baseNames[93:94] <- paste(catNames[93], baseNames[93:94], sep="-") # Contact Information
baseNames[95:99] <- paste(catNames[93], baseNames[95:99], sep="-") # Contact Information

colnames(inventory) <- baseNames
inventory <- inventory[which(inventory$`Tool ID`=="1"):nrow(inventory),] # Extract only the tools
invValsOnly <- inventory[which(is.na(inventory$`Group ID`)==F),] # Extract the only the ones with Group IDs - should be all the tools

##needed with tool id with new file, may not be needed in future versions
# Format the Tool ID to have just 1 decimal point and add a space padding if its a single digit number
invValsOnly$`Tool ID` <- as.character(format(round(as.numeric(invValsOnly$`Tool ID`),1)),nsmall=1)

# # hhh <- invValsOnly[,c(1:39, 45:99)]
# Remove columns that are not useful including:
# "dat", "fig", "gis", "map", "rprt" 
invValsOnly <- invValsOnly[,c(1:39, 45:99)] 

##read in the template files to be editted
findTempFile <- list.files(dataDir, templatePageName, recursive=T, full.names=T)
templatePage <- paste(readLines(findTempFile), collapse="\n")
findIndFile <- list.files(dataDir, templateIndex, recursive=T, full.names=T)
templateIndexPg <- paste(readLines(findIndFile), collapse="\n")
findSearchTempFile <- list.files(dataDir, searchCodePage, recursive=T, full.names=T)
templateSearchTemp <- paste(readLines(findSearchTempFile), collapse="\n")

##split by group id, seems to be coorolated with developers
toolIDs <- unique(invValsOnly$`Tool ID`) # Make sure all values are different
toolIDs <- toolIDs[is.na(toolIDs)==F] # Make sure all values are not NA
# Convert the data.frame to a list where each item is a different tool
splitByToolID <- lapply(toolIDs, function(x){invValsOnly[which(invValsOnly$`Tool ID`==x & is.na(invValsOnly$`Tool ID`)==F),]})

##########################################################################
# Set up search engine??
##########################################################################

##create the various subdirectories for the site
if(dir.exists(toolsDir)==F){ # public/tools/ directory
  dir.create(toolsDir, recursive=T)
}
if(dir.exists(discovDir)==F){ # public/data/ directory
  dir.create(discovDir, recursive=T)
}
if(dir.exists(imageDir)==F){ # public/images/ directory
  dir.create(imageDir, recursive=T)
}

# Create a series of empty data.frames with specific column names
searchTags <- data.frame(toolName=NA, toolLink=NA, tag=NA)
searchSoftReqs <- data.frame(toolName=NA, toolLink=NA, softReqs=NA)
searchGeoScope <- data.frame(toolName=NA, toolLink=NA, toolState=NA, toolLoc=NA, coastal=NA)
searchToolFunct <- data.frame(toolName=NA, toolLink=NA, toolFunct=NA)
searchTopFilters <- data.frame(toolName=NA, toolLink=NA, topFilter=NA, topFilterNam=NA,  subFilter=NA)
#searchSubFilters <- data.frame(toolName=NA, toolLink=NA, subFilter=NA)

#ttt <- data.frame(table(invValsOnly$`Geographic Scope-Scope`, useNA="ifany"))
#hhh <- invValsOnly[invValsOnly$`Geographic Scope-Scope`=="National",]

# Run the generateToolPage function on each tool. #### IT APPEARS THE ABOVE DATA.FRAMES ARE USED GLOBALLY!!
pblapply(splitByToolID, generateToolPage, tempPage=templatePage, el=el, siteDir=toolsDir, scTab=stcntyTab, updateContent=T)
# Remove the first row from the data.frames???
searchTags <- searchTags[-1,]
searchSoftReqs <- searchSoftReqs[-1,]
searchGeoScope <- searchGeoScope[-1,]
searchToolFunct <- searchToolFunct[-1,]
searchTopFilters <- searchTopFilters[-1,]
#searchSubFilters <- searchSubFilters[-1,]

# Set non coastal areas to 0 and coastal areas to 1. Set to numeric values
searchGeoScope$coastal[is.na(searchGeoScope$coastal)==T] <- 0
searchGeoScope$coastal[searchGeoScope$coastal=="x"] <- 1
searchGeoScope$coastal <- as.numeric(searchGeoScope$coastal)

# Generate a markdown files, an index file, to create the tools page:
# its in content/tools
# el is end line "/n"
##set up tool index file
toolIndex <- gsub("TemplateIndex", "Tools", templateIndexPg)
toolIndex <- paste0(toolIndex, el, el, "A collection of articles, presentations or talks, most likely on stuff and stuff.")
writeFile <- paste0(toolsDir, "_index.md")
writeLines(toolIndex, file(writeFile))

# Generate other pages, which have since be removed, i.e., commented out
##site index file
#siteIndex <- gsub("TemplateIndex", "Home", templateIndexPg)
#siteIndex <- gsub("\nlink: NA\nimage: NA\ndescription: NA\nweight: 10", "", siteIndex)
#siteIndex <- gsub("\n  weight: 0.5", "", siteIndex)
#siteIndex <- gsub("0.6", "1.0\n\noutputs:\n- html\n- rss\n- json", siteIndex)

##fill the about file
#abSection1Title <- "### Climate Tools"
#ab1Text <- paste0("The changing climate of our world is one of the largest concerns of any generation. Due to the scope of climate change, and its affects on human societies, it would be almost impossible to collect, analyze, and communicate the results of an analysis without some helpful tools. However, not every tool is the same, and selecting the correct tool is a critical decision.", el)

#abSection2Title  <- "### A Tool for Tools"
#ab2Text <- paste0("This site is a service for you to be able to view a summary of the available climate tools, and compare the various tools in one similar format. Our goal is to be the advisor on which climate tools would best suit the goals of your project.", el)

#abSection3Title  <- "### EESI at Penn State, and the people involved"
#ab3Text <- paste0("This is who we are. We do cool stuff!")

#siteIndex <- paste(siteIndex, abSection1Title, ab1Text, abSection2Title, ab2Text, abSection3Title, ab3Text, sep=el)
#writeFile <- paste0(siteDir, "_index.md")
##writeFile <- paste0(homeDir, "_index.md")
#writeLines(siteIndex, file(writeFile))

##create the tool search object jscript for tags
#createRecs <- paste('{"tag":"', searchTags$tag, '","name":"', searchTags$toolName, '","link":"', searchTags$toolLink, '"}', sep="", collapse=",\n")
#tagData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
#writeFile <- paste0(discovDir, "searchTags.json")
#writeLines(tagData, file(writeFile))


# Generate Search Engine jscripts -----------------------------------------
##########################################################################
# Jscript Software Requirements
##########################################################################
##create the tool search object jscript for software requirements
createRecs <- paste('{"softReq":"', searchSoftReqs$softReqs, '","name":"', 
                    searchSoftReqs$toolName, '","link":"', searchSoftReqs$toolLink, 
                    '"}', sep="", collapse=",\n")
softReqsData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchSoft.json")
writeLines(softReqsData, file(writeFile))
##########################################################################
# Jscript geographic scope
##########################################################################
##create the tool search object jscript for geographic scope
##state
createRecs <- paste('{"geoScopeST":"', searchGeoScope$toolState, '","name":"', searchGeoScope$toolName, '","link":"', searchGeoScope$toolLink, '"}', sep="", collapse=",\n")
geoScopeData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchGeoScope_state.json")
writeLines(geoScopeData, file(writeFile))
##county
createRecs <- paste('{"geoScopeCNTY":"', searchGeoScope$toolLoc, '","name":"', searchGeoScope$toolName, '","link":"', searchGeoScope$toolLink, '"}', sep="", collapse=",\n")
geoScopeData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchGeoScope_county.json")
writeLines(geoScopeData, file(writeFile))
##coastal
#createRecs <- paste('{"geoScopeCoast":"', searchGeoScope$coastal, '","name":"', searchGeoScope$toolName, '","link":"', searchGeoScope$toolLink, '"}', sep="", collapse=",\n")
fff <- cbind.data.frame(searchGeoScope$coastal, searchGeoScope$toolName, searchGeoScope$toolLink)
ggg <- unique(fff)
createRecs <- paste('{"geoScopeCoast":"', ggg$`searchGeoScope$coastal`, '","name":"', ggg$`searchGeoScope$toolName`, '","link":"', ggg$`searchGeoScope$toolLink`, '"}', sep="", collapse=",\n")
geoScopeData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchGeoScope_coastal.json")
writeLines(geoScopeData, file(writeFile))


##########################################################################
# Jscript tool function
##########################################################################
##create the tool search object jscript for tool function
createRecs <- paste('{"toolFun":"', searchToolFunct$toolFunct, '","name":"', searchToolFunct$toolName, '","link":"', searchToolFunct$toolLink, '"}', sep="", collapse=",\n")
toolFunData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchToolFun.json")
writeLines(toolFunData, file(writeFile))
##########################################################################
# Jscript topic filter
##########################################################################
##create the tool search object jscript for topic filter
createRecs <- paste('{"topFilter":"', searchTopFilters$topFilterNam, '","name":"', searchTopFilters$toolName, '","link":"', searchTopFilters$toolLink, '"}', sep="", collapse=",\n")
mainFiltData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchTopFilter.json")
writeLines(mainFiltData, file(writeFile))
##create the tool search object jscript for sub-topic filter
createRecs <- paste('{"subFilter":"', searchTopFilters$subFilter, '","name":"', searchTopFilters$toolName, '","link":"', searchTopFilters$toolLink, '"}', sep="", collapse=",\n")
subFiltData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchSubFilter.json")
writeLines(subFiltData, file(writeFile))


# ##fill out the tool search page code
# ##tags
# uniqueTags <- unique(searchTags$tag)
# createTagBoxes <- paste('<label><input type="checkbox" aria-selected="false" class="tagChecks" value="', uniqueTags, '">', uniqueTags, '</label>', sep="", collapse="<br>\n")
# templateSearchTemp <- gsub("<inputTags>", createTagBoxes, templateSearchTemp)
# ##software requirements
# uniqueReqs <- unique(searchSoftReqs$softReqs)
# createReqBoxes <- paste('<label><input type="checkbox" aria-selected="false" class="reqChecks" value="', uniqueReqs, '">', uniqueReqs, '</label>', sep="", collapse="<br>\n")
# templateSearchTemp <- gsub("<inputSoftReq>", createReqBoxes, templateSearchTemp)
# ##geographic scope
# uniqueGeoScope <- unique(searchGeoScope$geoScope)
# createGeoBoxes <- paste('<label><input type="checkbox" aria-selected="false" class="geoChecks" value="', uniqueGeoScope, '">', uniqueGeoScope, '</label>', sep="", collapse="<br>\n")
# templateSearchTemp <- gsub("<inputGeoScp>", createGeoBoxes, templateSearchTemp)
# ##tool function
# uniqueToolFunct <- unique(searchToolFunct$toolFunct)
# createTFBoxes <- paste('<label><input type="checkbox" aria-selected="false" class="toolFunctChecks" value="', uniqueToolFunct, '">', uniqueToolFunct, '</label>', sep="", collapse="<br>\n")
# templateSearchTemp <- gsub("<inputToolFun>", createTFBoxes, templateSearchTemp)
# ##topic filter
# uniqueTopFilters <- unique(searchTopFilters$topFilterNam)
# createTFiltBoxes <- paste('<label><input type="checkbox" aria-selected="false" class="topFiltChecks" value="', uniqueTopFilters, '">', uniqueTopFilters, '</label>', sep="", collapse="<br>\n")
# templateSearchTemp <- gsub("<inputTopFilter>", createTFiltBoxes, templateSearchTemp)
# ##sub-topic filter
# uniqueSubFilters <- unique(data.frame(topFilter=searchTopFilters$topFilter, subFilter=searchTopFilters$subFilter))
# createSFiltBoxes <- paste('<label class="sublabs-', uniqueSubFilters$topFilter, '"><input type="checkbox" aria-selected="false" class="subFiltChecks" value="', uniqueSubFilters$subFilter, '">', uniqueSubFilters$subFilter, '</label>', sep="", collapse="<br>\n")
# templateSearchTemp <- gsub("<inputSubFilter>", createSFiltBoxes, templateSearchTemp)

##for when finally ready to make fully automatic
#writeFile <- paste0(toolSearchDir, "toolsearch.html")
#writeLines(templateSearchTemp, file(writeFile))








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


