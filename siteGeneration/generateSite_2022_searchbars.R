##########################################################################
##########################################################################
## Script Name: generateSite.R
## Purpose of Script: A script to automatically pull apart input data and present that data in a website.
##
## Author: Kelsey Ruckert
## Email: klr324@psu.edu
## Date Created: 4/6/2022
##
## Copyright (c) 2022 The Pennsylvania State University
##
##########################################################################
##########################################################################
##libraries
library(readxl)
library(pbapply)
library(stringi)
# install.packages("webshot")
library(webshot)
library(openxlsx)
##webshot::install_phantomjs()
library(dplyr)
library(stringr)
library(tidycensus)
library(jsonlite)

baseDir <- "~/Documents/Github/"
dataDir <- paste0(baseDir, "climToolsReference/siteGeneration/")
source(paste0(dataDir, "siteGeneratingFunctions_2022_searchbars.R"))

siteDir <- paste0(baseDir, "climToolsReference/")
tagDir <- paste0(siteDir, "_tag/")
toolsDir <- paste0(siteDir, "_individualtools/")
discovDir <- paste0(baseDir, "climToolsReference/misc/")
imageDir <- paste0(baseDir, "climToolsReference/images/")

toolInventoryName <- "Tool Inventory - 12.15.21 copy.xlsx"
templateCollectionName <- "collectionTemplate.md"
templatePageName <- "pageTemplate.md"
templateTagName <- "tagTemplate.md"
templateIndex <- "toolsTemplate.md" # "templateIndex.md"
searchCodePage <- "searchTemplate.html"
stcntyFile <- paste0(dataDir, "MARISAstateFIPSUpdate.csv")
statefips = read.csv(paste0(dataDir, "stateFIPS.csv"))

##line break character
el <- "\n"
tab <- "  "

##########################################################################
# STOP - DO YOU WANT TO CLEAN THE TOOLS?
# THIS IS A MUST IF YOU HAVE REMOVED TOOLS OR REMOVED TAGS
# clean_website_tools_and_tags <- FALSE: NO
# clean_website_tools_and_tags <- TRUE: YES
##########################################################################
clean_website_tools_and_tags <- FALSE #TRUE

# if true remove the files/subdirectories
if(clean_website_tools_and_tags){
  # Remove all the tool_pages. Leave the index
  rm_toolpages <- grep("page", list.files(toolsDir, full.names = TRUE), value=TRUE)
  file.remove(rm_toolpages)
  
  # Remove all the tool_pages in collection. Leave the index
  rm_toolpages <- grep("page", list.files(paste0(siteDir, "collection/"), full.names = TRUE), value=TRUE)
  file.remove(rm_toolpages)
  
  # Refresh everything in tags:
  system(paste0("rm -rf ", baseDir, "climToolsReference/public/tags/*"))
  
  # Refresh everything in data:
  rm_datajson <- list.files(discovDir, full.names = TRUE)
  file.remove(rm_datajson)
  
  # Refresh everything in tools (a mix of folders and files):
  system(paste0("rm -rf ", baseDir, "climToolsReference/public/tools/*"))
  
  # Refresh everything in collection (a mix of folders and files):
  system(paste0("rm -rf ", baseDir, "climToolsReference/public/collection/*"))
}
##########################################################################
##########################################################################

##read in the tool description document
toolInventoryFile <- list.files(dataDir, toolInventoryName, recursive=T, full.names=T)
toolInventoryFile <- toolInventoryFile[grep("~",toolInventoryFile, invert=TRUE)]

##get number of sheets, to load all of them
sheetNames <- excel_sheets(toolInventoryFile)

##read in data
# Each list represents a seperate excel sheet

# Use read.xlsx to get the merged cells
toolInventory <- lapply(sheetNames, read.xlsx, xlsxFile=toolInventoryFile, colNames=F, fillMergedCells = TRUE)
names(toolInventory) <- sheetNames
stcntyTab <- read.csv(stcntyFile) # Read the fips csv file

##inventory, cleaning headers to be useful
inventory <- toolInventory$Inventory # toolInventory[[1]]
inventory <- toolInventory$Inventory
catNames <- as.character(inventory[1,]) # Top line header (category names) for sheet 1: Inventory
baseNames <- as.character(inventory[2,]) # Secondary header (sub-category names) for sheet 1: Inventory
level3Names <- as.character(inventory[3,]) # Secondary header (sub-category names) for sheet 1: Inventory
level4Names <- as.character(inventory[4,]) # Secondary header (sub-category names) for sheet 1: Inventory

# Combine the top line and secondary headers to set as the column names
headers = rep(NA, length(catNames))
for(i in 1:length(catNames)){
  if(is.na(catNames[i])){
    headers[i] = baseNames[i]
  } else {
    headers[i] = paste(catNames[i], baseNames[i], sep="-")
  }
}

# Find the duplicate headers and add the third category of names to the header
n_occur <- data.frame(table(headers))
dup_headers <- unique(headers[headers %in% n_occur$headers[n_occur$Freq > 1]])
ind_dup = unlist(lapply(X = dup_headers, function(X){which(headers == X)}))
headers[ind_dup] = paste(headers[ind_dup], level3Names[ind_dup], sep="-")

# Find the duplicate headers and add the third category of names to the header
n_occur_4level <- data.frame(table(headers))
dup_headers_4level <- unique(headers[headers %in% n_occur_4level$headers[n_occur_4level$Freq > 1]])

# Make sure website friendliness is included (it is currently not a duplicate)
if(!("Description-Tags-Website friendliness" %in% dup_headers_4level)){
  dup_headers_4level <- c(dup_headers_4level, "Description-Tags-Website friendliness")
}

ind_dup_4level = unlist(lapply(X = dup_headers_4level, function(X){which(headers == X)}))
headers[ind_dup_4level] = paste(headers[ind_dup_4level], level4Names[ind_dup_4level], sep="-")
                      
# # Combine the top line and secondary headers to set as the column names
colnames(inventory) <- headers # baseNames
inventory <- inventory[which(inventory$`Tool ID`=="1"):nrow(inventory),] # Extract only the tools (Should remove the first 4 rows; the header rows)
invValsOnly <- inventory[which(is.na(inventory$`Group ID`)==F),] # Extract the only the ones with Group IDs - should be all the tools

##needed with tool id with new file, may not be needed in future versions
# Format the Tool ID to have just 1 decimal point and add a space padding if its a single digit number
invValsOnly$`Tool ID` <- as.character(format(round(as.numeric(invValsOnly$`Tool ID`),1)),nsmall=1)

# Remove all columns after: "Documentation-Related Publications"
doc_ind <- grep("Documentation", colnames(invValsOnly))
invValsOnly <- invValsOnly[ ,1:doc_ind[length(doc_ind)]] 

# Remove columns that are crossed out in the excel sheet:
# "dat", "fig", "gis", "map", "rprt" 
crossNames <- c("Tool Outputs-dat-dat- numerical data", "Tool Outputs-fig", 
                "Tool Outputs-gis", "Tool Outputs-map", "Tool Outputs-rprt-rprt- reports")
invValsOnly <- invValsOnly[ ,!(colnames(invValsOnly) %in% crossNames)]

##read in the template files to be editted
findColFile <- list.files(dataDir, templateCollectionName, recursive=T, full.names=T)
collectionPage <- paste(readLines(findColFile), collapse="\n")
findTempFile <- list.files(dataDir, templatePageName, recursive=T, full.names=T)
templatePage <- paste(readLines(findTempFile), collapse="\n")
findTagFile <- list.files(dataDir, templateTagName, recursive=T, full.names=T)
templateTag <- paste(readLines(findTagFile), collapse="\n")
findIndFile <- list.files(dataDir, templateIndex, recursive=T, full.names=T)
templateIndexPg <- paste(readLines(findIndFile), collapse="\n")
findSearchTempFile <- list.files(dataDir, searchCodePage, recursive=T, full.names=T)
templateSearchTemp <- paste(readLines(findSearchTempFile), collapse="\n")

##split by group id, seems to be coorolated with developers
toolIDs <- unique(invValsOnly$`Tool ID`) # Make sure all values are different
toolIDs <- toolIDs[is.na(toolIDs)==F] # Make sure all values are not NA
# Convert the data.frame to a list where each item is a different tool
splitByToolID <- lapply(toolIDs, function(x){invValsOnly[which(invValsOnly$`Tool ID`==x & is.na(invValsOnly$`Tool ID`)==F),]})

# Find tool collections and group by collection
multiTools <- toolIDs[grep("\\.[1-9]", toolIDs)]
multiToolsID <- unique(gsub("\\..*", "", multiTools))
toolCol <- toolIDs[which(toolIDs %in% paste0(multiToolsID, ".0"))]

splitByCol <- lapply(multiToolsID, function(x){colID <- grep(x, toolIDs); invValsOnly[colID, ]})

##########################################################################
# Set up search engine??
##########################################################################

# ##create the various subdirectories for the site
# if(dir.exists(toolsDir)==F){ # public/tools/ directory
#   dir.create(toolsDir, recursive=T)
# }
# if(dir.exists(discovDir)==F){ # public/data/ directory
#   dir.create(discovDir, recursive=T)
# }
# if(dir.exists(imageDir)==F){ # public/images/ directory
#   dir.create(imageDir, recursive=T)
# }

# Create a series of empty data.frames with specific column names
# searchTags <- data.frame(toolName=NA, toolLink=NA, tag=NA)
# searchSoftReqs <- data.frame(toolName=NA, toolLink=NA, softReqs=NA)
# searchGeoScope <- data.frame(toolName=NA, toolLink=NA, toolState=NA, toolLoc=NA, coastal=NA)
# searchToolFunct <- data.frame(toolName=NA, toolLink=NA, toolFunct=NA)
# searchTopFilters <- data.frame(toolName=NA, toolLink=NA, topFilter=NA, topFilterNam=NA,  subFilter=NA)
# #searchSubFilters <- data.frame(toolName=NA, toolLink=NA, subFilter=NA)

#ttt <- data.frame(table(invValsOnly$`Geographic Scope-Scope`, useNA="ifany"))
#hhh <- invValsOnly[invValsOnly$`Geographic Scope-Scope`=="National",]

# Run the generateToolPage function on each tool. #### IT APPEARS THE ABOVE DATA.FRAMES ARE USED GLOBALLY!!
toolpage_list = pblapply(splitByToolID, generateToolPage, tempPage=templatePage, 
                         el=el, siteDir=siteDir, scTab=stcntyTab, updateContent=TRUE,
                         splitByCol=splitByCol, toolCol=toolCol, collectionPage=collectionPage,
                         multiTools=multiTools)

# Tool Highlights
highlights <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$listTools})
highlights <- do.call(rbind.data.frame, highlights)

rmcollections <- highlights[-which(is.na(highlights$url)), ]
orderedTools <- rmcollections[order(rmcollections$name), ]
toolHome <- paste(templateIndexPg, paste0(orderedTools$img, orderedTools$url, orderedTools$description, 
                                          orderedTools$tags, collapse=el), sep=el)

conTool <- file(paste0(siteDir, "tools.md")) # Connect to the file
writeLines(toolHome, conTool)
close(conTool)

# Tags
searchTags <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$searchTags})
searchTags <- do.call(rbind.data.frame, searchTags)

# Software requirements
searchSoftReqs <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$searchSoftReqs})
searchSoftReqs <- do.call(rbind.data.frame, searchSoftReqs)

# Geographic scope
searchGeoScope <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$searchGeoScope})
searchGeoScope <- do.call(rbind.data.frame, searchGeoScope)

# Tool Function
searchToolFunct <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$searchToolFunct})
searchToolFunct <- do.call(rbind.data.frame, searchToolFunct)

# Tp Filters
searchTopFilters <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$searchTopFilters})
searchTopFilters <- do.call(rbind.data.frame, searchTopFilters)

# Tool attributes
toolAttributes <- lapply(X = 1:length(toolpage_list), function(X){toolpage_list[[X]]$tooljson})

# Convert to json format
attributejson = sapply(X = 1:length(toolpage_list), 
                     function(X){if(X == length(toolpage_list)){
                       paste('{\n "tools": ', toJSON(toolAttributes[[X]], pretty=TRUE, auto_unbox=TRUE),
                             '\n }')
                     } else {
                       paste('{\n "tools": ', toJSON(toolAttributes[[X]], pretty=TRUE, auto_unbox=TRUE),
                             '\n },')
                       }})
# Save as a variable in a javascript file
toolsjson = c("var toolsjson = [", attributejson, "];")
cat(toolsjson, file=paste0(discovDir, "toolsFilter.js"), sep="\n")

# Tag pages
filterTags <- searchTags[-which(searchTags$tag == ""), ]
unqTags <- unique(filterTags$tag)

for(i in 1:length(unqTags)){
  indTag <- which(filterTags$tag == unqTags[i])
  header <- paste0("<h1>Tag: ", unqTags[i], "</h1>")
  bullets <- paste0("* [", filterTags$toolName[indTag], "](/", filterTags$toolLink[indTag], "/){:target='blank'}", collapse = el)
  
  tagPage <- gsub("titleholder", unqTags[i], templateTag)
  tagPage <- gsub("placeholder", paste0(gsub("/", "", unqTags[i]), "/"), tagPage)
  
  toolTag <- paste(tagPage, header, bullets, sep=el)
  
  conTool <- file(paste0(tagDir, gsub("/", "", unqTags[i]), ".md")) # Connect to the file
  writeLines(toolTag, conTool)
  close(conTool)
}



# STOP --------------------------------------------------------------------



# # Remove the first row from the data.frames???
# searchTags <- searchTags[-1,]
# searchSoftReqs <- searchSoftReqs[-1,]
# searchGeoScope <- searchGeoScope[-1,]
# searchToolFunct <- searchToolFunct[-1,]
# searchTopFilters <- searchTopFilters[-1,]
# #searchSubFilters <- searchSubFilters[-1,]

# Set non coastal areas to 0 and coastal areas to 1. Set to numeric values
searchGeoScope$coastal[is.na(searchGeoScope$coastal)] <- 0
searchGeoScope$coastal[searchGeoScope$coastal=="x"] <- 1
searchGeoScope$coastal <- as.numeric(searchGeoScope$coastal)

# Generate a markdown files, an index file, to create the tools page:
# its in content/tools
# el is end line "/n"
##set up tool index file
toolIndex <- gsub("TemplateIndex", "Tools", templateIndexPg)
toolIndex <- paste0(toolIndex, el, el, "A collection of articles, presentations or talks, most likely on stuff and stuff.")
writeFile <- paste0(toolsDir, "_index.md")
conFile <- file(writeFile)
writeLines(toolIndex, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

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
conFile <- file(writeFile)
writeLines(softReqsData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

##########################################################################
# Jscript geographic scope
##########################################################################
##create the tool search object jscript for geographic scope
##state
state.df <- cbind.data.frame(searchGeoScope$toolState, searchGeoScope$toolName, searchGeoScope$toolLink)
state.unq <- unique(state.df) # Keep only the unique ones so we don't have duplicates
createRecs <- paste('{"geoScopeST":"', state.unq$'searchGeoScope$toolState', '","name":"', 
                    state.unq$'searchGeoScope$toolName', '","link":"', state.unq$'searchGeoScope$toolLink', 
                    '"}', sep="", collapse=",\n")
geoScopeData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchGeoScope_state.json")
conFile <- file(writeFile)
writeLines(geoScopeData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

##county
createRecs <- paste('{"geoScopeCNTY":"', searchGeoScope$toolLoc, '","name":"', 
                    searchGeoScope$toolName, '","link":"', searchGeoScope$toolLink, 
                    '"}', sep="", collapse=",\n")
geoScopeData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchGeoScope_county.json")
conFile <- file(writeFile)
writeLines(geoScopeData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

##coastal
#createRecs <- paste('{"geoScopeCoast":"', searchGeoScope$coastal, '","name":"', searchGeoScope$toolName, '","link":"', searchGeoScope$toolLink, '"}', sep="", collapse=",\n")
fff <- cbind.data.frame(searchGeoScope$coastal, searchGeoScope$toolName, searchGeoScope$toolLink)
ggg <- unique(fff)
createRecs <- paste('{"geoScopeCoast":"', ggg$`searchGeoScope$coastal`, '","name":"', 
                    ggg$`searchGeoScope$toolName`, '","link":"', 
                    ggg$`searchGeoScope$toolLink`, '"}', sep="", collapse=",\n")
geoScopeData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchGeoScope_coastal.json")
conFile <- file(writeFile)
writeLines(geoScopeData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

##########################################################################
# Jscript tool function
##########################################################################
##create the tool search object jscript for tool function
createRecs <- paste('{"toolFun":"', searchToolFunct$toolFunct, '","name":"', 
                    searchToolFunct$toolName, '","link":"', searchToolFunct$toolLink, 
                    '"}', sep="", collapse=",\n")
toolFunData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchToolFun.json")
conFile <- file(writeFile)
writeLines(toolFunData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

##########################################################################
# Jscript topic filter
##########################################################################
##create the tool search object jscript for topic filter
createRecs <- paste('{"topFilter":"', searchTopFilters$topFilterNam, '","name":"', 
                    searchTopFilters$toolName, '","link":"', 
                    searchTopFilters$toolLink, '"}', sep="", collapse=",\n")
mainFiltData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchTopFilter.json")
conFile <- file(writeFile)
writeLines(mainFiltData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

##create the tool search object jscript for sub-topic filter
createRecs <- paste('{"subFilter":"', searchTopFilters$subFilter, '","name":"', 
                    searchTopFilters$toolName, '","link":"', searchTopFilters$toolLink, 
                    '"}', sep="", collapse=",\n")
subFiltData <- paste0('{', el, '"items":[', el, createRecs, el, ']', el, '}')
writeFile <- paste0(discovDir, "searchSubFilter.json")
conFile <- file(writeFile)
writeLines(subFiltData, conFile)
close(conFile) # Make sure to close the connection to prevent errors or warnings

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


