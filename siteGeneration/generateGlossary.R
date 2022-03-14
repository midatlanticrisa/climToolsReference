library(readxl)
library(openxlsx)

# ---
# layout: page
# title: About
# ---

baseDir <- "~/Documents/Github/"
dataDir <- paste0(baseDir, "climToolsReference/siteGeneration/")

siteDir <- paste0(baseDir, "climToolsReference/content/")
toolsDir <- paste0(siteDir, "glossary/")

toolInventoryName <- "glossary.xlsx"
filenm = "index.md"
layout= "page"
title = "Glossary"

##create the various subdirectories for the site
if(dir.exists(toolsDir)==FALSE){ # public/tools/ directory
  dir.create(toolsDir, recursive=TRUE)
}

# Read glossary definitions -----------------------------------------------

##read in the tool description document
glossaryFile <- list.files(dataDir, toolInventoryName, recursive=T, full.names=T)
glossaryFile <- glossaryFile[grep("~",glossaryFile, invert=TRUE)]

##get number of sheets, to load all of them
sheetNames <- excel_sheets(glossaryFile)

# Use read.xlsx to read the data
glossary <- read.xlsx(sheetNames, xlsxFile=glossaryFile, colNames=TRUE)
ord_ind = order(glossary$keyword) # Order alphabetically
glossary = glossary[ord_ind, ]

firstCharacter <- substr(glossary$keyword, 1, 1)

groupedGlos = list()
for(i in LETTERS){
  if(any(firstCharacter == i)){
    letterGlos <- glossary[firstCharacter == i, ]
    groupedGlos[[i]] = c(paste0('<a id="', i, '"><h3>', i, '</h3></a>'), "<DL>", 
                         paste0("<DT>", letterGlos$keyword, "<DD>", 
                                letterGlos$defintion), "</DL>")
  } else {
    groupedGlos[[i]] = paste0('<a id="', i, '"><h3>', i, '</h3></a><br>')
  }
  # groupedGlos[[i]] = glossary[firstCharacter == i, ]
}


page.layout = c("---", paste("layout:", layout), paste("title:", title), "---")

# LETTERS; alphabet
floatingLetterLinks <- paste0('<a href="#', LETTERS, '">', LETTERS, '</a>', collapse = " ")

format.glossary <- c(page.layout, "", paste0("<h3>", floatingLetterLinks, "</h3>"), 
                     "", unlist(groupedGlos))


# <a href="#A">A</a> <a href="#B">B</a> 
# <a id="A">This is the Facebook ad example I want to link to.</a>

# format.glossary <- c(page.layout, "<DL>", paste0("<DT>", glossary$keyword, "<DD>", 
#                                     glossary$defintion), "</DL>")

cat(format.glossary, file=paste0(toolsDir, filenm), sep="\n")


groupedtest = list()
for(i in LETTERS){
  if(i == "Z"){ # Ensure there is no ending comma
    if(any(firstCharacter == i)){
      letterGlos <- glossary[firstCharacter == i, ]
      
      alldefs <- paste0('<DT>', letterGlos$keyword, '<DD>', letterGlos$defintion, collapse="")
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h3>', i, 
                                '</h3><DL>', alldefs, '</DL>"}')
    } else {
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h3>', i, '</h3>"}')
    }
    
  } else { # make sure there is an ending comma
    if(any(firstCharacter == i)){
      letterGlos <- glossary[firstCharacter == i, ]
      
      alldefs <- paste0('<DT>', letterGlos$keyword, '<DD>', letterGlos$defintion, collapse="")
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h3>', i, 
                                '</h3><DL>', alldefs, '</DL>"},')
    } else {
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h3>', i, '</h3>"},')
    }
  }
}

json.glossary <- c("var glossary = {", unlist(groupedtest), "}")
cat(json.glossary , file=paste0(toolsDir, "glossary.js"), sep="\n")
