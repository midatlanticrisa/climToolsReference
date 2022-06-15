##########################################################################
##########################################################################
## Script Name: generateGlossary.R
## Purpose of Script: Generate json files of definitions.
##
## Author: Kelsey Ruckert
## Email: klr324@psu.edu
## Date Created: 4/6/2022
##
## Copyright (c) 2022 The Pennsylvania State University
##
##########################################################################
##########################################################################
library(readxl)
library(openxlsx)

# Set directories using relative paths
toolsDir <- "../misc/"
searchDir <- "../_includes/"
toolInventoryName <- "glossary.xlsx"

## Ensure the misc file exists
if(dir.exists(toolsDir)==FALSE){ # public/tools/ directory
  print(paste("Creating", toolsDir, "directory"))
  dir.create(toolsDir, recursive=TRUE)
}

# Read glossary definitions -----------------------------------------------

# Read in the document with definitions
# glossaryFile <- list.files(dataDir, toolInventoryName, recursive=T, full.names=T)
# glossaryFile <- glossaryFile[grep("~",glossaryFile, invert=TRUE)]
glossaryFile <-toolInventoryName

# Get number of sheets, to load all of them
sheetNames <- excel_sheets(glossaryFile)

# Use read.xlsx to read the data
glossary <- read.xlsx(sheetNames, xlsxFile=glossaryFile, colNames=TRUE)
definitions <- glossary
ord_ind = order(glossary$keyword) # Order alphabetically
glossary = glossary[ord_ind, ]

# Extract the first letter of each keyword
firstCharacter <- substr(glossary$keyword, 1, 1)

# Group definitions by the first letter of the keyword
groupedtest = list()
for(i in LETTERS){
  if(i == "Z"){ # Ensure there is no ending comma
    if(any(firstCharacter == i)){
      letterGlos <- glossary[firstCharacter == i, ]
      
      alldefs <- paste0('<DT><b>', letterGlos$keyword, '</b><DD>', letterGlos$defintion, "<br>", collapse="")
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h2>', i, 
                                '</h2><DL>', alldefs, '</DL>"}')
    } else {
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h2>', i, '</h2>"}')
    }
    
  } else { # make sure there is an ending comma
    if(any(firstCharacter == i)){
      letterGlos <- glossary[firstCharacter == i, ]
      
      alldefs <- paste0('<DT><b>', letterGlos$keyword, '</b><DD>', letterGlos$defintion, "<br><br>", collapse="")
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h2>', i, 
                                '</h2><DL>', alldefs, '</DL>"},')
    } else {
      groupedtest[[i]] = paste0('"', i, '": {\n "text": "<h2>', i, '</h2>"},')
    }
  }
}
# Save glossary definitions as a json file
json.glossary <- c("var glossary = {", unlist(groupedtest), "}")
cat(json.glossary , file=paste0(toolsDir, "glossary.js"), sep="\n")

# Create json and html file for definition look up in tool search  --------
# Parse and order definitions by topic
tag_ind = grep("(tag)", definitions$defintion)
filter_ind = grep("(topic filter)", definitions$defintion)
function_ind = grep("(tool function)", definitions$defintion)
skill_ind = grep("(skill level)", definitions$defintion)
subfilters = definitions[-c(tag_ind, filter_ind, function_ind, skill_ind), ]

ordered_defs <- rbind(definitions[function_ind, ], definitions[filter_ind, ], 
                      subfilters, definitions[skill_ind, ], definitions[tag_ind, ])

tags <- definitions$keyword[tag_ind][order(definitions$keyword[tag_ind])]
filters <- definitions$keyword[filter_ind][order(definitions$keyword[filter_ind])]
functions <- definitions$keyword[function_ind][order(definitions$keyword[function_ind])]
skill <- definitions$keyword[skill_ind]
subs <- subfilters$keyword[order(subfilters$keyword)]

# Set keywords as dropdown options
option_tag <- paste0('<option value="', tags, '">', tags, "</option>", collapse = "\n")
option_filter <- paste0('<option value="', filters, '">', filters, "</option>", collapse = "\n")
option_function <- paste0('<option value="', functions, '">', functions, "</option>", collapse = "\n")
option_skill <- paste0('<option value="', skill, '">', skill, "</option>", collapse = "\n")
option_subfilter <- paste0('<option value="', subs, '">', subs, "</option>", collapse = "\n")

# Save the dropdown menu to a html file
dropdown <- paste0('<label for="definitionfunction"><b>Need a definition?</b></label><br>\n',
  '<select class="js-definitionfunction-single" id="definitionfunction" style="width: 100%">\n',
  '<option></option>\n<optgroup label="Tool function">\n', option_function, 
  '</optgroup>\n<optgroup label="Topics">\n', option_filter, '</optgroup>\n<optgroup label="Sub-topics">\n',
  option_subfilter, '</optgroup>\n<optgroup label="Tags">\n', option_skill, '\n', option_tag, 
  '</optgroup>\n</select>')
cat(dropdown, file=paste0(searchDir, "definition.html"), sep="\n")

# Save the matching dropdown information to a json file
defsjson = paste('var defsjson = [\n', paste0('{"defs":  {\n"name": "', definitions$keyword,
       '",\n"definition": "', definitions$defintion, '"\n}}', collapse=",\n"), '\n];')
cat(defsjson, file=paste0(toolsDir, "definition.js"), sep="\n")

##########################################################################
##########################################################################
