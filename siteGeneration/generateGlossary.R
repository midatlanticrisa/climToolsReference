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
searchDir <- paste0(siteDir, "toolsearch/")

toolInventoryName <- "glossary.xlsx"
filenm = "index.md"
type = "page"
title = "Glossary"
layout = "glossary"

##create the various subdirectories for the site
if(dir.exists(toolsDir)==FALSE){ # public/tools/ directory
  print(paste("Creating", toolsDir, "directory"))
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
definitions <- glossary
ord_ind = order(glossary$keyword) # Order alphabetically
glossary = glossary[ord_ind, ]

firstCharacter <- substr(glossary$keyword, 1, 1)

# groupedGlos = list()
# for(i in LETTERS){
#   if(any(firstCharacter == i)){
#     letterGlos <- glossary[firstCharacter == i, ]
#     groupedGlos[[i]] = c(paste0('<a id="', i, '"><h3>', i, '</h3></a>'), "<DL>", 
#                          paste0("<DT>", letterGlos$keyword, "<DD>", 
#                                 letterGlos$defintion), "</DL>")
#   } else {
#     groupedGlos[[i]] = paste0('<a id="', i, '"><h3>', i, '</h3></a><br>')
#   }
#   # groupedGlos[[i]] = glossary[firstCharacter == i, ]
# }
# 
# 
page.layout = c("---", paste("type:", type), paste("title:", title), 
                paste("layout:", layout), "---")
format.glossary <- page.layout
# 
# # LETTERS; alphabet
# floatingLetterLinks <- paste0('<a href="#', LETTERS, '">', LETTERS, '</a>', collapse = " ")
# 
# format.glossary <- c(page.layout, "", paste0("<h3>", floatingLetterLinks, "</h3>"), 
#                      "", unlist(groupedGlos))
# 
# 
# # <a href="#A">A</a> <a href="#B">B</a> 
# # <a id="A">This is the Facebook ad example I want to link to.</a>
# 
# # format.glossary <- c(page.layout, "<DL>", paste0("<DT>", glossary$keyword, "<DD>", 
# #                                     glossary$defintion), "</DL>")
# 
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

option_tag <- paste0('<option value="', tags, '">', tags, "</option>", collapse = "\n")
option_filter <- paste0('<option value="', filters, '">', filters, "</option>", collapse = "\n")
option_function <- paste0('<option value="', functions, '">', functions, "</option>", collapse = "\n")
option_skill <- paste0('<option value="', skill, '">', skill, "</option>", collapse = "\n")
option_subfilter <- paste0('<option value="', subs, '">', subs, "</option>", collapse = "\n")

dropdown <- paste0('<label for="definitionfunction"><b>Need a definition?</b></label><br>\n',
  '<select class="js-definitionfunction-single" id="definitionfunction" style="width: 100%">\n',
  '<option></option>\n<optgroup label="Tool function">\n', option_function, 
  '</optgroup>\n<optgroup label="Topics">\n', option_filter, '</optgroup>\n<optgroup label="Sub-topics">\n',
  option_subfilter, '</optgroup>\n<optgroup label="Tags">\n', option_skill, '\n', option_tag, 
  '</optgroup>\n</select>')
cat(dropdown, file=paste0(searchDir, "definition.html"), sep="\n")

defsjson = paste('var defsjson = [\n', paste0('{"defs":  {\n"name": "', definitions$keyword,
       '",\n"definition": "', definitions$defintion, '"\n}}', collapse=",\n"), '\n];')
cat(defsjson, file=paste0(searchDir, "definition.js"), sep="\n")


