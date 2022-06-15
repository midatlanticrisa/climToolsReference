##site generating functions
##########################################################################
##########################################################################
formatStarStrings <- function(str){ # Not USED?
  #################
  #str <-toolRec$`Target Audience`
  #################
  
  checkStr <- strsplit(str, "\r\n")[[1]]
  checkStr <- sapply(checkStr, function(chr){subChr<-substr(chr,2,nchar(chr));
  trimChr<-gsub("^\\s+|\\s+$","",subChr);
  return(trimChr)})
  return(checkStr)
}
##########################################################################
## Function to look up state names using state abbreviations
##########################################################################
stateLookUp <- function(abb, statecodes){
  statecodes$Name[match(abb, statecodes$AlphaCode)]
}
##########################################################################
##########################################################################
# createDeveloperDirs <- function(inRecs, tempFile, siteDir, updateContent=F){
#   #################
#   #inRecs <- invValsOnly
#   #tempFile <- chaptPage
#   #siteDir <- siteDir
#   #################
#   ##remove NA from the vector
#   #inRecs$Developer[is.na(inRecs$Developer)==T] <- "Unknown"
#   
#   #splitDevs <- lapply(inRecs$Developer, function(txt){strsplit(txt, split="; ")[[1]]})
#   #names(splitDevs) <- inRecs$`Tool ID`
#   #uniqueDevs <- sort(unique(unlist(splitDevs)), decreasing=F)
#   ##if directories do not exist, create them
#   sapply(uniqueDevs, function(dev){if(dir.exists(paste0(siteDir,dev))==F){
#     dir.create(paste0(siteDir,dev), recursive=T)};
#     ##create an index file for each directory                              
#     if((file.exists(paste0(siteDir,dev,"/_index.md"))==F | updateContent==T)){
#       chapFile <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), tempFile)
#       chapFile <- gsub("Chapter Title", dev, chapFile)
#       #chapFile <- gsub("draft: true", "draft: false", chapFile)
#       chapFile <- paste0(chapFile, "\n",
#                          "## ", dev, "\n",
#                          "\n",
#                          "### List of Tools:\n")
#       writeFile <- paste0(siteDir,dev,"/_index.md")
#       writeLines(chapFile, file(writeFile))
#     }})
#   return(splitDevs)
# }
##########################################################################
##########################################################################
# toolRec: data.frame of information for a tool
# tempPage: markdown template page
# el: end of line marker, "/n"
# siteDir: path to output directory
# scTab: data.frame of states, county names, and whether they are coastal
# updateContent=T: Boolean (True/FALSE), True will rewrite the files if they exist
generateToolPage <- function(toolRec, tempPage, el, siteDir, scTab, updateContent=T, 
                             splitByCol, toolCol, collectionPage, multiTools){
  #################
  #toolRec <- splitByToolID[[2]]
  #toolRec <- splitByToolID[[4]]
  #toolRec <- splitByToolID[[5]]
  #toolRec <- splitByToolID[[7]]
  #toolRec <- splitByToolID[[12]]
  #toolRec <- splitByToolID[[24]]
  #toolRec <- splitByToolID[[45]]
  #toolRec <- splitByToolID[[64]]
  #toolRec <- splitByToolID[[100]]
  #tempPage <- templatePage
  #el <- "\n"
  #siteDir <- toolsDir
  #scTab <- stcntyTab
  #updateContent <- T
  #################
  print(toolRec$`Tool Name`)
  
  # Create a tool list of attributes
  tooljson = list()
  tooljson$name = toolRec$`Tool Name`
  listTools <- data.frame(name = tooljson$name)
  
  ##check the developers, to make sure the directories are available and are set up properly
  # If the Developer column is NA, set it to unknown
  if(is.na(toolRec$Developer)){#==T){
    toolRec$Developer <- "Unknown"
  }
  ##if there is more than one developer, split the developers into a vector
  splitDevs <- strsplit(toolRec$Developer, "; ")[[1]]
  
  # Is it part of a tool collection? Save in the collection directory
  if(toolRec$`Tool ID` %in% multiTools){
    # Set the name of the markdown page as "page-tool<tool ID number>.md"
    ##the name of the tool page within the site
    toolIDtxt <- paste0("collection/page-tool", trimws(toolRec$`Tool ID`))  
    listTools$url <- NA
  } else {
    toolIDtxt <- paste0("individualtools/page-tool", trimws(toolRec$`Tool ID`)) 
    listTools$url <- paste0(el, "### [", tooljson$name, "](/", toolIDtxt, "/){:target='blank'}")
  }
  # Set the link as a tool attribute
  tooljson$link = toolIDtxt
  
  writeTool <- paste0(siteDir, "_", toolIDtxt, ".md")
  #writeTool <- paste0(siteDir, splitDevs[1], "/", toolIDtxt, ".md")  ##the file in which the tool information will be written to, assumes first developer is the most important
  toolRec$`Tool Name` <- gsub(el, " ", toolRec$`Tool Name`) ##remove any carriage returns from the tool name
  #toolDir <- paste0(siteDir, gsub("/|:", "-", toolRec$`Tool Name`), "/")
  
  # Check if the file exists
  # If the file does not exists or updateContent is TRUE,
  # then create the markdown page, otherwise do nothing.
  if(file.exists(writeTool)==FALSE | updateContent==TRUE){
    
    titleholder = gsub(":", "-", toolRec$`Tool Name`)
    # Is it a tool collection? Create a tool collection page
    if(toolRec$`Tool ID` %in% toolCol){
      ## Replace "page-template" with the tool name as the page title
      toolPage <- gsub("titleholder", titleholder, collectionPage)
      ## Update the link 
      toolPage <- gsub("collection/placeholder", paste0(tooljson$link, "/"), toolPage)
      
      # ### TOOLS HOMEPAGE DESCRIPTION
      # ## Change the description with the tool description or the name of the tool
      # # if no description exists
      if(is.na(toolRec$`Purpose-Description`)==FALSE){
        # toolPage <- gsub("a page", toolRec$`Purpose-Description`, toolPage)
        listTools$description <- paste0(el, toolRec$`Purpose-Description`, "<br><br><br><br><br>")
      }else{
        # toolPage <- gsub("a page", toolRec$`Tool Name`, toolPage)
        listTools$description <- NA
      }
      listTools$tags <- ""
      
      # Set tags as an empty space if no strengths exist
      toolTags <- ""
      
    } else { # A regular tool page
      ## Replace "page-template" with the tool name as the page title
      toolPage <- gsub("titleholder", titleholder, tempPage)
      ## Update the link 
      toolPage <- gsub("tools/placeholder", paste0(tooljson$link, "/"), toolPage)
      
      # ### TOOLS HOMEPAGE DESCRIPTION
      # ## Change the description with the tool description or the name of the tool
      # # if no description exists
      if(is.na(toolRec$`Purpose-Description`)==FALSE){
        listTools$description <- paste0(el, toolRec$`Purpose-Description`)
        # toolPage <- gsub("a page", toolRec$`Purpose-Description`, toolPage)
      }else{
        listTools$description <- paste0(el, toolRec$`Tool Name`)
        toolPage <- gsub("a page", toolRec$`Tool Name`, toolPage)
      }
      
      ### TOOLS HOMEPAGE TAGS
      # Find the tags
      tags_list <- toolRec[ ,grep("Description-Tags", colnames(toolRec))]
      named_tags <- gsub('^(?:[^-]*-){3}','', colnames(tags_list)) # Remove everything before the 3rd "-"
      tags <- named_tags[!is.na(tags_list)] # Determine which tags are not NA
      
      # Since all models are free, remove Free as a tag for now
      freetag <- which(tags == "Free")
      if(length(freetag) > 0){
        tags <- tags[-freetag]
      }
      
      # If Strengths exists, then set them as tags
      ##add  tags to the header
      ##and extract tags to add to search objects
      if(length(tags) > 0){
        # Update the "tags" with the list of tags
        # toolPage <- gsub('\"tags\"', paste(paste0('\"', unique(tags), '\"'), collapse=", "), toolPage) # commented out
        # Create a data.frame of tags
        toolTags <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, tag=tags)
        listTools$tags <- paste0(el, "<ul class='tags'>", paste0("<li><a class='tag' href='/tag/", 
                                                                 gsub("/", "", toolTags$tag), 
                                                                 "' target='_blank'>", toolTags$tag, "</a></li>", 
                                                                 collapse = el), "</ul><br><br><br>")
        # BIND the tags data.frame to the GLOBAL searchTags list
        # searchTags <<- rbind.data.frame(searchTags, toolTags)
      }else{
        #toolPage <- gsub('\"tags\"', '\"placeholder\"', toolPage)
        # Set tags as an empty space if no strengths exist
        # toolPage <- gsub('\"tags\"', '', toolPage) # commented out
        toolTags <- ""
        listTools$tags <- ""
      }
    }
    
    # # If Strengths exists, then set them as tags
    # ##add  tags to the header
    # ##and extract tags to add to search objects
    # if(is.na(toolRec$`Description-Strengths`)==F){
    #   # split the string of strengths into a vector of strengths
    #   tags <- strsplit(toolRec$`Description-Strengths`, "; |;  ")[[1]]
    #   # Update the "tags" with the list of tags..."strengths"
    #   toolPage <- gsub('\"tags\"', paste(paste0('\"', unique(tags), '\"'), collapse=", "), toolPage)
    #   # Create a data.frame of tags
    #   toolTags <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, tag=tags)
    #   # BIND the tags data.frame to the GLOBAL searchTags list
    #   searchTags <<- rbind.data.frame(searchTags, toolTags)
    # }else{
    #   #toolPage <- gsub('\"tags\"', '\"placeholder\"', toolPage)
    #   # Set tags as an empty space if no strengths exist
    #   toolPage <- gsub('\"tags\"', '', toolPage)
    # }
    
    # ##replace the placeholder image with an actual screenshot of the tool, if available
    # toolPage <- gsub("pageImage: https://www.887theriver.ca/wp-content/uploads/2017/07/placeholder.jpg", 
    #                  paste0("pageImage: https://cbtooltest.marisa.psu.edu/images/scaled_250_400/TOOLID_", 
    #                         trimws(toolRec$`Tool ID`), "_ScreenCapture-1.png"), toolPage)
    # toolPage <- gsub("thumbImage: https://www.887theriver.ca/wp-content/uploads/2017/07/placeholder.jpg", 
    #                  paste0("thumbImage: https://cbtooltest.marisa.psu.edu/images/scaled_156_250/TOOLID_", 
    #                         trimws(toolRec$`Tool ID`), "_ScreenCapture-1.png"), toolPage)
    
    listTools$img <- paste0(el, '<img src="/images/scaled_156_250/TOOLID_', 
                            trimws(toolRec$`Tool ID`), 
                            '_ScreenCapture-1.png" style="max-height:156px;max-width:250;" align="right"/>', el)
    
    toolName <- paste0("## ", toolRec$`Tool Name`, el)
    
    toolImg <- paste0('<img src="/images/scaled_250_400/TOOLID_', 
                      trimws(toolRec$`Tool ID`), 
                      '_ScreenCapture-1.png" style="max-height:250px;max-width:400;" align="right"/>', el)
    
    ##extract software requirements, for search terms on site
    # IF software requirements exist
    ## THIS IS NOT currently available on the site
    if(!is.na(toolRec$`Software Requirements-software`)){
      softReqs <- strsplit(toolRec$`Software Requirements-software`, "; |;  |\r\n")[[1]]
      toolSoftReqs <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, softReqs=softReqs)
    }else{
      toolSoftReqs <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, softReqs="None")
    }
    # BIND the tags data.frame to the GLOBAL searchSoftReqs list
    # searchSoftReqs <<- rbind.data.frame(searchSoftReqs, toolSoftReqs)
    
    ##tool developer
    # Set the developer based on if multiple developers exist
    if(length(splitDevs)==1){
      toolDev <- paste0("**Developed By:** ", splitDevs, el)
    }else{
      toolDev <- paste0("**Developed By:** ", 
                        paste0(paste(splitDevs[1:(length(splitDevs)-1)], 
                                     collapse=", "), ", and ", 
                               splitDevs[length(splitDevs)]), el)
    }
    
    ##tool description
    # Set the summary description if one exists
    if(!is.na(toolRec$`Description-About`)){
      toolDes <- paste0("**Summary:** ", toolRec$`Description-About`, el)
    }else{
      toolDes <- paste0("**Summary:** ", "Not Available", el)
    }
    ##########################################################################
    # TOOL SEARCH
    ##########################################################################

# Tool function -----------------------------------------------------------

    # Extract the "Tool Functions" for the tool for use in the "Tool Function search engine"
    # convert the codes into real variables
    ##Why tool is useful
    toolPurposes <- toolRec[grep("Purpose-Tool Function", names(toolRec))]
    toolPurposes <- toolPurposes[which(!is.na(toolPurposes))]
    purposeVars <- sapply(strsplit(names(toolPurposes), "Purpose-Tool Function-"), "[[", 2)
      
    if(length(purposeVars)>0){
      purposeVars <- ifelse(purposeVars == "past", "View Past/Current Conditions", purposeVars)
      purposeVars <- ifelse(purposeVars == "futr", "View Future Projections", purposeVars)
      purposeVars <- ifelse(purposeVars == "risk", "Identify Vulnerabilities", purposeVars)
      purposeVars <- ifelse(purposeVars == "adpt", "Adaptation Planning", purposeVars)
      purposeVars <- ifelse(purposeVars == "mtgt", "Climate Mitigation Planning", purposeVars)
      purposeVars <- ifelse(purposeVars == "prcs", "Process Support", purposeVars)
      purposeVars <- ifelse(purposeVars == "eng", "Engagement", purposeVars)
      purposeVars <- ifelse(purposeVars == "com", "Citizen Science", purposeVars)
      purposeVars <- ifelse(purposeVars == "coll", "Toolkit/Tool Collection", purposeVars)
      # if("past" %in% purposeVars){
      #   purposeVars[which(purposeVars=="past")] <- "View Past/Current Conditions"
      # }
      # if("futr" %in% purposeVars){
      #   purposeVars[which(purposeVars=="futr")] <- "View Future Projections"
      # }
      # if("risk" %in% purposeVars){
      #   purposeVars[which(purposeVars=="risk")] <- "Identify Vulnerabilities"
      # }
      # if("adpt" %in% purposeVars){
      #   purposeVars[which(purposeVars=="adpt")] <- "Adaptation Planning"
      # }
      # if("mtgt" %in% purposeVars){
      #   purposeVars[which(purposeVars=="mtgt")] <- "Climate Mitigation Planning"
      # }
      # if("prcs" %in% purposeVars){
      #   purposeVars[which(purposeVars=="prcs")] <- "Process Support"
      # }
      # if("eng" %in% purposeVars){
      #   purposeVars[which(purposeVars=="eng")] <- "Engagement"
      # }
      # if("com" %in% purposeVars){
      #   purposeVars[which(purposeVars=="com")] <- "Citizen Science"
      # }
      toolPurposes <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolFunct=purposeVars)
      # Set the tool functions (purposes) as a tool attribute
      tooljson$toolfunction = purposeVars
    }else{ # Set to none if no function exists
      toolPurposes <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolFunct="None")
      # Set the tool functions (purposes) as a tool attribute
      tooljson$toolfunction = "None"
    }
    # BIND the tags data.frame to the GLOBAL search* list
    # searchToolFunct <<- rbind.data.frame(searchToolFunct, toolPurposes)
    
# Topic filter ------------------------------------------------------------

    ##topic filters - both main and sub-filters
    toolSubFilts <- vector(mode="character")
    
    # Extract all climate related topic filters
    climFilters <- toolRec[grep("Topic Filters-Climate", names(toolRec))]
    climFilters <- climFilters[which(!is.na(climFilters))]
    if(length(climFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(climFilters), "Topic Filters-Climate-"), "[[", 2))
    }
    # Extract all ecosystem related topic filters
    ecoFilters <- toolRec[grep("Topic Filters-Ecosystems", names(toolRec))]
    ecoFilters <- ecoFilters[which(!is.na(ecoFilters))]
    if(length(ecoFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(ecoFilters), "Topic Filters-Ecosystems-"), "[[", 2))
    }
    #agFilters <- toolRec[grep("Topic Filters-Agriculture", names(toolRec))]
    #agFilters <- agFilters[which(!is.na(agFilters))]
    #if(length(agFilters)>0){
    #  toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(agFilters), "Topic Filters-Agriculture-"), "[[", 2))
    #}
    # Extract all Agriculture / Built Environment related topic filters
    builtFilters <- toolRec[grep("Topic Filters-Agriculture / Built Environment", names(toolRec))]
    builtFilters <- builtFilters[which(!is.na(builtFilters))]
    if(length(builtFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(builtFilters), "Topic Filters-Agriculture / Built Environment-"), "[[", 2))
    }
    # Extract all society related topic filters
    socFilters <- toolRec[grep("Topic Filters-Society", names(toolRec))]
    socFilters <- socFilters[which(is.na(socFilters)==F)]
    if(length(socFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(socFilters), "Topic Filters-Society-"), "[[", 2))
    }
    #floodFilters <- toolRec[grep("Topic Filters-Flooding", names(toolRec))]
    #floodFilters <- floodFilters[which(!is.na(floodFilters))]
    #if(length(floodFilters)>0){
    #  toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(floodFilters), "Topic Filters-Flooding-"), "[[", 2))
    #}
    # Extract all water / flooding related topic filters
    waterFilters <- toolRec[grep("Topic Filters-Water / Flooding", names(toolRec))]
    waterFilters <- waterFilters[which(!is.na(waterFilters))]
    if(length(waterFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(waterFilters), "Topic Filters-Water / Flooding-"), "[[", 2))
    }
     # Remove erosion as a category from water/flooding
    ##remove errosion as a category
    if("ersn" %in% toolSubFilts){
      toolSubFilts <- toolSubFilts[-grep("ersn", toolSubFilts)]
    }
    # Extract all coastal related topic filters
    coastFilters <- toolRec[grep("Topic Filters-Coastal / Inland", names(toolRec))]
    coastFilters <- coastFilters[which(is.na(coastFilters)==F)]
    if(length(coastFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(coastFilters), "Topic Filters-Coastal / Inland-"), "[[", 2))
    }
    
    # Classify each sub topic into a top level topic for the topic filter
    ##set up for output
    toolFilts <- toolSubFilts
    toolFiltsName <- toolSubFilts
    
    if(length(toolSubFilts)>0){
      if("crbn" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="crbn")
        toolSubFilts[varInd] <- "Carbon Emissions"
        toolFilts[varInd] <- "clim"
        toolFiltsName[varInd] <- "Weather and Climate"
      }
      if("temp" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="temp")
        toolSubFilts[varInd] <- "Temperature"
        toolFilts[varInd] <- "clim"
        toolFiltsName[varInd] <- "Weather and Climate"
      }
      if("prcp" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="prcp")
        toolSubFilts[varInd] <- "Precipitation"
        toolFilts[varInd] <- "clim"
        toolFiltsName[varInd] <- "Weather and Climate"
      }
      if("slr" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="slr")
        toolSubFilts[varInd] <- "Sea Level Rise"
        toolFilts[varInd] <- "clim"
        toolFiltsName[varInd] <- "Weather and Climate"
      }
      if("strm" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="strm")
        toolSubFilts[varInd] <- "Storms and Hurricanes"
        toolFilts[varInd] <- "clim"
        toolFiltsName[varInd] <- "Weather and Climate"
      }
      if("drght" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="drght")
        toolSubFilts[varInd] <- "Drought"
        toolFilts[varInd] <- "clim"
        toolFiltsName[varInd] <- "Weather and Climate"
      }
      if("wet" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="wet")
        toolSubFilts[varInd] <- "Wetlands"
        toolFilts[varInd] <- "eco"
        toolFiltsName[varInd] <- "Ecosystems"
      }
      if("sav" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="sav")
        toolSubFilts[varInd] <- "Submerged Aquatic Vegitation"
        toolFilts[varInd] <- "eco"
        toolFiltsName[varInd] <- "Ecosystems"
      }
      if("shln" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="shln")
        toolSubFilts[varInd] <- "Shorelines and Erosion"
        toolFilts[varInd] <- "eco"
        toolFiltsName[varInd] <- "Ecosystems"
      }
      if("soil" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="soil")
        toolSubFilts[varInd] <- "Soils"
        toolFilts[varInd] <- "eco"
        toolFiltsName[varInd] <- "Ecosystems"
      }
      if("rest" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="rest")
        toolSubFilts[varInd] <- "Restoration"
        toolFilts[varInd] <- "eco"
        toolFiltsName[varInd] <- "Ecosystems"
      }
      if("val" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="val")
        toolSubFilts[varInd] <- "Eco Services Valuation"
        toolFilts[varInd] <- "eco"
        toolFiltsName[varInd] <- "Ecosystems"
      }
      if("agr" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="agr")
        toolSubFilts[varInd] <- "Agriculture"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("for" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="for")
        toolSubFilts[varInd] <- "Forestry"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("fish" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="fish")
        toolSubFilts[varInd] <- "Fisheries and Aquaculture"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("ufor" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="ufor")
        toolSubFilts[varInd] <- "Urban Forestry"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("plan" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="plan")
        toolSubFilts[varInd] <- "Planning / Land Use"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("bldg" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="bldg")
        toolSubFilts[varInd] <- "Buildings / Infrastructure"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("trnsp" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="trnsp")
        toolSubFilts[varInd] <- "Transportation"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("econ" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="econ")
        toolSubFilts[varInd] <- "Economic Assessment"
        toolFilts[varInd] <- "blt"
        toolFiltsName[varInd] <- "Agriculture / Built Environment"
      }
      if("socv" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="socv")
        toolSubFilts[varInd] <- "Social Vulnerability"
        toolFilts[varInd] <- "soc"
        toolFiltsName[varInd] <- "Society"
      }
      if("cult" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="cult")
        toolSubFilts[varInd] <- "Cultural Resources"
        toolFilts[varInd] <- "soc"
        toolFiltsName[varInd] <- "Society"
      }
      if("hlth" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="hlth")
        toolSubFilts[varInd] <- "Public Health"
        toolFilts[varInd] <- "soc"
        toolFiltsName[varInd] <- "Society"
      }
      if("air" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="air")
        toolSubFilts[varInd] <- "Air Quality"
        toolFilts[varInd] <- "soc"
        toolFiltsName[varInd] <- "Society"
      }
      if("high" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="high")
        toolSubFilts[varInd] <- "High Tide / Recurrent Flooding"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      if("surge" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="surge")
        toolSubFilts[varInd] <- "Storm Surge"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      if("salt" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="salt")
        toolSubFilts[varInd] <- "Saltwater Intrusion"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      if("inlnd" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="inlnd")
        toolSubFilts[varInd] <- "Inland Flooding"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      #if("ersn" %in% toolSubFilts){
      #  toolSubFilts[which(toolSubFilts=="ersn")] <- "Erosion"
      #}
      if("wl" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="wl")
        toolSubFilts[varInd] <- "Water Level"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      if("qual" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="qual")
        toolSubFilts[varInd] <- "Water Quality"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      if("flow" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="flow")
        toolSubFilts[varInd] <- "Stream Flow"
        toolFilts[varInd] <- "fld"
        toolFiltsName[varInd] <- "Water / Flooding"
      }
      if("nonc" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="nonc")
        toolSubFilts[varInd] <- "Noncoastal/ Inland"
        toolFilts[varInd] <- "cstl"
        toolFiltsName[varInd] <- "Coastal / Inland"
      }
      if("cstl" %in% toolSubFilts){
        varInd <- which(toolSubFilts=="cstl")
        toolSubFilts[varInd] <- "Coastal"
        toolFilts[varInd] <- "cstl"
        toolFiltsName[varInd] <- "Coastal / Inland"
      }
      toolFilters <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, topFilter=toolFilts, topFilterNam=toolFiltsName, subFilter=toolSubFilts)
      # Set the tool filters as a tool attribute
      tooljson$subfilter = toolSubFilts
    }else{
      toolFilters <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, topFilter="None", topFilterNam="None", subFilter="None")
      # Set the tool filters as a tool attribute
      tooljson$subfilter = "None"
    }
    #searchSubFilters <<- rbind.data.frame(searchSubFilters, toolFilters)
    # BIND the tags data.frame to the GLOBAL search* list
    # searchTopFilters <<- rbind.data.frame(searchTopFilters, toolFilters)
    
    #if(is.na(toolRec$`Climate Relevance`)==F){
    #  toolUse <- paste0("**Relevance:** ", toolRec$`Climate Relevance`, el)
    #}else{
    #  toolUse <- paste0("**Relevance:** ", "Not Available", el)
    #}
    
    ##tool audience
    #if(is.na(toolRec$`Target Audience`)==F){
    #  checkAud <- strsplit(toolRec$`Target Audience`, ";")[[1]]
    #  toolAud <- paste0("**Target Audience:** ", paste(checkAud, collapse=", "), el)
    #}else{
    #  toolAud <- paste0("**Target Audience:** ", "Not Available", el)
    #}
    
    
    #toolRec <- splitByToolID[[12]]
    #toolRec <- splitByToolID[[45]]
    #toolRec <- splitByToolID[[5]]
    #toolRec <- splitByToolID[[21]]
    #toolRec <- splitByToolID[[35]]
    #toolRec <- splitByToolID[[70]]
    #toolRec <- splitByToolID[[77]]
    #toolRec <- splitByToolID[[24]]
    #toolRec <- splitByToolID[[8]]
    
# Geographic Scope --------------------------------------------------------
    ## tool geographic scope
    ### LOCALITY
    if(toolRec$`Geographic Scope-Scope`=="Locality"){
      
      # Convert the string of localities to a list of localities
      collectLocs <- strsplit(toolRec$`Geographic Scope-Locality`, "; ")[[1]]
      
      if(length(collectLocs) == 1 && collectLocs == "Chesapeake Bay"){
        subTab <- scTab[scTab$state %in% c("VA","MD"),]
        subTab <- subTab[subTab$coastal=="Y",]
        # whichnotCity <- grep(" City", subTab$cntyName, invert = TRUE)
        # subTab$cntyName[whichnotCity] <- paste0(subTab$cntyName[whichnotCity], " County")
        locsNoState <- subTab$cntyName
        findState <- subTab$state
        
      } else {
        # Remove the state from the location (locality, state)
        locsNoState <- sapply(strsplit(collectLocs, ", "), "[[", 1)
        # Isolate the state from the list of localities
        findState <- sapply(strsplit(collectLocs, ", "), "[[", 2)
      }
      
      # Change Washington to District of Columbia
      ind_notcounty <- grep("County", locsNoState, invert = TRUE)
      locsNoState[ind_notcounty] <- gsub("Washington", "District of Columbia", locsNoState[ind_notcounty])
      
      # Add state abbreviation to county/city name (exclude DC if it exists)
      if(any(locsNoState == "District of Columbia")){
        dc.ind <- which(locsNoState == "District of Columbia")
        locationlist <- paste0(locsNoState[-dc.ind], ", ", findState[-dc.ind])
      } else {
        locationlist <- paste0(locsNoState, ", ", findState)
      }
      
      # Convert the list of states from abbreviations to the full name. Use a look up table.
      collectSts <- sapply(findState, stateLookUp, statecodes = statefips)
      
      # Set the location as a tool attribute
      tooljson$location = c(unique(collectSts), locationlist)
      
      # toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
      #                       toolState=collectSts, toolLoc=locsNoState, 
      #                       coastal=toolRec$`Geographic Scope-Coastal`)
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
                            toolState=collectSts, toolLoc=locsNoState, 
                            coastal=toolRec$`Geographic Scope-Only Coastal`)
      toolScope <- paste0("**Geographic Coverage**", el, el, "* ", toolRec$`Geographic Scope-Locality`, el)
      
      ### Coastal (only coastal part of state)
    } else if(toolRec$`Geographic Scope-Scope`=="Coastal"){

      collectSts <- strsplit(toolRec$`Geographic Scope-State`, "; ")[[1]]
      # Grab only the coastal counties
      subTab <- scTab[scTab$state %in% collectSts,]
      subTab <- subTab[subTab$coastal=="Y",]
      # whichnotCity <- grep(" City", subTab$cntyName, invert = TRUE)
      # subTab$cntyName[whichnotCity] <- paste0(subTab$cntyName[whichnotCity], " County")
      locsNoState <- subTab$cntyName
      collectSts <- subTab$state
      
      # Add state abbreviation to county/city name
      locationlist <- paste0(locsNoState, ", ", collectSts)

      # Convert the list of states from abbreviations to the full name. Use a look up table.
      collectSts <- sapply(collectSts, stateLookUp, statecodes = statefips)
      
      # Set the location as a tool attribute
      tooljson$location = c(unique(collectSts), locationlist)

      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt,
                            toolState=collectSts, toolLoc=locsNoState,
                            coastal=toolRec$`Geographic Scope-Only Coastal`)
      toolScope <- paste0("**Geographic Coverage**", el, el, "* Coastal ",
                          paste(unique(collectSts), collapse="; "), el)
      
      ### STATE (whole state)
    } else if(toolRec$`Geographic Scope-Scope`=="State"){
      collectSts <- strsplit(toolRec$`Geographic Scope-State`, "; ")[[1]]
      
      # # if it is coastal then grab only the coastal counties
      # locsNoState <- NA ## NEED TO CREATE AN ELSE OPTION FOR NONE COASTAL COUNTIES
      # if(!is.na(toolRec$`Geographic Scope-Coastal`) & toolRec$`Geographic Scope-Coastal`=="x"){
      #   subTab <- scTab[scTab$state %in% collectSts,]
      #   subTab <- subTab[subTab$coastal=="Y",]
      #   # whichnotCity <- grep(" City", subTab$cntyName, invert = TRUE)
      #   # subTab$cntyName[whichnotCity] <- paste0(subTab$cntyName[whichnotCity], " County")
      #   locsNoState <- subTab$cntyName
      #   collectSts <- subTab$state
      #   
      #   # Convert the list of states from abbreviations to the full name. Use a look up table.
      #   collectSts <- sapply(collectSts, stateLookUp, statecodes = statefips)
      # } else {
        # library(tidycensus)
        data(fips_codes) # download state/county info
        # list only the Marisa state counties
        marisaSts <- collectSts[collectSts %in% unique(scTab$state)]
        localities <- fips_codes[fips_codes$state %in% marisaSts, ]
        locsNoState <-str_to_title(localities$county)
        # Convert the list of states from abbreviations to the full name.
        collectSts <- localities$state_name
        collectStsAbb <- localities$state
      # }
        
        # Add state abbreviation to county/city name
        locationlist <- paste0(locsNoState, ", ", collectStsAbb)
        # Set the location as a tool attribute
        tooljson$location = c(unique(collectSts), locationlist)
      
      # toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
      #                       toolState=collectSts, toolLoc=locsNoState, 
      #                       coastal=toolRec$`Geographic Scope-Coastal`)
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
                            toolState=collectSts, toolLoc=locsNoState, 
                            coastal=toolRec$`Geographic Scope-Only Coastal`)
      toolScope <- paste0("**Geographic Coverage**", el, el, "* ", 
                          paste(unique(collectSts), collapse="; "), el)
      
      ### NATIONAL
    } else if(toolRec$`Geographic Scope-Scope`=="National"){
      
      # locsNoState <- NA ## NEED TO CREATE AN ELSE OPTION FOR NONE COASTAL COUNTIES
      if(!is.na(toolRec$`Geographic Scope-Only Coastal`) & toolRec$`Geographic Scope-Only Coastal`=="x"){
        
        # We only need to parse the states in the Mid-Atlantic
        collectSts <- c("DE", "MD", "NJ", "NY", "PA", "VA", "DC")
        
        subTab <- scTab[scTab$state %in% collectSts,]
        subTab <- subTab[subTab$coastal=="Y",]
        # whichnotCity <- grep(" City", subTab$cntyName, invert = TRUE)
        # subTab$cntyName[whichnotCity] <- paste0(subTab$cntyName[whichnotCity], " County")
        locsNoState <- subTab$cntyName
        collectSts <- subTab$state
        
        # Add state abbreviation to county/city name (exclude DC if it exists)
        if(any(locsNoState == "District of Columbia")){
          dc.ind <- which(locsNoState == "District of Columbia")
          locationlist <- paste0(locsNoState[-dc.ind], ", ", collectSts[-dc.ind])
        } else {
          locationlist <- paste0(locsNoState, ", ", collectSts)
        }

        # Convert the list of states from abbreviations to the full name. Use a look up table.
        collectSts <- sapply(collectSts, stateLookUp, statecodes = statefips)  
        
        # Set the location as a tool attribute
        tooljson$location = c(unique(collectSts), locationlist)
      
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt,
                              toolState=collectSts, toolLoc=locsNoState,
                              coastal=toolRec$`Geographic Scope-Only Coastal`)
        toolScope <- paste0("**Geographic Coverage**", el, el, "* Coastal Contiguous United States", el)
      } else{
        
        # We only need to parse the states in the Mid-Atlantic
        collectSts <- c("DE", "MD", "NJ", "NY", "PA", "VA", "DC")
        subTab <- scTab[scTab$state %in% collectSts,]
        locsNoState <- subTab$cntyName
        collectSts <- subTab$state
        
        # Add state abbreviation to county/city name (exclude DC if it exists)
        if(any(locsNoState == "District of Columbia")){
          dc.ind <- which(locsNoState == "District of Columbia")
          locationlist <- paste0(locsNoState[-dc.ind], ", ", collectSts[-dc.ind])
        } else {
          locationlist <- paste0(locsNoState, ", ", collectSts)
        }
        
        # Convert the list of states from abbreviations to the full name. Use a look up table.
        collectSts <- sapply(collectSts, stateLookUp, statecodes = statefips)  
        
        # Set the location as a tool attribute
        tooljson$location = c(unique(collectSts), locationlist)
        
      # toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
      #                       toolState="All", toolLoc="All", 
      #                       coastal=toolRec$`Geographic Scope-Coastal`)
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
                            toolState="All", toolLoc="All", 
                            coastal=toolRec$`Geographic Scope-Only Coastal`)
      toolScope <- paste0("**Geographic Coverage**", el, el, "* Contiguous United States", el)
      }
      
      ### OTHER OR ANYWHERE
    } else{
      # locsNoState <- NA ## NEED TO CREATE AN ELSE OPTION FOR NONE COASTAL COUNTIES
      if(!is.na(toolRec$`Geographic Scope-Only Coastal`) & toolRec$`Geographic Scope-Only Coastal`=="x"){
        
        # We only need to parse the states in the Mid-Atlantic
        collectSts <- c("DE", "MD", "NJ", "NY", "PA", "VA", "DC")
        
        subTab <- scTab[scTab$state %in% collectSts,]
        subTab <- subTab[subTab$coastal=="Y",]
        # whichnotCity <- grep(" City", subTab$cntyName, invert = TRUE)
        # subTab$cntyName[whichnotCity] <- paste0(subTab$cntyName[whichnotCity], " County")
        locsNoState <- subTab$cntyName
        collectSts <- subTab$state
        
        # Add state abbreviation to county/city name (exclude DC if it exists)
        if(any(locsNoState == "District of Columbia")){
          dc.ind <- which(locsNoState == "District of Columbia")
          locationlist <- paste0(locsNoState[-dc.ind], ", ", collectSts[-dc.ind])
        } else {
          locationlist <- paste0(locsNoState, ", ", collectSts)
        }

        # Convert the list of states from abbreviations to the full name. Use a look up table.
        collectSts <- sapply(collectSts, stateLookUp, statecodes = statefips)
        
        # Set the location as a tool attribute
        tooljson$location = c(unique(collectSts), locationlist)

        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt,
                              toolState=collectSts, toolLoc=locsNoState,
                              coastal=toolRec$`Geographic Scope-Only Coastal`)
        toolScope <- paste0("**Geographic Coverage**", el, el, "* Applicable Anywhere Coastal", el)
      } else{
        
        # We only need to parse the states in the Mid-Atlantic
        collectSts <- c("DE", "MD", "NJ", "NY", "PA", "VA", "DC")
        subTab <- scTab[scTab$state %in% collectSts,]
        locsNoState <- subTab$cntyName
        collectSts <- subTab$state
        
        # Add state abbreviation to county/city name (exclude DC if it exists)
        if(any(locsNoState == "District of Columbia")){
          dc.ind <- which(locsNoState == "District of Columbia")
          locationlist <- paste0(locsNoState[-dc.ind], ", ", collectSts[-dc.ind])
        } else {
          locationlist <- paste0(locsNoState, ", ", collectSts)
        }
        
        # Convert the list of states from abbreviations to the full name. Use a look up table.
        collectSts <- sapply(collectSts, stateLookUp, statecodes = statefips)  
        
        # Set the location as a tool attribute
        tooljson$location = c(unique(collectSts), locationlist)
        
      # toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
      #                       toolState="Gen", toolLoc="Gen", 
      #                       coastal=toolRec$`Geographic Scope-Coastal`)
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, 
                            toolState="Gen", toolLoc="Gen", 
                            coastal=toolRec$`Geographic Scope-Only Coastal`)
      toolScope <- paste0("**Geographic Coverage**", el, el, "* Applicable Anywhere", el)
      }
    }
    
    #If its a tool collection then list the tools instead of tags
    if(toolRec$`Tool ID` %in% toolCol){
      collectionIds <- sapply(splitByCol, "[[", 2)
      collectInd <- grep(toolRec$`Tool ID`, collectionIds)
      collectdf <- splitByCol[[collectInd]]
      collectdf <- collectdf[-which(collectdf$`Tool ID` == toolRec$`Tool ID`), ]
      
      ##tool strengths
      # Set the strengths as the tags
      if(nrow(collectdf) > 0){
        toolList <- paste0("[", collectdf$`Tool Name`, "](/collection/page-tool",
                           trimws(collectdf$`Tool ID`), "/){:target='blank'}")
        
        toolStrs <- paste0("**Available Tools**", el, el, paste("* ", toolList, collapse=el), el)
      }else{
        toolStrs <- paste0("**Available Tools**", el, "Not Available", el)
      }
      
    } else { # A regular page
      
      ##tool strengths
      # Set the strengths as the tags
      if(length(tags) > 0){
        toolStrs <- paste0("**Tags**", el, el, paste("* ", tags, collapse=el), el)
      }else{
        toolStrs <- paste0("**Tags**", el, "Not Available", el)
      }
    }
    
    ##tool weaknesses
    #if(is.na(toolRec$Limitations)==F){
    #  checkLim <- strsplit(toolRec$Limitations, ";")[[1]]
    #  toolLims <- paste0("**Limitations:** ", paste(checkLim, collapse=", "), el)
    #}else{
    #  toolLims <- paste0("**Limitations:** ", "Not Available", el)
    #}
    
    ##tool URL
    # Set the URL to "Get This Tool"
    if(!is.na(toolRec$URL)){
      #toolURL <- paste0('<a href="', toolRec$URL, '" target="_blank">Get This Tool</a>', el)  ##embedded link in Get This Tool
      toolURL <- paste0('**Get This Tool:** [', toolRec$URL, "](", toolRec$URL, "){:target='blank'}", el)
    }else{
      #toolURL <- paste0("[Get This Tool] ", "Not Available")
      toolURL <- paste0("", el)
    }
    
    ##tool cost
    # Set the cost
    if(!is.na(toolRec$`Software Requirements-Cost`)){
      toolCost <- paste0("**Cost**", el, el, "* ", toolRec$`Software Requirements-Cost`, el)
    }else{
      toolCost <- paste0("**Cost**", el, el, "* Unknown", el)
    }
    
    ##tool skill lvl
    # Set the skill level
    if(!is.na(toolRec$`Effort / Skill-Skill`)){
      toolSkill <- paste0("**Skill Level**", el, el, "* ", toolRec$`Effort / Skill-Skill`)
    }else{
      toolSkill <- paste0("**Skill Level**", el, "* Unknown")
    }
    
    ## Bring the page together
    #toolPage <- paste(toolPage, pageTitle, toolDev, "#### Tool Summary-", toolDes, toolUse, toolScope, toolAud, toolStrs, toolLims, toolURL, sep=el)
    #toolPage <- paste(toolPage, toolDev, "#### Tool Summary", toolDes, toolUse, toolScope, toolAud, toolStrs, toolLims, toolURL, sep=el)
    toolPage <- paste(toolPage, toolName, toolImg, toolDev, toolDes, toolURL, 
                      toolScope, toolStrs, toolCost, toolSkill, sep=el)
    conTool <- file(writeTool) # Connect to the file
    writeLines(toolPage, conTool)
    close(conTool) # Make sure to close the connection to prevent errors or warnings
    return(list(searchGeoScope = toolGeo, searchTopFilters = toolFilters, 
                searchToolFunct = toolPurposes, searchSoftReqs = toolSoftReqs,
                searchTags = toolTags, tooljson = tooljson, listTools = listTools))
  }
}
##########################################################################
##########################################################################

##########################################################################
##########################################################################

##########################################################################
##########################################################################

##########################################################################
##########################################################################





