##site generating functions
##########################################################################
##########################################################################
formatStarStrings <- function(str){
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
generateToolPage <- function(toolRec, tempPage, el, siteDir, scTab, updateContent=T){
  #################
  #toolRec <- splitByToolID[[2]]
  #toolRec <- splitByToolID[[4]]
  #toolRec <- splitByToolID[[5]]
  #toolRec <- splitByToolID[[7]]
  #toolRec <- splitByToolID[[12]]
  #toolRec <- splitByToolID[[45]]
  #toolRec <- splitByToolID[[64]]
  #toolRec <- splitByToolID[[100]]
  #tempPage <- templatePage
  #el <- "\n"
  #siteDir <- toolsDir
  #scTab <- stcntyTab
  #updateContent <- T
  #################
  ##check the developers, to make sure the directories are available and are set up properly
  if(is.na(toolRec$Developer)==T){
    toolRec$Developer <- "Unknown"
  }
  ##if there is more than one developer, split the developers into a vector
  splitDevs <- strsplit(toolRec$Developer, "; ")[[1]]
  toolIDtxt <- paste0("page-tool", trimws(toolRec$`Tool ID`))  ##the name of the tool page within the site
  #writeTool <- paste0(siteDir, splitDevs[1], "/", toolIDtxt, ".md")  ##the file in which the tool information will be written to, assumes first developer is the most important
  toolRec$`Tool Name` <- gsub(el, " ", toolRec$`Tool Name`) ##remove any cariage returns from the tool name
  #toolDir <- paste0(siteDir, gsub("/|:", "-", toolRec$`Tool Name`), "/")
  writeTool <- paste0(siteDir, toolIDtxt, ".md")
  
  if(file.exists(writeTool)==F | updateContent==T){
    ##provide new page title
    toolPage <- gsub("page-template", toolRec$`Tool Name`, tempPage)
    ##update date
    toolPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), toolPage)
    ##change description
    if(is.na(toolRec$`Purpose-Description`)==F){
      toolPage <- gsub("a page", toolRec$`Purpose-Description`, toolPage)
    }else{
      toolPage <- gsub("a page", toolRec$`Tool Name`, toolPage)
    }
    ##add  tags to the header
    ##and extract tags to add to search objects
    if(is.na(toolRec$`Description-Strengths`)==F){
      tags <- strsplit(toolRec$`Description-Strengths`, "; |;  ")[[1]]
      toolPage <- gsub('\"tags\"', paste(paste0('\"', unique(tags), '\"'), collapse=", "), toolPage)
      toolTags <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, tag=tags)
      searchTags <<- rbind.data.frame(searchTags, toolTags)
    }else{
      #toolPage <- gsub('\"tags\"', '\"placeholder\"', toolPage)
      toolPage <- gsub('\"tags\"', '', toolPage)
    }
    
    ##replace the placeholder image with an actual screenshot of the tool, if available
    toolPage <- gsub("pageImage: https://www.887theriver.ca/wp-content/uploads/2017/07/placeholder.jpg", 
                     paste0("pageImage: https://cbtooltest.marisa.psu.edu/images/scaled_250_400/TOOLID_", trimws(toolRec$`Tool ID`), "_ScreenCapture-1.png"), toolPage)
    toolPage <- gsub("thumbImage: https://www.887theriver.ca/wp-content/uploads/2017/07/placeholder.jpg", 
                     paste0("thumbImage: https://cbtooltest.marisa.psu.edu/images/scaled_156_250/TOOLID_", trimws(toolRec$`Tool ID`), "_ScreenCapture-1.png"), toolPage)
    
    ##extract software requirements, for search terms on site
    if(is.na(toolRec$`Software Requirements-software`)==F){
      softReqs <- strsplit(toolRec$`Software Requirements-software`, "; |;  |\r\n")[[1]]
      toolSoftReqs <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, softReqs=softReqs)
    }else{
      toolSoftReqs <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, softReqs="None")
    }
    searchSoftReqs <<- rbind.data.frame(searchSoftReqs, toolSoftReqs)
    
    ##tool developer
    if(length(splitDevs)==1){
      toolDev <- paste0("Developed By: ", splitDevs, el)
    }else{
      toolDev <- paste0("Developed By: ", paste0(paste(splitDevs[1:(length(splitDevs)-1)], collapse=", "), ", and ", splitDevs[length(splitDevs)]), el)
    }
    
    ##tool description
    if(is.na(toolRec$`Description-About`)==F){
      toolDes <- paste0("**Summary:** ", toolRec$`Description-About`, el)
    }else{
      toolDes <- paste0("**Summary:** ", "Not Available", el)
    }
    
    ##Why tool is useful
    toolPurposes <- toolRec[grep("Purpose-Tool Function-", names(toolRec))]
    toolPurposes <- toolPurposes[which(is.na(toolPurposes)==F)]
    purposeVars <- sapply(strsplit(names(toolPurposes), "Purpose-Tool Function-"), "[[", 2)
    if(length(purposeVars)>0){
      if("past" %in% purposeVars){
        purposeVars[which(purposeVars=="past")] <- "View Past/Current Conditions"
      }
      if("futr" %in% purposeVars){
        purposeVars[which(purposeVars=="futr")] <- "View Future Projections"
      }
      if("risk" %in% purposeVars){
        purposeVars[which(purposeVars=="risk")] <- "Identify Vulnerabilities"
      }
      if("adpt" %in% purposeVars){
        purposeVars[which(purposeVars=="adpt")] <- "Adaptation Planning"
      }
      if("mtgt" %in% purposeVars){
        purposeVars[which(purposeVars=="mtgt")] <- "Climate Mitigation Planning"
      }
      if("prcs" %in% purposeVars){
        purposeVars[which(purposeVars=="prcs")] <- "Process Support"
      }
      if("eng" %in% purposeVars){
        purposeVars[which(purposeVars=="eng")] <- "Engagement"
      }
      if("com" %in% purposeVars){
        purposeVars[which(purposeVars=="com")] <- "Citizen Science"
      }
      toolPurposes <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolFunct=purposeVars)
    }else{
      toolPurposes <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolFunct="None")
    }
    searchToolFunct <<- rbind.data.frame(searchToolFunct, toolPurposes)
    
    ##topic filters - both main and sub-filters
    toolSubFilts <- vector(mode="character")
    climFilters <- toolRec[grep("Topic Filters-Climate-", names(toolRec))]
    climFilters <- climFilters[which(is.na(climFilters)==F)]
    if(length(climFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(climFilters), "Topic Filters-Climate-"), "[[", 2))
    }
    ecoFilters <- toolRec[grep("Topic Filters-Ecosystems-", names(toolRec))]
    ecoFilters <- ecoFilters[which(is.na(ecoFilters)==F)]
    if(length(ecoFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(ecoFilters), "Topic Filters-Ecosystems-"), "[[", 2))
    }
    #agFilters <- toolRec[grep("Topic Filters-Agriculture-", names(toolRec))]
    #agFilters <- agFilters[which(is.na(agFilters)==F)]
    #if(length(agFilters)>0){
    #  toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(agFilters), "Topic Filters-Agriculture-"), "[[", 2))
    #}
    builtFilters <- toolRec[grep("Topic Filters-Agriculture / Built Environment-", names(toolRec))]
    builtFilters <- builtFilters[which(is.na(builtFilters)==F)]
    if(length(builtFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(builtFilters), "Topic Filters-Agriculture / Built Environment-"), "[[", 2))
    }
    socFilters <- toolRec[grep("Topic Filters-Society-", names(toolRec))]
    socFilters <- socFilters[which(is.na(socFilters)==F)]
    if(length(socFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(socFilters), "Topic Filters-Society-"), "[[", 2))
    }
    #floodFilters <- toolRec[grep("Topic Filters-Flooding-", names(toolRec))]
    #floodFilters <- floodFilters[which(is.na(floodFilters)==F)]
    #if(length(floodFilters)>0){
    #  toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(floodFilters), "Topic Filters-Flooding-"), "[[", 2))
    #}
    waterFilters <- toolRec[grep("Topic Filters-Water / Flooding-", names(toolRec))]
    waterFilters <- waterFilters[which(is.na(waterFilters)==F)]
    if(length(waterFilters)>0){
      toolSubFilts <- c(toolSubFilts, sapply(strsplit(names(waterFilters), "Topic Filters-Water / Flooding-"), "[[", 2))
    }
    
    ##remove errosion as a category
    if("ersn" %in% toolSubFilts){
      toolSubFilts <- toolSubFilts[-grep("ersn", toolSubFilts)]
    }
    
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
      toolFilters <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, topFilter=toolFilts, topFilterNam=toolFiltsName, subFilter=toolSubFilts)
    }else{
      toolFilters <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, topFilter="None", topFilterNam="None", subFilter="None")
    }
    #searchSubFilters <<- rbind.data.frame(searchSubFilters, toolFilters)
    searchTopFilters <<- rbind.data.frame(searchTopFilters, toolFilters)
    
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
    ##tool geographic scope
    require(openintro)
    if(toolRec$`Geographic Scope-Scope`=="Locality" & is.na(toolRec$`Geographic Scope-Locality`)==T){
      subTab <- scTab[scTab$state=="VA",]
      whichCity <- grep(" City", subTab$cntyName)
      subTab$cntyName[-whichCity] <- paste0(subTab$cntyName[-whichCity], " County")
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState="Virginia", toolLoc=subTab$cntyName, coastal=toolRec$`Geographic Scope-Coastal`)
      toolScope <- paste0("__**Geographic Coverage**__", el, "- All Virginian Cities and Counties", el)
    }else if(toolRec$`Geographic Scope-Scope`=="Locality" & is.na(toolRec$`Geographic Scope-Locality`)==F){ 
      
      if(toolRec$`Geographic Scope-Locality`=="all watershed cities/counties" | toolRec$`Geographic Scope-Locality`=="Chesapeake Bay"){
        subTab <- scTab[scTab$state %in% c("VA","MD"),]
        subTab <- subTab[subTab$coastal=="Y",]
        subTab$state[subTab$state=="MD"] <- "Maryland"
        subTab$state[subTab$state=="VA"] <- "Virginia"
        whichCity <- grep(" City", subTab$cntyName)
        subTab$cntyName[-whichCity] <- paste0(subTab$cntyName[-whichCity], " County")
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=subTab$state, toolLoc=subTab$cntyName, coastal="x")
        toolScope <- paste0("__**Geographic Coverage**__", el, "- All Cities and Counties around the Chesapeake Bay", el)
      }else if(toolRec$`Tool ID`=="23.0"){
        locsNoState <- c("City of Buffalo", "City of Lancaster", "Richmond City", "City of Gaithersburg", "City of Blacksburg")
        findState <- c("New York", "Pennsylvania", "Virginia", "Maryland", "Virginia")
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=findState, toolLoc=locsNoState, coastal=toolRec$`Geographic Scope-Coastal`)
        toolScope <- paste0("__**Geographic Coverage**__", el, "- Buffalo, NY; Lancaster, PA; Richmond City, VA; Gaithersburg, MD; Blaskburg, VA", el)
      }else if(sapply(strsplit(toolRec$`Tool ID`, "[.]"),"[[",1)=="45"){
        locsNoState <- "District of Columbia"
        findState <- "DC"
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=findState, toolLoc=locsNoState, coastal=toolRec$`Geographic Scope-Coastal`)
        toolScope <- paste0("__**Geographic Coverage**__", el, "- DC", el)
      }else if(toolRec$`Tool ID`=="46.0"){
        locsNoState <- c("District of Columbia", "City of Takoma Park", "Prince George's County", "Community of Beltsville", "City of Rockville", "Montgomery County",
                         "Charles County", "Frederick County", "Calvert County", "Alexandria City", "Arlington County", "Prince William County", 
                         "Loudoun County", "Fairfax County")
        findState <- c("DC", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Maryland", "Virginia", "Virginia", "Virginia", "Virginia", "Virginia")
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=findState, toolLoc=locsNoState, coastal=toolRec$`Geographic Scope-Coastal`)
        toolScope <- paste0("__**Geographic Coverage**__", el, "- DC; Takoma Park, MD; Prince George's County, MD; Beltsville, MD; Rockville, MD; Montgomery County, MD; Charles County, MD; Frederick County, MD; Calvert County, MD; Alexandria, VA; Arlington County, VA; Prince William County, VA; Loudoun County, VA; Farifax County, VA", el)
      }else if(toolRec$`Tool ID`=="53.1"){
        locsNoState <- c("District of Columbia", "Arlington County", "Alexandria City", "Fort Foote", "Fairfax County", "Prince George County")
        findState <- c("DC", "Virginia", "Virginia", "Maryland", "Virginia", "Virginia")
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=findState, toolLoc=locsNoState, coastal=toolRec$`Geographic Scope-Coastal`)
        toolScope <- paste0("__**Geographic Coverage**__", el, "- DC; Arlington County, VA; Alexandria, VA; Fort Foote, MD; Fairfax County, VA; Prince George County, VA", el)
      }else{
        collectLocs <- strsplit(toolRec$`Geographic Scope-Locality`, "; ")[[1]]
        ##isolate the location
        locsNoState <- sapply(strsplit(collectLocs, ", "), "[[", 1)
        whichCnts <- grep(" County", locsNoState)
        whichIsl <- grep(" Island$", locsNoState)
        whichDC <- grep("Washington, DC", collectLocs)
        locsNoState[-c(whichCnts,whichIsl,whichDC)] <- paste0(locsNoState[-c(whichCnts,whichIsl,whichDC)], " City")
        ##isolate the state
        findState <- sapply(strsplit(collectLocs, ", "), "[[", 2)
        toolScope <- paste0("__**Geographic Coverage**__", el, "- ", paste(locsNoState, ", ", findState, sep="", collapse="; "), el)
        findState[grep("VA", findState)] <- "Virginia"
        findState[grep("PA", findState)] <- "Pennsylvania"
        findState[grep("MD", findState)] <- "Maryland"
        findState[grep("NY", findState)] <- "New York"
        findState[grep("TN", findState)] <- "Tennessee"
        ##
        toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=findState, toolLoc=locsNoState, coastal=toolRec$`Geographic Scope-Coastal`)
      }
    }else if(toolRec$`Geographic Scope-Scope`=="State"){
      collectSts <- strsplit(toolRec$`Geographic Scope-State`, "; ")[[1]]
      
      locsNoState <- NA
      if(is.na(toolRec$`Geographic Scope-Coastal`)==F & toolRec$`Geographic Scope-Coastal`=="x"){
        subTab <- scTab[scTab$state %in% collectSts,]
        subTab <- subTab[subTab$coastal=="Y",]
        whichCity <- grep(" City", subTab$cntyName)
        subTab$cntyName[-whichCity] <- paste0(subTab$cntyName[-whichCity], " County")
        locsNoState <- subTab$cntyName
        collectSts <- subTab$state
      }
      
      collectSts[grep("VA", collectSts)] <- "Virginia"
      collectSts[grep("WV", collectSts)] <- "West Virginia"
      collectSts[grep("PA", collectSts)] <- "Pennsylvania"
      collectSts[grep("MD", collectSts)] <- "Maryland"
      collectSts[grep("NY", collectSts)] <- "New York"
      collectSts[grep("TN", collectSts)] <- "Tennessee"
      collectSts[grep("DE", collectSts)] <- "Delaware"
      collectSts[grep("NJ", collectSts)] <- "New Jersey"
      collectSts[grep("NC", collectSts)] <- "North Carolina"
      collectSts[grep("IN", collectSts)] <- "Indiana"
      collectSts[grep("IL", collectSts)] <- "Illinois"
      collectSts[grep("WI", collectSts)] <- "Wisconsin"
      collectSts[grep("MI", collectSts)] <- "Michigan"
      collectSts[grep("MN", collectSts)] <- "Minnesota"
      collectSts[grep("NE", collectSts)] <- "Nebraska"
      collectSts[grep("CO", collectSts)] <- "Colorado"
      collectSts[grep("MT", collectSts)] <- "Montana"
      collectSts[grep("MO", collectSts)] <- "Missouri"
      collectSts[grep("KS", collectSts)] <- "Kansas"
      collectSts[grep("NM", collectSts)] <- "New Mexico"
      collectSts[grep("IA", collectSts)] <- "Iowa"
      collectSts[grep("AR", collectSts)] <- "Arkansas"
      collectSts[grep("OR", collectSts)] <- "Oregon"
      collectSts[grep("SD", collectSts)] <- "South Dakota"
      collectSts[grep("TN", collectSts)] <- "Tennessee"
      collectSts[grep("CA", collectSts)] <- "California"
      collectSts[grep("CT", collectSts)] <- "Connecticut"
      collectSts[grep("RI", collectSts)] <- "Rhode Island"
      collectSts[grep("MA", collectSts)] <- "Massachusetts"
      collectSts[grep("NH", collectSts)] <- "New Hampshire"
      collectSts[grep("ME", collectSts)] <- "Maine"
      collectSts[grep("VT", collectSts)] <- "Vermont"
      collectSts[grep("KY", collectSts)] <- "Kentucky"
      
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState=collectSts, toolLoc=locsNoState, coastal=toolRec$`Geographic Scope-Coastal`)
      toolScope <- paste0("__**Geographic Coverage**__", el, "- ", paste(unique(collectSts), collapse="; "), el)
    }else if(toolRec$`Geographic Scope-Scope`=="National"){
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState="All", toolLoc="All", coastal=toolRec$`Geographic Scope-Coastal`)
      toolScope <- paste0("__**Geographic Coverage**__", el, "- Contiguous United States", el)
    }else{
      toolGeo <- data.frame(toolName=toolRec$`Tool Name`, toolLink=toolIDtxt, toolState="Gen", toolLoc="Gen", coastal=toolRec$`Geographic Scope-Coastal`)
      toolScope <- paste0("__**Geographic Coverage**__", el, "- Applicable Anywhere", el)
    }
    searchGeoScope <<- rbind.data.frame(searchGeoScope, toolGeo)
    
    
    
    ##tool strengths
    if(is.na(toolRec$`Description-Strengths`)==F){
      toolStrs <- paste0("__**Strengths**__", el, paste("- ", strsplit(toolRec$`Description-Strengths`, "; ")[[1]], collapse=el), el)
    }else{
      toolStrs <- paste0("__**Strengths**__", el, "Not Available", el)
    }
    
    ##tool weaknesses
    #if(is.na(toolRec$Limitations)==F){
    #  checkLim <- strsplit(toolRec$Limitations, ";")[[1]]
    #  toolLims <- paste0("**Limitations:** ", paste(checkLim, collapse=", "), el)
    #}else{
    #  toolLims <- paste0("**Limitations:** ", "Not Available", el)
    #}
    
    ##tool URL
    if(is.na(toolRec$URL)==F){
      #toolURL <- paste0('<a href="', toolRec$URL, '" target="_blank">Get This Tool</a>', el)  ##embedded link in Get This Tool
      toolURL <- paste0('__**Get This Tool:**__ ', toolRec$URL, el)
    }else{
      #toolURL <- paste0("[Get This Tool] ", "Not Available")
      toolURL <- paste0("", el)
    }
    
    ##tool cost
    if(is.na(toolRec$`Software Requirements-Cost`)==F){
      toolCost <- paste0("__**Cost**__", el, "- ", toolRec$`Software Requirements-Cost`, el)
    }else{
      toolCost <- paste0("__**Cost**__", el, "- Unknown", el)
    }
    
    ##tool skill lvl
    if(is.na(toolRec$`Effort / Skill-Skill`)==F){
      toolSkill <- paste0("__**Skill Level**__", el, "- ", toolRec$`Effort / Skill-Skill`)
    }else{
      toolSkill <- paste0("__**Skill Level**__", el, "- Unknown")
    }
    
    
    ##bring the page together
    #toolPage <- paste(toolPage, pageTitle, toolDev, "#### Tool Summary-", toolDes, toolUse, toolScope, toolAud, toolStrs, toolLims, toolURL, sep=el)
    #toolPage <- paste(toolPage, toolDev, "#### Tool Summary", toolDes, toolUse, toolScope, toolAud, toolStrs, toolLims, toolURL, sep=el)
    toolPage <- paste(toolPage, toolDev, toolDes, toolURL, toolScope, toolStrs, toolCost, toolSkill, sep=el)
    writeLines(toolPage, file(writeTool))
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





