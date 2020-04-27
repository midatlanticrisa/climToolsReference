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
generateToolPage <- function(toolRec, tempPage, el, siteDir, updateContent=F){
  #################
  #toolRec <- splitByToolID[[3]]
  #toolRec <- splitByToolID[[9]]
  #toolRec <- splitByToolID[[36]]
  toolRec <- splitByToolID[[64]]
  tempPage <- templatePage
  el <- "\n"
  siteDir <- toolsDir
  #################
  ##check the developers, to make sure the directories are available and are set up properly
  if(is.na(toolRec$Developer)==T){
    toolRec$Developer <- "Unknown"
  }
  ##if there is more than one developer, split the developers into a vector
  splitDevs <- strsplit(toolRec$Developer, "; ")[[1]]
  toolIDtxt <- paste0("page-tool", toolRec$`Tool ID`)  ##the name of the tool page within the site
  #writeTool <- paste0(siteDir, splitDevs[1], "/", toolIDtxt, ".md")  ##the file in which the tool information will be written to, assumes first developer is the most important
  toolRec$`Tool Name` <- gsub(el, " ", toolRec$`Tool Name`) ##remove any cariage returns from the tool name
  #toolDir <- paste0(siteDir, gsub("/|:", "-", toolRec$`Tool Name`), "/")
  writeTool <- paste0(siteDir, toolIDtxt, ".md")
  ##if subdirectories do not already exist for the developers, create them and the chapter file for that developer
  # sapply(splitDevs, function(dev){if(dir.exists(paste0(siteDir,dev))==F){
  #                                   dir.create(paste0(siteDir,dev), recursive=T);
  #                                 };
  #                                 
  #                                 ##check to see if the index, chapter, file for each developer exists
  #                                 findDevMain <- paste0(siteDir,dev,"/_index.md");
  #                                 if(file.exists(findDevMain)==T & updateContent==T){  ##if don't already exist, create it
  #                                   chaptPage <- paste(readLines(findDevMain), collapse=el);
  #                                   chaptCurrDate <- strsplit(chaptPage, "date: |\nweight:")[[1]]
  #                                   #chkTime <- difftime(strptime(chaptCurrDate[2], "%FT%T%z"), strptime(format(Sys.time(), "%FT%T%z"), "%FT%T%z"), units="mins")
  #                                   chaptPage <- paste0(chaptCurrDate[1], "date: ", format(Sys.time(), "%FT%T%z"), "\nweight:", chaptCurrDate[3],
  #                                                       "[", toolRec$`Tool Name`, "](#", toolIDtxt, ")")
  #                                 }else if(file.exists(findDevMain)==F){  ##if already exists and needs to be updated
  #                                   chapFile <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), tempIndex);
  #                                   chapFile <- gsub("Chapter Title", dev, chapFile);
  #                                   #chapFile <- gsub("draft: true", "draft: false", chapFile)
  #                                   chapFile <- paste0(chapFile, el,
  #                                                      "## ", dev, el,
  #                                                      el,
  #                                                      "### List of Tools:", el,
  #                                                      "[", toolRec$`Tool Name`, "](#", toolIDtxt, ")");
  #                                   writeLines(chapFile, file(findDevMain));
  #                                 };
  # 
  #                                 ##now, update all developer index files with a link to the tool
  #                                 chaptPage <- paste0(paste(readLines(findDevMain), collapse=el), el, 
  #                                                     "[", toolRec$`Tool Name`, "](#", toolIDtxt, ")");
  #                                 writeLines(chaptPage, file(findDevMain))
  #                                 })
  
  #if(dir.exists(toolDir)==F){
  #  dir.create(toolDir, recursive=T);
  #}
  
  if(file.exists(writeTool)==F | updateContent==T){
    ##provide new page title
    toolPage <- gsub("page-template", toolRec$`Tool Name`, tempPage)
    ##update date
    toolPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), toolPage)
    ##change description
    if(is.na(toolRec$Description)==F){
      toolPage <- gsub("a page", toolRec$Description, toolPage)
    }else{
      toolPage <- gsub("a page", toolRec$`Tool Name`, toolPage)
    }
    ##add categories to the header
    baseScope <- toolRec$Scope
    if(baseScope=="State" & is.na(toolRec$State)==F){
      availStates <- strsplit(toolRec$State, "; |;\n")[[1]]
      baseScope <- paste0(baseScope, " - ", availStates)
    }else if(baseScope=="Locality" & is.na(toolRec$Locality)==F){
      availLocs <- strsplit(toolRec$Locality, "'*'|;\n|*\n")[[1]]
      baseScope <- paste0(baseScope, " - ", availLocs)
    }else if(is.na(baseScope)==T){
      baseScope <- "Unknown Scope"
    }
    cats <- baseScope
    ##checks and adds the tool functionality
    chkFunction <- toolRec[,grep("Tool Function-", names(toolRec))]
    chkFunInd <- which(chkFunction=="x")
    if(length(chkFunInd)>0){
      baseFunction <- sapply(strsplit(names(chkFunction)[chkFunInd], "-"), "[[", 2)
      cats <- c(cats, baseFunction)
    }
    ##checks and adds the tool function projections
    chkProj <- toolRec[,grep("Future Projections-", names(toolRec))]
    chkProjInd <- which(chkProj=="x")
    if(length(chkProjInd)>0){
      baseProj <- sapply(strsplit(names(chkProj)[chkProjInd], "-"), "[[", 2)
      cats <- c(cats, baseProj)
    }
    ##add coastal, if appropriate
    if(toolRec$Coastal=="x" & is.na(toolRec$Coastal)==F){
      cats <- c(cats, "Coastal")
    }
    toolPage <- gsub('\"categories\"', paste(paste0('\"', unique(cats), '\"'), collapse=", "), toolPage)
    
    ##add  tags to the header
    tags <- splitDevs
    if(is.na(toolRec$State)==F){
      tags <- c(tags, strsplit(toolRec$State, "; |;\n")[[1]])
    }
    if(is.na(toolRec$Locality)==F){
      tags <- c(tags, strsplit(toolRec$Locality, "'*'|;\n|*\n")[[1]])
    }
    if(is.na(toolRec$`Target Audience`)==F){
      tags <- c(tags, formatStarStrings(toolRec$`Target Audience`))
    }
    toolPage <- gsub('\"tags\"', paste(paste0('\"', unique(tags), '\"'), collapse=", "), toolPage)
    
    
    ##page/tool title
    #pageTitle <- paste0(el, "## ", toolRec$`Tool Name`)
    
    ##tool developer
    if(length(splitDevs)==1){
      toolDev <- paste0("Developer: ", splitDevs, el)
    }else{
      toolDev <- paste0("Developers: ", paste(splitDevs, collapse=", "), el)
    }
    
    ##tool description
    if(is.na(toolRec$About)==F){
      toolDes <- paste0("**Description:** ", toolRec$About, el)
    }else{
      toolDes <- paste0("**Description:** ", "Not Available", el)
    }
    
    ##Why tool is useful
    if(is.na(toolRec$`Climate Relevance`)==F){
      toolUse <- paste0("**Relevance:** ", toolRec$`Climate Relevance`, el)
    }else{
      toolUse <- paste0("**Relevance:** ", "Not Available", el)
    }
    
    ##tool audience
    if(is.na(toolRec$`Target Audience`)==F){
      checkAud <- formatStarStrings(toolRec$`Target Audience`)
      toolAud <- paste0("**Target Audience:** ", paste(checkAud, collapse=", "), el)
    }else{
      toolAud <- paste0("**Target Audience:** ", "Not Available", el)
    }
    
    ##tool scope
    if(is.na(toolRec$Scope)==F){
      toolScope <- paste0("**Scope:** ", toolRec$Scope, el)
    }else{
      toolScope <- paste0("**Scope:** ", "Not Available", el)
    }
    
    ##tool strengths
    if(is.na(toolRec$Strengths)==F){
      checkStr <- formatStarStrings(toolRec$Strengths)
      toolStrs <- paste0("**Strengths:** ", paste(checkStr, collapse=", "), el)
    }else{
      toolStrs <- paste0("**Strengths:** ", "Not Available", el)
    }
    
    ##tool weaknesses
    if(is.na(toolRec$Limitations)==F){
      checkLim <- formatStarStrings(toolRec$Limitations)
      toolLims <- paste0("**Limitations:** ", paste(checkLim, collapse=", "), el)
    }else{
      toolLims <- paste0("**Limitations:** ", "Not Available", el)
    }
    
    ##tool URL
    if(is.na(toolRec$URL)==F){
      toolURL <- paste0("**Where this tool be found:** ", toolRec$URL)
    }else{
      toolURL <- paste0("**Where this tool be found:** ", "Not Available")
    }
    
    ##bring the page together
    #toolPage <- paste(toolPage, pageTitle, toolDev, "#### Tool Summary-", toolDes, toolUse, toolScope, toolAud, toolStrs, toolLims, toolURL, sep=el)
    toolPage <- paste(toolPage, toolDev, "#### Tool Summary", toolDes, toolUse, toolScope, toolAud, toolStrs, toolLims, toolURL, sep=el)
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





