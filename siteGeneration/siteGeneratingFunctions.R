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
generateToolPage <- function(toolRec, tempPage, tempIndex, el, siteDir, updateContent=F){
  #################
  #toolRec <- splitByToolID[[98]]
  #tempPage <- templatePage
  #tempIndex <- chaptPage
  #el <- "\n"
  #siteDir <- siteDir
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
  toolDir <- paste0(siteDir, gsub("/|:", "-", toolRec$`Tool Name`), "/")
  writeTool <- paste0(toolDir, "_index.md")
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
  if(dir.exists(toolDir)==F){
    dir.create(toolDir, recursive=T);
  }
  
  if(file.exists(writeTool)==F | updateContent==T){
    ##provide new page title
    toolPage <- gsub("page-template", toolRec$`Tool Name`, tempPage)
    ##update date
    toolPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), toolPage)
    ##change description
    toolPage <- gsub("a page", toolRec$`Tool Name`, toolPage)
    ##change the weight of the file based on what is already in the directory
    #toolPage <- gsub("weight: 1", paste0("weight: ", length(list.files(paste0(siteDir,splitDevs[1])))+1), toolPage)
    
    ##page/tool title
    #pageTitle <- paste0(el, "## ", toolRec$`Tool Name`)
    
    ##tool developer
    if(length(splitDevs)==1){
      toolDev <- paste0("Developer: ", splitDevs, el)
    }else{
      toolDev <- paste0("Developers: ", paste(splitDevs, collapse=", "), el)
    }
    
    ##tool description
    toolDes <- paste0("**Description:** ", toolRec$About, el)
    
    ##Why tool is useful
    toolUse <- paste0("**Relevance:** ", toolRec$`Climate Relevance`, el)
    
    ##tool audience
    checkAud <- formatStarStrings(toolRec$`Target Audience`)
    toolAud <- paste0("**Target Audience:** ", paste(checkAud, collapse=", "), el)
    
    ##tool scope
    toolScope <- paste0("**Scope:** ", toolRec$Scope, el)
    
    ##tool strengths
    checkStr <- formatStarStrings(toolRec$Strengths)
    toolStrs <- paste0("**Strengths:** ", paste(checkStr, collapse=", "), el)
    
    ##tool weaknesses
    checkLim <- formatStarStrings(toolRec$Limitations)
    toolLims <- paste0("**Limitations:** ", paste(checkLim, collapse=", "), el)
    
    ##tool URL
    toolURL <- paste0("**Where this tool be found:** ", toolRec$URL...5)
    
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





