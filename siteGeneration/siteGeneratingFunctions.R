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
generateToolPage <- function(toolRec, tempPage, el, siteDir, updateContent=T){
  #################
  #toolRec <- splitByToolID[[2]]
  #toolRec <- splitByToolID[[9]]
  #toolRec <- splitByToolID[[36]]
  #toolRec <- splitByToolID[[109]]
  #tempPage <- templatePage
  #el <- "\n"
  #siteDir <- toolsDir
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
    
    ##extract software requirements, for search terms on site
    if(is.na(toolRec$`Software Requirements-software`)==F){
      softReqs <- strsplit(toolRec$`Software Requirements-software`, "; |;  ")[[1]]
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
    
    ##tool scope
    require(openintro)
    if(is.na(toolRec$`Geographic Scope-Scope`)==F & toolRec$`Geographic Scope-Scope`=="State"){
      toolScope <- paste0("__**Geographic Coverage**__", el, paste("- ", abbr2state(strsplit(toolRec$`Geographic Scope-State`, "; ")[[1]]), collapse=el), el)
    }else if(is.na(toolRec$`Geographic Scope-Scope`)==F & toolRec$`Geographic Scope-Scope`=="Locality"){ 
      toolScope <- paste0("__**Geographic Coverage**__", el, paste("- ", strsplit(toolRec$`Geographic Scope-Locality`, "; ")[[1]], collapse=el), el)
    }else if(is.na(toolRec$`Geographic Scope-Scope`)==F & toolRec$`Geographic Scope-Scope`=="National"){
      toolScope <- paste0("__**Geographic Coverage**__", el, "- Contiguous United States", el)
    }else if(is.na(toolRec$`Geographic Scope-Scope`)==F & toolRec$`Geographic Scope-Scope`=="General"){
      toolScope <- paste0("__**Geographic Coverage**__", el, "- Applicable Anywhere", el)
    }else{
      toolScope <- paste0("__**Geographic Coverage**__", el, "Not Available", el)
    }
    
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
      toolURL <- paste0('<a href="', toolRec$URL, '" target="_blank">Get This Tool</a>', el)
      #toolURL <- paste0('[Get This Tool](', toolRec$URL, '){:target="_blank"}', el)
      ##collect a screenshot of the tool, and store it to be included in the website
      if(length(grep(".xls", toolRec$URL))==0){
        testGetShot <- try(webshot(toolRec$URL, paste0(imageDir, toolIDtxt, ".png"), cliprect="viewport", vwidth=674, vheight=457, delay=1.5), silent=T)
        if(class(testGetShot) %in% 'try-error'){
          
        }else{
          webshot(toolRec$URL, paste0(imageDir, toolIDtxt, ".png"), cliprect="viewport", vwidth=674, vheight=457, delay=1.5)
          #print(toolPage)
          toolPage <- gsub("https://www.887theriver.ca/wp-content/uploads/2017/07/placeholder.jpg", paste0("/images/", toolIDtxt, ".png"), toolPage)
          #print(toolPage)
        }
      }
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





