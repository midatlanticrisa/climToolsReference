##a script to automatically pull apart input data and present that data in a website

##libraries
library(readxl)
library(pbapply)

##find data file, assumed to be on computer
baseDir <- "/Users/mdl5548/Documents/GitHub/"
dataDir <- paste0(baseDir, "testingRepo/")
#siteDir <- paste0(dataDir, "docs/")
siteDir <- paste0(baseDir, "climToolsReference/content/")
source(paste0(dataDir, "siteGeneratingFunctions.R"))

#toolInventoryName <- "Inventory_2020-01-21_v2.xlsx"
toolInventoryName <- "COPY-of-Inventory_Ben"
templatePageName <- "pageTemplate.md"
templateChapterName <- "chapterTemplate.md"
#ymlFileName <- "_site.yml"

##line break character
el <- "\n"
tab <- "  "

##read in the tool description document
toolInventoryFile <- list.files(dataDir, toolInventoryName, recursive=T, full.names=T)
#toolInventoryFile <- descFile[grep(paste0("/",toolInventoryFile), descFile)]

##get number of sheets, to load all of them
sheetNames <- excel_sheets(toolInventoryFile)

##read in data
toolInventory <- lapply(sheetNames, read_excel, path=toolInventoryFile, col_names=F)

##inventory, cleaning headers to be useful
inventory <- toolInventory[[1]]
colnames(inventory) <- as.character(inventory[2,])
inventory <- inventory[which(inventory$`Tool ID`=="1.0"):nrow(inventory),]
invValsOnly <- inventory[which(is.na(inventory$`Group ID`)==F),]
invValsOnly <- invValsOnly[,1:13]

##read in the template files to be editted
findTempFile <- list.files(dataDir, templatePageName, recursive=T, full.names=T)
templatePage <- paste(readLines(findTempFile), collapse="\n")
findChaptFile <- list.files(dataDir, templateChapterName, recursive=T, full.names=T)
chaptPage <- paste(readLines(findChaptFile), collapse="\n")

##set up directories for each tool developer
#toolDevs <- createDeveloperDirs(invValsOnly, chaptPage, siteDir, updateContent=T)

##split by group id, seems to be coorolated with developers
#splitByGroup <- lapply(unique(invValsOnly$`Group ID`), function(x){invValsOnly[invValsOnly$`Group ID`==x,]})
toolIDs <- unique(invValsOnly$`Tool ID`)
toolIDs <- toolIDs[is.na(toolIDs)==F]
splitByToolID <- lapply(toolIDs, function(x){invValsOnly[which(invValsOnly$`Tool ID`==x & is.na(invValsOnly$`Tool ID`)==F),]})
#splitByScope <- lapply(unique(invValsOnly$Scope), function(x){invValsOnly[invValsOnly$Scope==x,]})

#agencyNames <- unique(invValsOnly$Developer)
#scope <- unique(invValsOnly$Scope)
#toolGroup <- unique(invValsOnly$`Group ID`)

##########################################################################


pblapply(splitByToolID, generateToolPage, tempPage=templatePage, tempIndex=chaptPage, el=el, siteDir=siteDir, updateContent=T)

##need to set up the index and about files
indexPage <- gsub("page-template", "Climate Tools are Coolz", templatePage)
indexPage <- gsub("1-1-1111", format(Sys.time(), "%FT%T%z"), indexPage)
isect1Title <- "### Climate Tools and what they do"
ip1 <- paste0("Blurb about what climate tools are and things they do.", el)
isect2Title <- "### Why Climate Tools are important"
ip2 <- paste0("Blurb about why this list is useful and important", el)
indexPage <- paste(indexPage, isect1Title, ip1, isect2Title, ip2, sep=el)
writeFile <- paste0(siteDir, "/_index.md")
writeLines(indexPage, file(writeFile))


 
# aboutPage <- gsub("page-template", "about", templatePage)
# asect1Title <- "### Who we are"
# ap1 <- paste0("EESI and people involved")
# aboutPage <- paste(aboutPage, asect1Title, ap1, sep=el)
# writeFile <- paste0(siteDir, "/about.Rmd")
# writeLines(aboutPage, file(writeFile))
# 
# ##build the YML file from scratch
# siteName <- paste0('name: "testSite"')
# setNavBar <- paste0('navbar:', el, 
#                     tab, 'title: "Climate Tools"', el, 
#                     tab, 'left:', el,
#                     tab, tab, '- text: "Home"', el,
#                     tab, tab, tab, 'href: index.html', el,
#                     tab, tab, '- text: "About"', el,
#                     tab, tab, tab, 'href: about.html', el,
#                     tab, tab, '- text: "Tools"', el,
#                     tab, tab, tab, 'menu:')
# ##using for loop instead of a apply for convinence
# findToolFiles <- list.files(siteDir, "page-tool")
# ##replace the .Rmd with .html, to reflect the change in file type when the site is built
# toolHTMLs <- sapply(findToolFiles, gsub, pattern=".Rmd", replacement=".html")
# toolFileNames <- sapply(strsplit(findToolFiles, ".Rmd"), "[[", 1)
# 
# navBar <- paste0(tab, tab, tab, tab, '- text: "', toolFileNames, '"', el,
#                  tab, tab, tab, tab, tab, 'href: ', toolHTMLs, collapse=el)
# 
# endYML <- paste0('output:', el,
#                  #tab, 'html_document:', el,
#                  tab, 'distill::distill_article:', el,
#                  #tab, tab, 'theme: cosmo')
#                  tab, tab, 'css: styles.css')
# 
# constructYML <- paste(siteName, setNavBar, navBar, endYML, sep=el)
# writeFile <- paste0(siteDir, "/_site.yml")
# writeLines(constructYML, file(writeFile))
# 
# ##create a custom css file for the site
# cssFile <- paste0('.distill-site-nav {', el,
#                   tab, 'height: 100%;', el,
#                   tab, 'width: 160px;', el, 
#                   tab, 'position: fixed;', el,
#                   tab, 'z-index: 1;', el,
#                   tab, 'top: 0;', el,
#                   tab, 'left: 0;', el,
#                   tab, 'background-color: #111;', el,
#                   tab, 'overflow-x: hidden;', el,
#                   tab, 'padding-top: 20px;', el,
#                   '}', el,
#                   '.distill-site-nav a {', el,
#                   tab, 'padding: 6px 8px 6px 16px;', el,
#                   tab, 'text-decoration: none;', el,
#                   tab, 'font-size: 25px;', el,
#                   tab, 'color: #818181;', el,
#                   tab, 'display: block;', el,
#                   '}', el,
#                   '.distill-site-nav a:hover {', el,
#                   tab, 'color: #f1f1f1;', el,
#                   '}', el,
#                   '.main {', el,
#                   tab, 'margin-left: 160;;', el,
#                   tab, 'padding: 0px 10px;', el,
#                   '}', el,
#                   '@media screen and (max-height: 450px) {', el,
#                   tab, '.distill-site-nav {padding-top: 15px;}', el,
#                   tab, '.distill-site-nav a {font-size: 18px;}', el,
#                   '}')
# writeFile <- paste0(siteDir, "/styles.css")
# writeLines(cssFile, file(writeFile))
# 
# setwd(siteDir)
# rmarkdown::render_site()


