#climToolsReference
This repository generates and hosts the website for the [Climate Tool Inventory](https://cbtooltest.marisa.psu.edu/), for the NOAA Mid-Atlantic Regional Integrated Sciences and Assessments (MARISA). The Climate Tool Inventory is a synthesis of climate tools where a user can filter the tools based on geographic region and/or topic. A user can also review a short discription of the tool and description of the functions offered. The goal is to help advise which climate tools would best suit the goals of a users project.

##Generating the website
All of the scripts, data, templates, and other files for generating the website are contained in the folder *siteGeneration*.

The list of climate tools and the information for each tool is contained in the *siteGeneration/Tool Inventory - .xlsx* file. There are multiple versions of this file, but the most up-to-date is **Tool Inventory - 10.23.20.xlsx**.

**MARISAstateFIPS.csv** - list of counties in each state and whether they are coastal. 

**generateSite.R** - Main R script that parses the tool dataset and .csv file to create the website using the templates listed in the directory.

**siteGeneratingFunctions.R** - Functions called in the generateSite.R script to facilitate website generation.

**pageTemplate.md** - markdown template for pages with Tool descriptions.

**resizeScreenshots** - shell script to resize Tool screenshots using image Magick.

**saveRemovedAllCheckCode** - ??

**searchTemplate.html** - template for searching and filtering tools

**templateIndex.md** - ??

##Building the website
The website is built using [HUGO](https://gohugo.io/), where the output is saved in the *public* folder. For this site, we merged two HUGO themes: [Soho](https://themes.gohugo.io/themes/soho/) and [Resume](https://themes.gohugo.io/themes/hugo-resume/). 

To get started with HUGO, a [quick start guide](https://gohugo.io/getting-started/quick-start/) is available. Most of these steps have already been completed for this site. However, the last (Step 7) is how we build the site.


##Contact
Detailed questions should be directed to Matthew Lisk: <mdl5548@psu.edu> or Kelsey Ruckert: <klr324@psu.edu>.

