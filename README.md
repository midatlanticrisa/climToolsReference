# climToolsReference
This repository generates and hosts the website for the [Climate Tool Inventory](https://cbtooltest.marisa.psu.edu/), for the NOAA Mid-Atlantic Regional Integrated Sciences and Assessments (MARISA). The Climate Tool Inventory is a synthesis of climate tools where a user can filter the tools based on geographic region and/or topic. A user can also review a short discription of the tool and description of the functions offered. The goal is to help advise which climate tools would best suit the goals of a users project.

## Generating the website
All of the scripts, data, templates, and other files for generating the website are contained in the folder *siteGeneration*.

The list of climate tools and the information for each tool is contained in the *siteGeneration/Tool Inventory - .xlsx* file. There are multiple versions of this file, but the most up-to-date is **Tool Inventory - 12.15.21 copy.xlsx**.

The list of definitions for keywords used in the climate tools website are contained in the *siteGeneration/glossary.xlsx* file.

**MARISAstateFIPS.csv** - list of counties in each state and whether they are coastal. 

**generateSite\_2022\_searchbars.R** - Main R script that parses the tool dataset and .csv file to create the website using the templates listed in the directory.

**siteGeneratingFunctions\_2022\_searchbars.R** - Functions called in the generateSite.R script to facilitate website generation.

**generateSite\generateGlossary.R** - R script that parses the definition dataset to create the glossary page and the definition dropdown menu on the tool search page.

**collectionTemplate.md** - markdown template for pages with Tool collection descriptions.

**pageTemplate.md** - markdown template for pages with Tool descriptions.

**toolsTemplate.md** - markdown template for the Tools page.

**tagTemplate.md** - markdown template for pages with Tag lists.

**resizeScreenshots** - shell script to resize Tool screenshots using image Magick.

**saveRemovedAllCheckCode** - ??

**searchTemplate.html** - template for searching and filtering tools

**templateIndex.md** - ??

## Building the website
### Development

We now use a custom Docker container as the preferred method for building and testing the site. Using the Jekyll container defined in the [scrim-network/container-dev](https://github.com/scrim-network/container-dev) repository, run `./buildit` to build the site and `./testit` with your browser pointed to `127.0.0.1:4000` to view live changes while editing your local version of the site.

On systems where the Docker container cannot be installed, you will need to install Jekyll locally and use the `jekyll build` or `jekyll serve` (maybe with the help of Bundler?) commands instead. Installation of Jekyll varies by platform and is beyond the scope of this document.


### Deployment

The MARISA Climate Tools site is currently hosted on the EMS laurasia webserver. To push the locally-built site to the server, use `./shipit` and authenticate with your Penn State credentials. At present, neither 2FA nor a VPN are required to push changes.

#### Deprecated approach

Note: Many changes have been made to the site since this approach was last used. There's a good chance it would break the site.

```
jekyll build
scp -o ProxyCommand="ssh woju.scrim.psu.edu nc laurasia.ems.psu.edu 22" -r _site/* laurasia.scrim.psu.edu:marisa_website/
```
For ease of use, you can use the `build_and_push_to_website` script to accomplish this. This won't work, of course, unless you have access to the webserver. Sorry, Kelsey (we'll figure this out at some point).

## Contact
Detailed questions should be directed to Matthew Lisk: <mdl5548@psu.edu> or Kelsey Ruckert: <klr324@psu.edu>.


## Credits
The MARISA Climate Data Portal was adapted from Barry Clark's lovely, minimalist [Jekyll Now](https://github.com/barryclark/jekyll-now) theme. Thanks, Barry!
