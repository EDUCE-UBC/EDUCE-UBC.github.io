---
title: "EDUCE Frequently Asked Questions"
output: 
  html_document:
    toc: true
    toc_depth: 4
    toc_float:
      collapsed: false
      smooth_scroll: true
---

# What is EDUCE?

Experiential Data Science for Undergraduate Cross-disciplinary Education (EDUCE)

EDUCE seeks to develop a uniform experiential learning framework that is cross-disciplinary and collaborative to equip undergraduate students in the life sciences with basic competency and literacy in data science as it pertains to microbiome sequence information. To achieve this, EDUCE will develop and implement a mechanism to augment undergraduate training in the Department of Microbiology & Immunology (MICB) with basic analyses and statistical principles and microbiome data processing and interpretation – skills that transcend any one course in the department.

# What is Course Compiler?

Here you can select and compile all the data science modules for either teaching purposes and/or self-learning purposes. Select the courses that best fit your interests/curriculum and have the Course Compiler Shiny App knit it for you. We will also provide the source code for generating the arbitrary knitted files and enable you to customize courses with your own files. More experienced data science instructors will have access to more benefits, so we encourage you to learn the basics of RStudio to maximize your benefits!

#### What Course Compiler Offers:
- Ability to select and knit courses that interest you and/or fit your teaching plans
- Ability to select the HTML format for novice data science instructors or the ZIP format for more experienced data science instructors (Source code will be provided in the ZIP format for you to use and personalize your own courses)
- More experienced data science instructors will have access to knitted files that contain a table of content for easier browsing

#### Provided Files in ZIP Format:

  - Rmd_Output = All the courses that you selected
  - Rmd_Master_File = Master file containing R children for all the knitted courses
  - User_File = Source code used to generate all the courses that you want

#### Instructions for knitting your own personalized courses:
To knit your own courses, select the courses that you want and then select the ZIP format in Course Compiler. To personalize the knitted files with your own Rmd files, put all the files that you want to knit into the folder "Rmd_Output" and knit the "User_File.Rmd". Both of which are provided from the ZIP format. This will generate a "Rmd_Master_File.Rmd", which contains all the R children of the files that you want knitted. This makes it easier if you wanted to knit multiple Rmd files together.

# What is Data Normalization?

The Data Normalization Shiny App provides an interactive way to learn the various methods of normalization in a tabular and graphical way. Novice data science instructors will be provided some basic files to experiment with and more experienced data science instructors will have the ability to use their own files to do some more in-depth experimentation and manipulation of data via the graph inputs (Number of OTU/ASV to Display, Number of Columns to Display, and Random Generator Seed).

#### File Inputs For Data Normalization:
- Text/CSV
- Text/Plain
- CSV

#### What are the "Validate Data" and "Test App Version" Buttons?

More experienced data science instructors may help us test the Data Normalization Shiny app version by providing us with their OTU/ASV files for integration testing. To submit your file(s), upload your file and press the "Validate Data" button. This will save a copy of your file on our server and we may use it for future tests. To determine if the current version of the app is working as intended, click the "Test App Version" button and you will receive a notification indicating whether the current version of the app is working as intended or not.

#### Advantages/Disadvantages of Methods of Normalization:

Percent Relative Abundance (PRA)
Desc

Advantages:

- Can easily conceptualize data without overthinking

Disadvantages:

- Difficult to comprehend complex data sets

https://sciencing.com/advantages-disadvantages-frequency-table-12000027.html

Random Subsampling (RS)
Desc

Advantages:

- Can be repeated an indefinite number of times

Disadvantages:

- Test sets are not independently drawn with respect to underlying distribution (Can lead to increased chance of Type I error)
	- Rarefied counts remain overdispersed relaitve to Poisson model (Increase in Type I error)

- Rarefied counts represent only a small fraction of original data (Can lead to increased chance of Type II error)

- Requires arbitrary selection of library size minimum threshold that affects downstream inference

- Random step in rarefying is unnecessary and adds artificial uncertainty

https://www.coursehero.com/file/pvuecg/Random-subsampling-and-k-fold-cross-validation-are-two-common-methods-of-resam/

http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003531#s4

Multiple Imputation (MI)
Desc

Advantages:

- Reduced bias

- Improve validity

- Increase precision

- Result in robust statistics (Resistant to outliers)

Disadvantages:

- Must make a few required statistical assumptions:
	- Data is missing at random?
	- Multivariate normal distribution? (For some of the modeling methods)

- Need to transform variables to approximate normal distribution before running imputation procedure (If data isn't normally distributed)

- Incorrect model choices or exclusion of vital data points may lead to more bias

http://www.statisticshowto.com/multiple-imputation/

https://www.quora.com/How-do-you-handle-missing-data-statistics-What-imputation-techniques-do-you-recommend-or-follow

Other
https://microbiomejournal.biomedcentral.com/articles/10.1186/s40168-017-0237-y#Sec4

https://www-jstor-org.ezproxy.library.ubc.ca/stable/pdf/2400321.pdf?refreqid=excelsior%3A48f959307c1ea51f5620e54d748be01c

https://books.google.ca/books?id=LAO7N_0YlQsC&pg=PA292&lpg=PA292&dq=advantage+and+disadvantage+of+rarefaction&source=bl&ots=wov5AC1C0q&sig=DTH2PDSLVarrT81h7Th2wSCTEdI&hl=en&sa=X&ved=0ahUKEwjB8-LIw5fcAhUhCjQIHWITBQUQ6AEIMTAB#v=onepage&q=advantage%20and%20disadvantage%20of%20rarefaction&f=false
