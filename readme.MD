## Chapter 4

These files and folders are meant to accompany the third chapter from the [https://learn.r-journalism.com](learn.r-journalism.com) class.

The quickest way to access the data for this chapter is to 1) make sure your current project directory is logically organized. 

Hopefully, you've created folder on your computer in your `Documents` folder called `learning-r` or something like that.

Once you've set your working directory (with maybe `setwd()`) then you can run these commands in your console to download the correct folders and data:

```
install.packages("usethis")
```

and then

```
usethis::use_course("https://github.com/r-journalism/learn-chapter-4/archive/master.zip")
```

----

Otherwise:

Clone or download and unzip this repo. Make sure the folder is **not** in a temporary unzipped folder.

To ensure these files work as expected, please make sure the project directory is set up correctly: 

When you type `getwd()` it should look something like `your_file_path/learn-r-chapter-4`

You can manually set this up by:

*Session > Set Working Directory > To Project Directory*

Alternatively, you can double click the **learn-chapter-4.Rproj** file from your finder/file browser.

----

Navigate to the .rmd files for each sub-chapter to open and follow along with each section.

Or you can execute the following commands in the console to bring up the files.

* `file.edit("charts_with_ggplot/charts_with_ggplot.Rmd")`
* `file.edit("customizing_charts/customizing_charts.Rmd")`
* `file.edit("more_charts/more_charts.Rmd")`


To view the local html version of the .rmd files, you can try the following commands in your console to see rendered version of the sub-chapter:

* `browseURL("charts_with_ggplot/charts_with_ggplot.html")`
* `browseURL("customizing_charts/customizing_charts.html")`
* `browseURL("more_charts/more_charts.html")`


----
