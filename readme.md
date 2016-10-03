# Pipeline

Data analysis pipeline for the R language.

## Install
Since the repo is private for now git fork are the easiest way to contribute.

## Documentation
TBA - see the man/ folder - Accessible through R studio

## Examples
TBA - see the examples/ folder 

# Contribute !
If you want to contribute to the project you can find us on :
* Our forum 
* Twitter
* Ask for a Slack invite ! https://r-pipeline.slack.com


## Code format 
The pipeline should be written following the google style guide for R : https://google.github.io/styleguide/Rguide.xml

* A lot of refactoring as to be done to comply with the google style guide 
* camelCase for variables PascalCase for functions and objects 
* Comments must follow the Roxygen (https://github.com/klutometis/roxygen) syntax and the code must ortherwise be adequatly commented for anyone to understand how the data is processed and how results are generated. 
* Use long and overly clear names like : CenterOnMeanOfMean(), correlatedVariableNamesArray etc.
 
## Testing
It is essential that every addition should be testable. 
https://cran.r-project.org/web/packages/RUnit/vignettes/RUnit.pdf

```
    # Testing a function that convert celsius to fahrenheit
	c2f <- function(c) return(9/5 * c + 32)

    # A corresponding test function could look like this:
	test.c2f <- function() {
	checkEquals(c2f(0), 32)
	checkEquals(c2f(10), 50)
	checkException(c2f("xx"))
	}
```

## Current functions 
Please see the docs () for a complete description of the functions. 
### Preprocessing
### Filtering
### Dimensionality reduction
### Statistics
### Time series 
### Visualization
### Formatting 
### Export


## Roadmap 
* Discuss methodology with other teams
* Research the open source projects already available to determine which functions are redundant, and what function to import/modify for integration in the package
* Refactor the code 
* Discuss and build robust data-analysis workflows around functions in the package as well as outside the package 
* Preprocessing : Decide on the best practice and how to implement it 
* Time series analysis tools 
* Build an object wrapper (S4 ? S6?) to help in data management / versioning 
* Build I/O functions
* Export functions to XLS, CSV, JSON 
* (Future: Discuss possibilities of making the package self-suficient ?  (no external dependency) - Optimize with compiled lib )
* Add all the R-snippets into functions (https://github.com/jvcasill/R-snippets) 

  

## Readings
* Google Style Guide: https://google.github.io/styleguide/Rguide.xml
* Roxygen: https://github.com/klutometis/roxygen
* Unit testing: https://cran.r-project.org/web/packages/RUnit/vignettes/RUnit.pdf
* R cheat-sheet: https://docs.google.com/document/d/1Se0awz1Jse2S0CS8tOLyug7Cr4wvmLV2be2G8Kv0wlc/edit?usp=sharing

Open source tools: 
* https://www.eternalcoding.com/?p=1981
* https://gun.io/blog/maintaining-an-open-source-project/

Sublime text workflow for R development: 
* https://github.com/randy3k/R-Box
* https://packagecontrol.io/packages/R-snippets
* http://www.kevjohnson.org/using-r-in-sublime-text-3
* https://sublimerepl.readthedocs.io/en/latest/
* https://www.r-bloggers.com/set-up-sublime-text-for-light-weight-all-in-one-data-science-ide/
* https://github.com/randy3k/SendREPL
* https://github.com/alienhard/SublimeAllAutocomplete

Markdown:
* http://daringfireball.net/projects/markdown/syntax
* https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
* https://warpedvisions.org/projects/markdown-cheat-sheet
* rd to HTML : https://stat.ethz.ch/R-manual/R-devel/library/tools/html/Rd2HTML.html


## Other projects of interest
* http://www.martinos.org/mne/stable/index.html
