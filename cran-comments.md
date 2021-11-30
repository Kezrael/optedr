## Resubmission
This is a resubmission. In this version I have:

* Removed examples for unexported functions

* Removed \dontrun{} from examples as it was not needed

* Wrapped interactive functions in if(interactive()){}

* Removed option setting (options(warn = -1)) and used suppressWarnings() where needed instead.


## R CMD check results
There were no ERRORs, WARNINGs or NOTEs.



## Downstream dependencies
New submission, there are currently no downstream dependencies for this package.
