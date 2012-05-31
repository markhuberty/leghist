

# Directions to create a leghist package

# in durkheim 
ssh -p 2222 sanders@durkheim.berkeley.edu
# First git pull at ~/.git/leghist if not updated.

# create package skeleton, force must = TRUE if you're overwriting.
package.skeleton("leghist",code_files=c("~/.git/leghist/code/leg_functions.R",
                                    "~/.git/leghist/code/leghist-package.R"), 
                 force=TRUE)
# Note the the package skelton will be created wherever you currently are in.
# If you want to say where the package will be create, you can use the
# path = <filepath> argument. (Default = ".")

library(roxygen2)

roxygenise("~/.git/leghist/code/leghist",overwrite=TRUE)
# Note that this is the roxygen2 function. roxygenize has different defaults.

# exit R

# In durkheim, go to ~/.git/leghist/code/leghist
# run:
emacs DESCRIPTION
# replace the current description with some variant of the following:

Package: leghist
Maintainer: Santa Claus <notsureyet@berkeley.edu>
License: GPL-2
Title: The Leghist Package
Type: Package
LazyLoad: yes
Author: Mark Huberty, Hillary Sanders
Description: leghist provides functionality within the R programming language to automate the textual evolution of legislation from its introduction as a bill, through the amendment process, until finalization.
Version: 1.0
Date: 2012-05-28
Collate: 'leg_functions.R' 'leghist-package.R'
Depends: tm, topicmodels, stringr, lsa, Matrix, RWeka, gdata, igraph, plyr, catspec, foreach, RecordLinkage

# Save and quit emacs. Then in durkheim, delete the "inst" and 
# "Read-and-delete-me" files. Then run:

cd ..
R CMD check leghist
R CMD build leghist
R CMD INSTALL leghist


# And in R:
install.packages("~/.git/leghist/code/leghist_1.0.tar.gz",repos=NULL,type="source")

# current warnings:
# Some function arguments need roxy comments
# note  :  LevenshteinDist: no visible binding for global variable ‘x’


#########################
## IF YOU WANT TO EDIT ##
#########################

To edit, modify the ~leghist/R .R files and use roxygenise() to update.

Basically everything works as you think it would. Namespace and Rd. files get properly updated.

One note: if you delete a function in the source code files, it will still show up in the man folder, but you cannot open it, as it will just say "no such fie or directory"

If you want to create a completely new file, you have to delete your 
directory first, or else even if package.skeleton (force = TRUE) is used, 
nothing will be overwritten with the new source code. The old source code will be used. If a new file is created, you'll have to create a new "DESCRIPTION" file, as well as delete the "Read-and-delete-me file", and the "inst" file.'







