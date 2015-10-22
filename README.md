[![Travis-CI Build Status](https://travis-ci.org/aursiber/ade4TkGUI.svg?branch=master)](https://travis-ci.org/aursiber/ade4TkGUI)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/y3771xk9a4obepas/branch/master?svg=true)](https://ci.appveyor.com/project/aursiber/ade4TkGUI/branch/master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ade4TkGUI)](http://cran.r-project.org/package=ade4TkGUI)


# ade4TkGUI
*ade4TkGUI* is a Tcl/Tk GUI for some basic functions in the *ade4 package


Installing *ade4TkGUI*
-------------

To install the development version from github:

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Make sure you have a working development environment.
    * **Windows**: Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).
    * **Mac**: Install Xcode from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).
    
Then:

```r
library(devtools)
install_github("sdray/ade4TkGUI")
```

The stable version can be installed from CRAN using:

```r
install.packages("ade4TkGUI")
```

Once installed, the package can be loaded using:

```r
library("ade4TkGUI")
```

