[![Travis-CI Build Status](https://travis-ci.org/aursiber/ade4TkGUI.svg?branch=master)](https://travis-ci.org/aursiber/ade4TkGUI)
[![AppVeyor Build status](https://ci.appveyor.com/api/projects/status/10pt4g0fvlg96djm/branch/master?svg=true)](https://ci.appveyor.com/project/aursiber/ade4TkGUI/branch/master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ade4TkGUI)](http://cran.r-project.org/package=ade4TkGUI)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/ade4TkGUI)](https://cran.r-project.org/package=ade4TkGUI)


# [ade4TkGUI](http://pbil.univ-lyon1.fr/ade4TkGUI/)

`ade4TkGUI` is a Tcl/Tk GUI for some statistical functions of the `ade4` package and uses the graphical functions of the `adegraphics` package.

The `ade4` package contains Data Analysis functions to analyse Ecological and Environmental data in the framework of Euclidean Exploratory methods. To learn more about `ade4`, see [the web site](http://pbil.univ-lyon1.fr/ADE-4/) and the [GitHub page](https://github.com/sdray/ade4), and for `adegraphics`, see the [GitHub page](https://github.com/sdray/adegraphics).

The `ade4TkGUI` package mixes the advantages of a GUI (ease to use, no need to learn numerous commands) with the possibility to use R expressions in the dialog boxes, to generate understandable R commands, and to manage a session history file.

## Installing `ade4TkGUI`

To install the development version from github:

1. Install the release version of `devtools` from CRAN with `install.packages("devtools")`.

2. Make sure you have a working development environment.
    * **Windows**: Install [Rtools](http://cran.r-project.org/bin/windows/Rtools/).
    * **Mac**: Install Xcode from the Mac App Store.
    * **Linux**: Install a compiler and various development libraries (details vary across different flavors of Linux).
    
3. In a R console :
```r
library(devtools)
install_github("aursiber/ade4TkGUI")
```

The stable version can be installed from CRAN using:
```r
install.packages("ade4TkGUI")
```

You also must install the `ade4` and `adegraphics` packages in a stable version from CRAN or in a development one from GitHub.
```r
install_github("sdray/ade4")
install_github("sdray/adegraphics")
```
or
```r
install.packages("ade4")
install.packages("adegraphics")
```


Once installed, the package can be loaded using:
```r
library("ade4TkGUI")
```

If you do not wish to install the development environments Rtools (Windows) / XCode (Mac), you can get the binary packages here:

* [**Windows**](http://pbil.univ-lyon1.fr/members/thioulouse/bin/windows/)

* [**macOS**](http://pbil.univ-lyon1.fr/members/thioulouse/bin/macosx/)


## Start with `ade4TkGUI`

The core of the package is the `ade4TkGUI()` function which opens this main GUI window :
<br/>
<br/>
<br/>
![fig-figure1](https://cloud.githubusercontent.com/assets/13218953/10664478/03bbcdcc-78c4-11e5-9ee5-2025e5a06242.png)
<br/>
<br/>

Only a limited subset of `ade4` functions is displayed. Less frequently used functions are available through the menus of the menu bar, located at the top of the window.

The `ade4TkGUI()` function takes two arguments, `show` and `history`. The first one determines wether the R commands generated by the GUI should be printed in the console. If the `history` argument is set to TRUE, the commands generated by the GUI are also stored in the `.Rhistory` file, where they can easily be retrieved by the user.


