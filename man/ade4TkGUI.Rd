\name{ade4TkGUI}

\alias{ade4TkGUI}

\title{Tcl/Tk GUI for the ade4 package}

\description{
A Tcl/Tk GUI for some basic functions in the \code{ade4} package.
}

\usage{
ade4TkGUI(show = FALSE, history = FALSE)
}

\arguments{
  \item{show}{a logical value. If \code{TRUE}, \code{R} commands executed in the GUI are echoed to the console.}
  \item{history}{a logical value. If \code{TRUE}, \code{R} commands executed in the GUI are stored in the \code{R}
  session history.}
}

\details{
Right-click on \code{ade4} function buttons to get help on these functions.
You can also click on the questionhead icon in the dialog windows.
This displays the \code{R} help for the corresponding function.\cr

In all dialog windows, entries can either be filled with a \code{Set} button, or typed
directly in the entry field. The \code{Set} button displays a listbox with a list
of available objects in the global environment. This list is restricted to objects
with an adequate type (dataframe, \code{dudi}, factor, etc). Entries can also be filled
directly by typing the desired value. In this case, \code{R} expressions can also be entered,
for example \code{c("red", "green", "blue")}, \code{doubs$mil[1:20,1:5]}, \code{meau$plan$dat}
, or \code{1:20}.

}

\section{Buttons}{

The first row of buttons (\code{- Data sets -}) is used to load data sets,
either from a tab-delimited text file exported from Excel (\code{Read data file}
button), or from the \code{ade4} package built-in datasets (\code{Load a data set} button; 
right-click to get help on the selected dataset).\cr

The second block of buttons (\code{- One table analyses -}) gives access to simple analyses
(like \code{pca}), in which only one table is analysed. 

The third block (\code{- One table with groups -}) is for analyses in which the
rows of the table belong to several groups. This is the case for example  for 
discriminant analysis (\code{discrimin}) and for the \code{within} and \code{between}
analyses (see \code{ade4} documentation).

The fourth block (\code{- Two tables analyses -}) gives access to three analyses
in which the relationships between two data tables are analysed. \code{cca} is the well
known (at least in ecology) canonical correspondence analysis, \code{coinertia} can be seen
as a robust alternative to \code{cca} when the number of samples is low, and \code{pcaiv}
is principal components analyses with respect to instrumental variables (see \code{ade4}
documentation).

The fifth block (\code{- Graphic functions -}) contains three buttons that
launch the dialog windows for three basic graphic functions in \code{ade4} : \code{s.label}
(scatter diagram of a factor map), \code{s.class} (scatter diagram with groups), 
\code{s.value} (scatter diagram with squares proportional to one variable).

Other graphic functions are available in the \code{Graphics} menu : \code{s.arrow} (scatter diagram with arrows), 
\code{s.corcircle} (correlation circle in normed pca), \code{s.class} (scatter diagram with convex hulls, using the 
\code{chullSize} argument), and \code{s.match} (scatter diagram of two paired clouds of points).

The sixth block (\code{- Advanced graphics -}) can be used for several things:

- \code{dudi display} displays a dialog window grouping all the components of a \code{dudi}.
Each of these components is represented by a button which action is to draw a graphical
display of the corresponding component. The axes used to draw theses graphics can be set
by the user. The last row of buttons gives access to particular graphic functions that
can be used according to the \code{dudi} type.

- \code{MCTests} displays a dialog window for computing Monte-Carlo tests
after (e.g.) a \code{between} or \code{coinertia} analysis.

- \code{ordiClust} displays a dialog window for analysing cluster on ordination scores, 
providing a dynamic exploration of the clusters on the factor maps.

}

\section{Menu bar}{

The menu bar at the top of the window can be used to launch the same functions, plus
several others. All theses menus are tear-off.

The \code{File} menu can be used to read and load files and datasets, to edit a dataframe,
and to quit R :

- \code{Read text file} allows to read a data file and store the result in a dataframe.
It can be used to read standard Excel tab-delimited text files (with variable names on the
first row and sample names in the first column, leaving the first cell empty).
The name "clipboard" can be used to read a data table just copied from an Excel data sheet
(so it is not necessary to save the data in a text file).

- \code{Load data set} allows to load a data set from the \code{ade4} package.

- \code{Edit data frame} can be used to edit a dataframe.

- \code{Quit R} opens a dialog window to ask if the environment should be saved before
quitting R.

The \code{Windows} menu allows to manage several graphical windows :

- \code{New graphic window} opens a new graphic window and makes it the active window.
This is usefull to compare easily several graphics.

- \code{Change graphic window} changes the active window (i.e., the one into
which the next graphic will be drawn).

- \code{Save graphic window} saves the graphic drawn in the active window into
a disk file in several formats (postscript, pdf, etc).

The \code{1table}, \code{1table+groups}, \code{2tables}, and \code{Graphics}
menus give access to several other analysis methods and graphics.

}

\section{History and command line echoing}{

If the \code{show} argument is set to TRUE, then all the commands executed in the GUI are echoed
to the console. This is handy for complex commands, as it allows to check the exact syntax of the
command that was executed.

If the \code{history} argument is set to TRUE, then the commands executed in the GUI (the same as
the commands which are echoed to the console) are stored in the R session history buffer. For each
command, the history buffer is written to a temporary disk file with \code{savehistory()}, the
command is appended to this file, and the file is reread in memory with \code{loadhistory()}.

The value (\code{TRUE} or \code{FALSE}) of both arguments is recalled in the title of the window (ade4TkGUI), so that
different instances of the GUI launched simultaneously with different values for these arguments
can be easily recognized.
}

\author{
Jean Thioulouse \email{jean.thioulouse@univ-lyon1.fr} \cr
Stephane Dray \email{stephane.dray@univ-lyon1.fr}
}

\examples{
\dontrun{
## Start the GUI
ade4TkGUI()
}
}
\keyword{multivariate}
