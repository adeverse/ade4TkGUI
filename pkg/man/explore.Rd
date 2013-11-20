\name{explore}

\alias{explore}

\title{Factor map dynamic exploration function}

\description{
This function provides graphical tools for dynamically exploring the factor maps obtained in
multivariate data analysis. Many operations like zooming, panning, and searching are possible.
This is particularly usefull for large datasets, as it is very difficult to identify one particular
item on a cluttered factor map.
}

\usage{
explore(call.graph, scale.graph=1.3)
}

\arguments{
  \item{call.graph}{a call to a graphical function of the ade4 package (\code{s.label}, \code{s.arrow}, \code{s.class}, or \code{s.value}).}
  \item{scale.graph}{a scale factor used to resize the drawing. This is particularly usefull for small laptop screens.}
}

\details{
The main window displays the factor map corresponding to the first argument function call.
Under this graph, three buttons can be used to close the window, save the graph in a file,
and redraw the original graph. A scale widget allows to zoom in or out on the graph (this
operation can be slow when the number of points is high).\cr

In the top-right part of the main window, a small graph shows the density distribution
of points on the factor map, and a red rectangle shows which part of the graph has been
zoomed in.\cr

Under this small graph, four radiobuttons can be used to set the operation mode (see below
\code{Operation modes}). The \code{Show commands in R console} checkbox button allows to
echoe the commands to the console.\cr

The \code{Find a point} panel gives a list of the labels. Clicking on one label redraws the
main graph, centering it around the selected item and displaying it in red. This is handy
to locate rapidly a particular item. The text field just above the listbox can be used to
type a character string. The list of labels is then searched dynamically as characters are
typed and the list is restricted to labels matching the typed string.\cr

The \code{Label options} panel is used only when in \code{Labels} mode (see below \code{Operation modes}).\cr

At the bottom-right of the window, the \code{Information} panel lists item labels and
x & y coordinates in a listbox.\cr
}

\section{Operation modes}{

\code{Zoom mode}
is the default mode: clicking in the main graph window increases the zoom factor
around the coordinates of the click. If a drag operation is performed (button press, mouse move,
button release), then the zone defined by this operation is zoomed in.\cr

\code{Pan mode} just moves the center of the main graph to the coordinates where the click occured.\cr

\code{Labels mode} gives access to the four buttons of the \code{label options} panel. The two
buttons \code{Add all} and \code{Remove all} remove all the labels (leaving only small black
dots) or set them back in the graph. The \code{Select...} button display a listbox with all the
labels, allowing to select some of them, which are then displayed on the factor map. The
\code{By values...} button also allows to select labels, but based on the x and y numerical
ranges.\cr

\code{Identify mode} is usefull when labels are not displayed. It gives the label of the nearest
item after a mouse click in the main graph window. Typical use is after zooming to separate
cluttered points.\cr
}


\author{
Jean Thioulouse \email{jean.thioulouse@univ-lyon1.fr} \cr
Stephane Dray \email{stephane.dray@univ-lyon1.fr}
}

\examples{
\dontrun{
## Start the GUI
data(meau)
acpmil <- dudi.pca(meau$mil, scannf=F)
explore(s.label(acpmil$li))
}
}
\keyword{multivariate}
