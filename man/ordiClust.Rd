\name{ordiClust}

\alias{ordiClust}

\title{Ordination and Cluster analysis}

\description{
This function does cluster analysis on ordination scores, providing a dynamic exploration
of the clusters on the factor maps. As a first step, an ordination method computes the row scores
of the input data table. In a second step, cluster analysis is used on these row scores to obtain
groups. The dudi and the factor are available in the global environment under the names
\code{ordiClust.dudi} and \code{ordiClust.factor}.
}

\usage{
ordiClust(datatab=NULL, hscalef=1.2, vscalef=1.2, maxgr=20)
}

\arguments{
  \item{datatab}{a dataframe containing the data table to analyse, or directly a dudi.}
  \item{hscalef}{horizontal scale factor used to resize the drawing in the tkrplot window.}
  \item{vscalef}{vertical scale factor used to resize the drawing in the tkrplot window.}
  \item{maxgr}{maximum number of groups displayed on the ordination factor map.}
}

\details{
The GUI is divided in a graph panel on the right and several dialog panels on the left.\cr
\cr
Under the graph panel, a scale widget can be used to set the number of groups, and 
three buttons allow to change the graphical parameters (ellipses or convex hulls,
and color or black & white). The \code{Submit} button under the graph (or the return key)
draws the groups on the factor map. \code{Up arrow} and \code{Down arrow} keys (or the scale
widget) can be used to increase or decrease the number of groups. Choosing another clustering
algorithm or another distance automatically updates the graph, allowing easy comparisons. The
\code{Dismiss} button at the bottom of the window closes the ordiClust window, and the \code{Save}
button can be used to save the current graphic in a disk file.\cr
\cr
From top to bottom the left panels are :\cr
\cr
\code{Input data frame} : this is the data table that will be analysed. The user can set it
with the \code{Set} button.\cr
\cr
\code{Input dudi} : this is directly the dudi that will be analysed. The user can set it
with the \code{Set} button. No ordination method should be used when a dudi is selected here:
cluster analysis will be done on the row scores of this dudi.\cr
\cr
\code{Ordination method} : the user can choose between principal component analysis (centered
or normed), correspondence analysis, and multiple correspondence analysis. After computations
are finished, the eigenvalues bar chart is displayed in the right panel. The user can then
set the number of principal components (axes) on which cluster analysis will be done
with the \code{Set} button (defaults to 2). If no number is chosen, then all axes are kept.\cr
\cr
\code{Ordination graph} : draw the factor maps for rows or columns. Axis numbers can be
chosen (default is 1 for X axis and 2 for Y axis).\cr
\cr
\code{Cluster analysis - Distance} : choose the way distances are computed among rows. See help("dist").
The \code{dudi} option is a special case: computations are done with the \code{dist.dudi} function of the
\code{ade4} package (i.e., for a normed PCA this is the Euclidean distance computed on normed variables).\cr
\cr
\code{Cluster analysis - Cluster method} : choose the clustering algorithm. See help("hclust").
The \code{Submit} button starts the computations and the hierarchical tree is displayed in the
right panel. Choosing another algorithm or another distance automatically updates the graph,
allowing easy comparisons.\cr
\cr
\code{Number of groups} : this is the number of groups used to cut the tree computed by cluster analysis.
The \code{Cut tree} button cuts the tree and draws a red line on the right panel. \code{Up arrow}
and \code{Down arrow} keyboard keys (or the scale widget) can be used to increase or decrease
this value respectively.\cr
\cr
\code{Level height} : height of the cut level in the tree computed by cluster analysis.
The \code{Cut tree} button cuts the tree and draws a red line on the right panel.\cr
\cr
\code{Inertia ratio} : this is the between group to total inertia ratio (percentage of explained variance).
The \code{Draw curve} button draws the curve of this ratio as a function of the number
of groups. Steps on this curve can be used to choose the number of groups. The red cross
on this graph gives the number of groups and the corresponding percentage of between-groups
inertia. \code{Up arrow} and \code{Down arrow} keys (or the scale widget) can be used to increase or
decrease the number of groups. Choosing another clustering algorithm or another distance
automatically updates the graph, allowing easy comparisons.\cr
\cr
\code{BGA MCTest p-value} : p-value of the Between Groups Analysis Monte-Carlo test (BGA, ade4
\code{between} function).\cr
\cr
The dudi and the factor used to draw the graphs are available in the global environment,
withe the names \code{ordiClust.dudi} and \code{ordiClust.factor}.
}

\author{
Jean Thioulouse \email{jean.thioulouse@univ-lyon1.fr} \cr
Stephane Dray \email{stephane.dray@univ-lyon1.fr}
}

\examples{
\dontrun{
## Start the GUI form the console
data(meau) 
ordiClust(dudi.pca(meau$mil,scan=F))
}
}
\keyword{multivariate}
