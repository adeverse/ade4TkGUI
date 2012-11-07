################################
# GUI for s.corcircle function
################################
"dialog.s.corcircle" <- function(show, history)
{
	#op=options()
	#options(warn=-1)
#
# Main dialog window with title
#
	tt <- tktoplevel()
	tkwm.title(tt,"s.corcircle")

    frame1 <- tkframe(tt, relief="groove", borderwidth=2)
    frame2 <- tkframe(tt, relief="groove", borderwidth=2)
    frame3 <- tkframe(tt, relief="groove", borderwidth=2)
    xyframe <- tkframe(frame1, relief="groove", borderwidth=2)
    labframe <- tkframe(frame1, relief="groove", borderwidth=2)
    posframe <- tkframe(frame2, relief="groove", borderwidth=2)
    legframe <- tkframe(frame2, relief="groove", borderwidth=2)
    optframe <- tkframe(frame3, relief="groove", borderwidth=2)
    origframe <- tkframe(frame3, relief="groove", borderwidth=2)
    gridframe <- tkframe(frame3, relief="groove", borderwidth=2)
#
# Variables for text fields
#
	xyvar <- tclVar()
	nxvar <- tclVar(1)
	nyvar <- tclVar(2)
	labvar <- tclVar()
	clabvar <- tclVar(1)
	cpvar <- tclVar()
	cgrvar <- tclVar(1)
	subvar <- tclVar()
	csubvar <- tclVar(1.25)
#
# Checkboxes variables
#
	gridvar <- tclVar(1)
	posvar <- tclVar(1)
	addvar <- tclVar(0)
	fullcircvar <- tclVar(1)
	boxvar <- tclVar(0)
#
# Title
#
	TFrame <- tkframe(tt, relief="groove")
	labh <- tklabel(TFrame, bitmap="questhead")
	tkgrid(tklabel(TFrame,text="Correlation circle", font="Times 18", foreground="red"), labh)
	tkbind(labh, "<Button-1>", function() print(help("s.corcircle")))
	tkpack(TFrame)
#
# Coordinates frame
#
	xy.entry <- tkentry(xyframe, textvariable=xyvar, width=10)
	nx.entry <- tkentry(xyframe, textvariable=nxvar, width=3)
	ny.entry <- tkentry(xyframe, textvariable=nyvar, width=3)
	dfnr.label <- tklabel(xyframe, width=4)
	dfnc.label <- tklabel(xyframe, width=4)
	choosexy.but <- tkbutton(xyframe, text="Set", command=function() choosedf(xy.entry, dfnr.label, dfnc.label))
	tkgrid(tklabel(xyframe, text="- Coordinates -", foreground="blue"), columnspan=5)
	tkgrid(tklabel(xyframe,text="XY coordinates: "), xy.entry, choosexy.but, dfnr.label, dfnc.label)
	tkgrid(tklabel(xyframe,text="X axis col. #: "), nx.entry)
	tkgrid(tklabel(xyframe,text="Y axis col. #: "), ny.entry)
#
# Labels frame
#
	lab.entry <- tkentry(labframe, textvariable=labvar, width=10)
	clab.entry <- tkentry(labframe, textvariable=clabvar, width=10)
	chooselab.but <- tkbutton(labframe, text="Set", command=function() chooselab(tt, dfnr.label, lab.entry))
	tkgrid(tklabel(labframe, text="- Labels -", foreground="blue"), columnspan=3)
	tkgrid(tklabel(labframe,text="Labels : "), lab.entry, chooselab.but)
	tkgrid(tklabel(labframe,text="Label size : "), clab.entry)

	tkpack(xyframe, labframe, side="left")
	
	tkpack(frame1)
#
# Legend frame
#
    tkpack(tklabel(posframe, text="- Legend position -", foreground="blue"), anchor="w")
    tkpack(tkradiobutton(posframe, text="Top left", value=1, variable=posvar), anchor="w")
    tkpack(tkradiobutton(posframe, text="Top right", value=2, variable=posvar), anchor="w")
    tkpack(tkradiobutton(posframe, text="Bottom left", value=3, variable=posvar), anchor="w")
    tkpack(tkradiobutton(posframe, text="Bottom right", value=4, variable=posvar), anchor="w")

	sub.entry <- tkentry(legframe, textvariable=subvar)
	csub.entry <- tkentry(legframe, textvariable=csubvar, width=10)
	tkgrid(tklabel(legframe, text="- Legend -", foreground="blue"), columnspan=2)
	tkgrid(tklabel(legframe,text="Legend string: "), sub.entry)
	tkgrid(tklabel(legframe,text="Legend size: "), csub.entry)

    tkpack(legframe, posframe, side="left", expand=1)

    tkpack(frame2, fill="x")
#
# Options frame
#
	tkgrid(tklabel(optframe, text="- Draw options -", foreground="blue"))
	add.cbut <- tkcheckbutton(optframe,text="Add to plot", variable=addvar)
	tkgrid(add.cbut)
	
	fullcirc.cbut <- tkcheckbutton(optframe,text="Full circle", variable=fullcircvar)
	tkgrid(fullcirc.cbut)
	
	box.cbut <- tkcheckbutton(optframe,text="Draw box", variable=boxvar)
	tkgrid(box.cbut)
	
	tkgrid(tklabel(gridframe, text="- Grid -", foreground="blue"), columnspan=2)
	grid.cbut <- tkcheckbutton(gridframe,text="Draw grid", variable=gridvar)
	cgr.entry <- tkentry(gridframe, textvariable=cgrvar, width=10)
	tkgrid(grid.cbut)
	tkgrid(tklabel(gridframe,text="Grid size : "), cgr.entry)
	
	tkpack(optframe, gridframe, origframe, side="left", expand=1)

	tkpack(frame3, fill="x")
#
# Local variables
#
	vnr=NULL			# Vector of dataframes row numbers
	vnc=NULL			# Vector of dataframes column numbers
	numi=1				# Number of choosed element
	done <- tclVar(0)	# To terminate the dialog
	
################################
# Function to build the command line from dialog widgets
################################
	"build" <- function()
	{
	#
	# Check that the xy data frame is not empty and get its name
	#
		if (tclvalue(xyvar) != "") {
			xy  <- parse(text=tclvalue(xyvar))[[1]]
		} else {
			return(0)
		}
	#
	# Get x and y axis column number
	#
		if (tclvalue(nxvar) != "") {
			nx <- parse(text=tclvalue(nxvar))[[1]]
		} else nx <- 1
		if (tclvalue(nyvar) != "") {
			ny <- parse(text=tclvalue(nyvar))[[1]]
		} else ny <- 2
	#
	# Get labels : row.names(eval(xy, envir=globalenv()))
	#
		if (tclvalue(labvar) != "") {
			lab <- parse(text=tclvalue(labvar))[[1]]
		} else lab <- NULL
		if (tclvalue(clabvar) != "") {
			clab <- parse(text=tclvalue(clabvar))[[1]]
		} else clab <- 1
	#
	# Get cgrid
	#
		if (tclvalue(cgrvar) != "") {
			cgr <- parse(text=tclvalue(cgrvar))[[1]]
		} else cgr <- 1
	#
	# Get legend
	#
		if (tclvalue(subvar) != "") {
			sub <- tclvalue(subvar)
		} else sub <- ""
		if (tclvalue(csubvar) != "") {
			csub <- parse(text=tclvalue(csubvar))[[1]]
		} else csub <- 1.25
	#
	# Get checkboxes state
	#
		gridl <- as.logical(tclObj(gridvar))
		posit <- tclvalue(posvar)
		addl <- as.logical(tclObj(addvar))
		fullcircl <- as.logical(tclObj(fullcircvar))
		boxl <- as.logical(tclObj(boxvar))
		if (posit == 1) possub <- "topleft"
		if (posit == 2) possub <- "topright"
		if (posit == 3) possub <- "bottomleft"
		if (posit == 4) possub <- "bottomright"
	#
	# Make the command line
	#
		if (identical(lab,NULL)) substitute(s.corcircle(dfxy=xy, xax = nx, yax = ny, 
			clabel = clab, grid = gridl, cgrid = cgr, fullcircle = fullcircl, box =boxl,
			sub = sub, csub = csub, possub = possub, add.plot = addl))
    	else  substitute(s.corcircle(dfxy=xy, xax = nx, yax = ny, label = lab, 
			clabel = clab, grid = gridl, cgrid = cgr, fullcircle = fullcircl, box =boxl,
			sub = sub, csub = csub, possub = possub, add.plot = addl))
	}

################################
# Function to reset all dialog elements to default values
################################
	"reset" <- function()
	{
		tclvalue(xyvar) <- ""
		tclvalue(nxvar) <- "1"
		tclvalue(nyvar) <- "2"
		tclvalue(labvar) <- ""
		tclvalue(clabvar) <- "1"
		tclvalue(cgrvar) <- "1"
		tclvalue(subvar) <- ""
		tclvalue(csubvar) <- "1.25"
		tkconfigure(dfnr.label, text="")
		tkconfigure(dfnc.label, text="")
		gridvar <- tclVar(1)
		addvar <- tclVar(0)
	}
	
################################
# Function to draw the graphic
################################
	"drawgraph" <- function()
	{
		#
		# Build and display the command line so that the user can check it
		#
		cmd <- build()
		if (cmd == 0) return(0)
		if (show) {
			#
			# Echoe the command line to the console
			#
			pr1 <- substr(options("prompt")$prompt, 1,2)
			cat(deparse(cmd, width.cutoff = 500), "\n", pr1, sep="")
		}
		#
		# Execute the command
		#
		eval.parent(cmd)
		if (history) rewriteHistory(deparse(cmd, width.cutoff = 500))
	}
#
# Place the three buttons
#
	RCSFrame <- tkframe(tt, relief="groove")
	reset.but <- tkbutton(RCSFrame, text="Reset", command=reset)
	cancel.but <- tkbutton(RCSFrame, text="Dismiss", command=function() tkdestroy(tt))
	submit.but <- tkbutton(RCSFrame, text="Submit", default="active", command=function() drawgraph())
	tkgrid(cancel.but, submit.but, reset.but, ipadx=20)	
	tkpack(RCSFrame)
#
# If window is closed by user, terminate the dialog
#
	tkbind(tt, "<Destroy>", function() tclvalue(done)<-2)
	tkbind(tt, "<KeyPress-Return>", function() drawgraph())
	tkbind(tt, "<KeyPress-Escape>", function() tkdestroy(tt))
#
# User closed the window
#
	if(tclvalue(done)=="2") return()
}
