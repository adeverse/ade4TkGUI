################################
# GUI for s.value function
################################
"dialog.s.value" <- function(show, history)
{
	#op=options()
	#options(warn=-1)
#
# Main dialog window with title
#
	tt <- tktoplevel()
	tkwm.title(tt,"s.value")

    frame1 <- tkframe(tt, relief="groove", borderwidth=2)
    frame2 <- tkframe(tt, relief="groove", borderwidth=2)
    frame3 <- tkframe(tt, relief="groove", borderwidth=2)
    frame4 <- tkframe(tt, relief="groove", borderwidth=2)
    xyframe <- tkframe(frame1, relief="groove", borderwidth=2)
    symframe <- tkframe(frame1, relief="groove", borderwidth=2)
    limframe <- tkframe(frame2, relief="groove", borderwidth=2)
    posframe <- tkframe(frame2, relief="groove", borderwidth=2)
    legframe <- tkframe(frame2, relief="groove", borderwidth=2)
    optframe <- tkframe(frame3, relief="groove", borderwidth=2)
    origframe <- tkframe(frame3, relief="groove", borderwidth=2)
    gridframe <- tkframe(frame3, relief="groove", borderwidth=2)
    miscframe <- tkframe(frame4, relief="groove", borderwidth=2)
    methframe <- tkframe(frame4, relief="groove", borderwidth=2)
#
# Variables for text fields
#
	xyvar <- tclVar()
	nxvar <- tclVar(1)
	nyvar <- tclVar(2)
	nrvar <- tclVar()
	valvar <- tclVar()
	csizevar <- tclVar(1)
	zmaxvar <- tclVar()
	pchvar <- tclVar(20)
	cpvar <- tclVar()
	clegendvar <- tclVar(0.75)
	xl1var <- tclVar()
	xl2var <- tclVar()
	yl1var <- tclVar()
	yl2var <- tclVar()
	cgrvar <- tclVar(1)
	orxvar <- tclVar(0)
	oryvar <- tclVar(0)
	subvar <- tclVar()
	csubvar <- tclVar(1.25)
	neigvar <- tclVar()
	cneigvar <- tclVar(2)
	pmvar <- tclVar()
	contvar <- tclVar()
	areavar <- tclVar()
#
# Checkboxes variables
#
	methvar <- tclVar(1)
	gridvar <- tclVar(1)
	axesvar <- tclVar(1)
	origvar <- tclVar(1)
	posvar <- tclVar(1)
	addvar <- tclVar(0)
#
# Title
#
	TFrame <- tkframe(tt, relief="groove")
	labh <- tklabel(TFrame, bitmap="questhead")
	tkgrid(tklabel(TFrame,text="Values", font="Times 18", foreground="red"), labh)
	tkbind(labh, "<Button-1>", function() print(help("s.value")))
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
# Symbols frame
#
	pch.entry <- tkentry(symframe, textvariable=pchvar, width=10)
	cp.entry <- tkentry(symframe, textvariable=cpvar, width=10)
	clegend.entry <- tkentry(symframe, textvariable=clegendvar, width=10)
	tkgrid(tklabel(symframe, text="- Symbols -", foreground="blue"), columnspan=3)
	tkgrid(tklabel(symframe,text="Character #: "), pch.entry)
	tkgrid(tklabel(symframe,text="Char. size : "), cp.entry)
	tkgrid(tklabel(symframe,text="Legend size : "), clegend.entry)

	tkpack(xyframe, symframe, side="left")
	
	tkpack(frame1)
#
# Values frame
#
	tkgrid(tklabel(methframe, text="- Values -", foreground="blue"), columnspan=3)
	chooseval.but <- tkbutton(methframe, text="Set", command=function() chooseval(tt, dfnr.label, val.entry))
	val.entry <- tkentry(methframe, textvariable=valvar, width=10)
	csize.entry <- tkentry(methframe, textvariable=csizevar, width=10)
	zmax.entry <- tkentry(methframe, textvariable=zmaxvar, width=10)
	tkgrid(tklabel(methframe,text="Values : "), val.entry, chooseval.but)
	tkgrid(tklabel(methframe,text="Size coeff. : "), csize.entry)
	tkgrid(tklabel(methframe,text="Z max. : "), zmax.entry)
    tkgrid(tkradiobutton(methframe, text="B&W squares", value=1, variable=methvar), columnspan=3)
    tkgrid(tkradiobutton(methframe, text="Grey levels", value=2, variable=methvar), columnspan=3)
#
# Misc frame
#
	neig.entry <- tkentry(miscframe, textvariable=neigvar, width=10)
	cneig.entry <- tkentry(miscframe, textvariable=cneigvar, width=3)
	pm.entry <- tkentry(miscframe, textvariable=pmvar, width=10)
	cont.entry <- tkentry(miscframe, textvariable=contvar, width=10)
	area.entry <- tkentry(miscframe, textvariable=areavar, width=10)

	chooseneig.but <- tkbutton(miscframe, text="Set", command=function() chooseneig(neig.entry))
	choosepm.but <- tkbutton(miscframe, text="Set", command=function() choosepm(pm.entry))
	choosecont.but <- tkbutton(miscframe, text="Set", command=function() choosecont(cont.entry))
	choosearea.but <- tkbutton(miscframe, text="Set", command=function() choosearea(area.entry))

    tkgrid(tklabel(miscframe, text="- Misc. options -", foreground="blue"), columnspan=3)
	tkgrid(tklabel(miscframe,text="Neighbouring relation : "), neig.entry, chooseneig.but)
	tkgrid(tklabel(miscframe,text="Neighbouring size : "), cneig.entry)
	tkgrid(tklabel(miscframe,text="Pixmap : "), pm.entry, choosepm.but)
	tkgrid(tklabel(miscframe,text="Contour : "), cont.entry, choosecont.but)
	tkgrid(tklabel(miscframe,text="Area : "), area.entry, choosearea.but)

	tkpack(methframe, miscframe, side="left", expand=1)

	tkpack(frame4, fill="x")
#
# Limits frame
#
	xl1.entry <- tkentry(limframe, textvariable=xl1var, width=10)
	xl2.entry <- tkentry(limframe, textvariable=xl2var, width=10)
	yl1.entry <- tkentry(limframe, textvariable=yl1var, width=10)
	yl2.entry <- tkentry(limframe, textvariable=yl2var, width=10)
	tkgrid(tklabel(limframe, text="- Limits -", foreground="blue"), columnspan=2)
	tkgrid(tklabel(limframe,text="X min : "), xl1.entry)
	tkgrid(tklabel(limframe,text="X max : "), xl2.entry)
	tkgrid(tklabel(limframe,text="Y min : "), yl1.entry)
	tkgrid(tklabel(limframe,text="Y max : "), yl2.entry)
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


    tkpack(limframe, legframe, posframe, side="left", expand=1)

    tkpack(frame2, fill="x")
#
# Options frame
#
	axes.cbut <- tkcheckbutton(optframe,text="Draw axes", variable=axesvar)
	add.cbut <- tkcheckbutton(optframe,text="Add to plot", variable=addvar)
	tkgrid(tklabel(optframe, text="- Draw options -", foreground="blue"))
	tkgrid(axes.cbut)
	tkgrid(add.cbut)
	
	orig.cbut <- tkcheckbutton(origframe,text="Include origin", variable=origvar)
	orx.entry <- tkentry(origframe, textvariable=orxvar, width=10)
	ory.entry <- tkentry(origframe, textvariable=oryvar, width=10)
	tkgrid(tklabel(origframe, text="- Origin -", foreground="blue"), columnspan=2)
	tkgrid(orig.cbut)
	tkgrid(tklabel(origframe,text="X Origin : "), orx.entry)
	tkgrid(tklabel(origframe,text="Y Origin : "), ory.entry)

	grid.cbut <- tkcheckbutton(gridframe,text="Draw grid", variable=gridvar)
	cgr.entry <- tkentry(gridframe, textvariable=cgrvar, width=10)
	tkgrid(tklabel(gridframe, text="- Grid -", foreground="blue"), columnspan=2)
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
	# Get values
	#
		if (tclvalue(valvar) != "") {
			val <- parse(text=tclvalue(valvar))[[1]]
		} else {
			nr <- as.numeric(tkcget(dfnr.label, "-text"))
			val <- rep(1, nr)
		}
		if (tclvalue(csizevar) != "") {
			csize <- parse(text=tclvalue(csizevar))[[1]]
		} else csize <- 1
	#
	# Get points char
	#
		if (tclvalue(pchvar) != "") {
			pch <- parse(text=tclvalue(pchvar))[[1]]
		} else pch <- 20
		if (tclvalue(cpvar) != "") {
			cp <- parse(text=tclvalue(cpvar))[[1]]
		} else cp <- 0
	#
	# Get zmax
	#
		if (tclvalue(zmaxvar) != "") {
			zmax <- parse(text=tclvalue(zmaxvar))[[1]]
		} else zmax <- NULL
	#
	# Get csize
	#
		if (tclvalue(csizevar) != "") {
			csize <- parse(text=tclvalue(csizevar))[[1]]
		} else csize <- 1
	#
	# Get clegend
	#
		if (tclvalue(clegendvar) != "") {
			clegend <- parse(text=tclvalue(clegendvar))[[1]]
		} else clegend <- 0.75
	#
	# Get x and y lim
	#
		if (tclvalue(xl1var) != "") {
			xl1 <- parse(text=tclvalue(xl1var))[[1]]
		} else xl1 <- NULL
		if (tclvalue(xl2var) != "") {
			xl2 <- parse(text=tclvalue(xl2var))[[1]]
		} else xl2 <- NULL
		if (tclvalue(yl1var) != "") {
			yl1 <- parse(text=tclvalue(yl1var))[[1]]
		} else yl1 <- NULL
		if (tclvalue(yl2var) != "") {
			yl2 <- parse(text=tclvalue(yl2var))[[1]]
		} else yl2 <- NULL
	#
	# Get cgrid
	#
		if (tclvalue(cgrvar) != "") {
			cgr <- parse(text=tclvalue(cgrvar))[[1]]
		} else cgr <- 1
	#
	# Get origin
	#
		if (tclvalue(orxvar) != "") {
			orx <- parse(text=tclvalue(orxvar))[[1]]
		} else orx <- 0
		if (tclvalue(oryvar) != "") {
			ory <- parse(text=tclvalue(oryvar))[[1]]
		} else ory <- 0
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
	# Get neighbouring relationship
	#
		if (tclvalue(neigvar) != "") {
			neig <- parse(text=tclvalue(neigvar))[[1]]
		} else neig <- NULL
		if (tclvalue(cneigvar) != "") {
			cneig <- parse(text=tclvalue(cneigvar))[[1]]
		} else cneig <- 2
	#
	# Get pixmap
	#
		if (tclvalue(pmvar) != "") {
			pm <- parse(text=tclvalue(pmvar))[[1]]
		} else pm <- NULL
	#
	# Get contour
	#
		if (tclvalue(contvar) != "") {
			cont <- parse(text=tclvalue(contvar))[[1]]
		} else cont <- NULL
	#
	# Get area
	#
		if (tclvalue(areavar) != "") {
			area <- parse(text=tclvalue(areavar))[[1]]
		} else area <- NULL
	#
	# Get checkboxes state
	#
		gridl <- as.logical(tclObj(gridvar))
		axesl <- as.logical(tclObj(axesvar))
		origl <- as.logical(tclObj(origvar))
		posit <- tclvalue(posvar)
		addl <- as.logical(tclObj(addvar))
		if (posit == 1) possub <- "topleft"
		if (posit == 2) possub <- "topright"
		if (posit == 3) possub <- "bottomleft"
		if (posit == 4) possub <- "bottomright"
		meth <- tclvalue(methvar)
		if (meth == 1) method <- "squaresize"
		if (meth == 2) method <- "greylevel"
	#
	# Make the command line
	#
		if (is.data.frame(eval(val))) {
			cmd <- paste("if (sqrt(ncol(", deparse(val), ")) == round(sqrt(ncol(", deparse(val),
				"))) ) ngr <- sqrt(ncol(", deparse(val), ")) else ngr <- sqrt(ncol(", deparse(val),
				"))+1;par(mfrow=c(ngr,ngr));for(i in 1:ncol(", deparse(val), ")) s.value(", deparse(xy),
				",", deparse(val), "[,i], ", deparse(nx), ",", deparse(ny), ", sub=names(", deparse(val),
				")[i], csub=2, clegend=2, cgrid=2, xlim=c(", deparse(xl1), ",", deparse(xl2),
				"), ylim=c(", deparse(yl1), ",", deparse(yl1),"), method = \"", method, "\");par(mfrow=c(1,1))", sep="")
			parse(text=cmd)
		} else {
			substitute(s.value(dfxy=xy, z=val, xax = nx, yax = ny, method = method,
				zmax = zmax, csize = csize, pch = pch, cpoint = cp, clegend = clegend,
				neig = neig, cneig = cneig, xlim = c(xl1, xl2), ylim = c(yl1, yl2), grid = gridl, 
				addaxes = axesl, cgrid = cgr, include.origin = origl, origin = c(orx, ory), 
				sub = sub, csub = csub, possub = possub, pixmap = pm, 
				contour = cont, area = area, add.plot = addl))
		}
	}

################################
# Function to reset all dialog elements to default values
################################
	"reset" <- function()
	{
		tclvalue(xyvar) <- ""
		tclvalue(nxvar) <- "1"
		tclvalue(nyvar) <- "2"
		tclvalue(valvar) <- ""
		tclvalue(csizevar) <- "1"
		tclvalue(zmaxvar) <- ""
		tclvalue(clegendvar) <- "0.75"
		tclvalue(pchvar) <- "20"
		tclvalue(cpvar) <- ""
		tclvalue(xl1var) <- ""
		tclvalue(xl2var) <- ""
		tclvalue(yl1var) <- ""
		tclvalue(yl2var) <- ""
		tclvalue(cgrvar) <- "1"
		tclvalue(orxvar) <- "0"
		tclvalue(oryvar) <- "0"
		tclvalue(subvar) <- ""
		tclvalue(csubvar) <- "1.25"
		tclvalue(neigvar) <- ""
		tclvalue(cneigvar) <- "2"
		tclvalue(pmvar) <- ""
		tclvalue(contvar) <- ""
		tclvalue(areavar) <- ""
		tkconfigure(dfnr.label, text="")
		tkconfigure(dfnc.label, text="")
		methvar <- tclVar(0)
		gridvar <- tclVar(1)
		axesvar <- tclVar(1)
		origvar <- tclVar(1)
		ptlvar <- tclVar(1)
		ptrvar <- tclVar(0)
		pblvar <- tclVar(1)
		pbrvar <- tclVar(0)
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
		dcmd <- deparse(cmd, width.cutoff = 500)
		tcmd <- paste(cmd, sep="", collapse="; ")
		
		#if (cmd == 0) return(0)

		if (show) {
			#
			# Echoe the command line to the console
			#
			pr1 <- substr(options("prompt")$prompt, 1,2)
			if (length(grep("expression", dcmd, fixed=TRUE)) == 0)
				cat(dcmd, "\n", pr1, sep="")
			else
				cat(tcmd, "\n", pr1, sep="")
		}
		#
		# Execute the command
		#
		eval.parent(cmd)
		if (length(grep("expression", dcmd, fixed=TRUE)) == 0) {
		# cmdlist <<- c(cmdlist, cmd)
		assign("cmdlist", c(get("cmdlist", envir=.GlobalEnv), cmd), envir=.GlobalEnv)
			if (history) rewriteHistory(deparse(cmd, width.cutoff = 500))
		} else {
			if (history) rewriteHistory(tcmd)
		}
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
