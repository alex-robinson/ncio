

myfigure <- function(fldr=".",file="Rplot",date=TRUE,type="pdf",engine="cairo",
                     width=NULL,height=NULL,units="mm",asp=1,pointsize=12,res=300,
                     cex=1,cex.lab=1,cex.axis=1,bg="white",onefile=FALSE)
{
  
    # Make filename
    file = paste(file,".",type,sep="")
    if (date == TRUE) file = paste(today,"_",file,sep="")
    file = file.path(fldr,file)

    host = system("hostname",intern=TRUE)
    os   = system("uname",intern=TRUE)

    # If running on a mac, make sure engine is quartz!
    if ( os == "Darwin" ) engine = "quartz"

    # Determine width/heights in inches
    if ( is.null(width) & is.null(height) ) {  # Use default height, determine win via asp
        width  = 189  # Default width for pointsize 12
        height = width/asp
    } else if ( is.null(height) ) {  # only width specified, determine height
        height = width/asp
    } else if ( is.null(width) ) {  # only height specified, determine width
        width = asp*height
    } else {                    # height and width specified, determine asp
        asp = width/height
    }

    # Convert quantities if input was not inches
    cat(type,":",file,"\n")
    cat("width=",width,", height=",height," (",units,".) \n",sep="")
    conv = 1.0
    if ( units == "mm" ) conv = 0.0393700787
    if ( units == "cm" ) conv = 0.393700787
    hin = height*conv
    win = width*conv 
    #cat("width =",win,", height =",hin," (in.)\n")

    if (FALSE & os %in% c("Darwin") & type %in% c("png","jpg","tiff","pdf","ps")) {
        
        cat("Quartz plotting","\n")
        quartz(file=file,type=type,width=win,height=hin,pointsize=pointsize,dpi=res)

    } else if ( type == "png" ) {

        cat("engine = ",engine,"\n")
        png(file,width=win,height=hin,units="in",pointsize=pointsize,res=res,type=engine)

    } else if ( type == "jpg" ) {

        jpeg(file,width=win,height=hin,units="in",pointsize=pointsize,res=res,type=engine)

    } else if ( type == "tiff" ) {

        tiff(file,width=win,height=hin,units="in",pointsize=pointsize,res=res,type=engine)

    } else if ( type == "pdf" ) {

        if (engine %in% c("cairo","cairo1")) {
            cat("**cairo_pdf","\n")
            cairo_pdf(file,width=win,height=hin,pointsize=pointsize,onefile=onefile)
        } else {
            pdf(file,width=win,height=hin,pointsize=pointsize,onefile=onefile)
        }

    } else if ( type == "svg" ) {

        svg(file,width=win,height=hin,pointsize=pointsize)

    } else if ( type == "fig" ) {

        xfig(file,width=win,height=hin,pointsize=pointsize)

    } else {

        cairo_ps(file,width=win,height=hin,pointsize=pointsize)

    }

    par(bg=bg,cex=cex,cex.axis=cex.axis,cex.lab=cex.lab,tcl=0.2,mgp=c(2.5,0.3,0),las=1)

    return(win)
}

alpha <- function(col,percent=50)
{
  c <- rep("#000000",length(col))
  for (i in 1:length(col)) {
    cc <- col2rgb(col[i])/255
    c[i] <- rgb(t(cc),alpha=percent/100)
  }
  
  return(c)
}
