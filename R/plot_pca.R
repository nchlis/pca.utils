#' Plot PCA results
#'
#'inputs:
#'@param x: data frame, matrix or prcomp object to plot.
#'@param npcs: numeric vector indicating PCs to plot, defaults to c(1,2). If length(npcs)>3, only the first 3 elements will be used.
#'@param xlab: xlabel, see \link[graphics]{par}.
#'@param ylab: ylabel, see \link[graphics]{par}, see \link[graphics]{par}.
#'@param zlab: zlabel, see \link[rgl]{plot3d}.
#'@param main: title for the plot, see \link[graphics]{par}. Defaults to 'PCA results'.
#'@param pch: numeric vector of plotting characters, see \link[graphics]{par}.
#'@param col: vector of plotting character colors, see \link[graphics]{par}.
#'@param bg: vector of plotting character background colors, see \link[graphics]{par}.
#'@param cex: plotting character size. Defaults to 1 for 2D and to 7 for 3D plots.
#'@param pal: color palette. Defaults to the colorblind friendly RColorBrewer::brewer.pal(8,"Dark2"). Setting pal to NULL, will lead to the
#'default R colors being used in the plots.
#'@param add.legend: add a legend to the plot, defaults to FALSE.
#'@param legend.position: , argument to \link[graphics]{legend}. Position of the legend in the plot, defaults to 'bottomleft'.
#'@param legend.text: character vector to appear in the legend.
#'@param legend.col: color of legend elements, defaults to unique(col)
#'@param legend.bg: background color of legend elements, defaults to unique(bg)
#'
#'@return a list with class "prcomp", containing the same elements as \link[stats]{prcomp}:
#'\itemize{
#'  \item var_explained: numeric vector indicating the varince explained by each PC
#'}
#'
#'@examples
#'
#' data(iris)
#'
#' # 2D PCA
#' plot_pca(x=iris[1:4],col=iris$Species)
#'
#' # 3D PCA
#' plot_pca(x=iris[1:4],npcs=3,col=iris$Species)
#'
#'@export
plot_pca = function(x, npcs=c(1,2), xlab=NULL, ylab=NULL, zlab=NULL, main='PCA results', pch=NULL,col=NULL, bg=NULL,cex=NULL,
                    pal=RColorBrewer::brewer.pal(8,"Dark2"),add.legend=FALSE, legend.position='bottomleft', legend.text=NULL, legend.col = NULL, legend.bg = NULL)
{
  if(class(x)=='prcomp')
  {
    pc=x
  }else
  {
    pc=stats::prcomp(as.matrix(x))
  }

  if(is.null(col)==TRUE){col=rep(1,NROW(x))}
  if(class(col)=='factor'){col=as.numeric(col)}#convert to numeric
  if(class(bg)=='factor'){bg=as.numeric(bg)}#convert to numeric
  if((is.null(col)==F)&(is.null(bg)==T)){bg=col}#set bg to save value as col, if only col has been provided

  #a palette has been provided
  if(is.null(pal)==FALSE)
  {
    col=pal[col]
    bg=pal[bg]
  }

  #calculate the explained variance by the principal components
  var_explained=pc$sdev^2 / sum(pc$sdev^2)
  var_explained_percentage=round(100*var_explained,digits=2)

  if(length(npcs)==1)#if only a single number is given
  {
    npcs=1:npcs
  }
  if(length(npcs)>3)#only plot up to 3 Principal Componets
  {
    npcs=npcs[1:3]
  }

  #set default values if none are provided by the user
  if(is.null(xlab)){xlab=paste0('PC',npcs[1],': ',var_explained_percentage[1],'%')}
  if(is.null(ylab)){ylab=paste0('PC',npcs[2],': ',var_explained_percentage[2],'%')}
  if(is.null(pch)){pch=rep(21,length(npcs))}
  if(is.null(col)){col=1}
  if(is.null(bg)){bg=1}
  if(is.null(legend.text)){legend.text=paste('class',1:length(unique(col)))}
  if(is.null(cex)){cex=ifelse(length(npcs)==2,1,7)}#set size to 1 for 2D and 7 for 3D plots

  if(length(npcs)==2)
  {
    plot(pc$x[,1],pc$x[,2],col=col,pch=pch,bg=col,cex=cex,
         xlab=xlab,
         ylab=ylab,
         main=main)

    if(add.legend==TRUE)
    {
      if(is.null(legend.col)){legend.col=unique(col)}
      if(is.null(legend.bg)){legend.bg=unique(bg)}
      legend(legend.position,legend=legend.text, lty=rep(NA,length(legend.text)), pch = pch, col = legend.col, pt.bg = legend.bg, lwd=2.5)
    }
  }else
  {
    if(is.null(zlab)){zlab=paste0('PC',npcs[3],': ',var_explained_percentage[3],'%')}
    rgl::plot3d(pc$x[,1],pc$x[,2],pc$x[,3],col=col,pch=pch,bg=col,size=cex,
         xlab=xlab,
         ylab=ylab,
         zlab=zlab,
         main=main)

    if(add.legend==TRUE)
    {
      if(is.null(legend.col)){legend.col=unique(col)}
      if(is.null(legend.bg)){legend.bg=unique(bg)}
      rgl::legend3d(legend.position,legend=legend.text, lty=rep(NA,length(legend.text)), pch = pch, col = legend.col, pt.bg = legend.bg, lwd=2.5)
    }
  }

  pc$var_explained=var_explained
  return(base::invisible(pc))
}
