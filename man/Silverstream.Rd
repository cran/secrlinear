%\encoding{UTF-8} for degree symbol?
\name{Silverstream}
\docType{data}
\alias{Silverstream}

\title{ Silverstream River Centrelines }

\description{

Silverstream (45\eqn{^o}{o}48' S, 170\eqn{^o}{o}26' E) is a small river in a forested
catchment near Dunedin, New Zealand, about 12 km from top to
bottom. There are numerous side streams making this a good example of a
dendritic network.
 
}

\format{

An ESRI polyline shapefile (silverstream.shp, silverstream.shx, silverstream.dbf).

}

\source{

  Land Information New Zealand (LINZ) 1:50 000 topographic database
  \url{http://data.linz.govt.nz/layer/327-nz-mainland-river-centrelines-topo-150k/}
  licensed by LINZ for re-use under the Creative Commons Attribution 3.0
  New Zealand licence. Downloaded 2014-10-26.

}

\references{

ESRI (1998) \emph{ESRI shapefile technical description}.
    \url{http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}.

}

\examples{

\dontrun{

setwd(system.file("extdata", package = "secrlinear"))
silverstreammask <- read.linearmask(file = "silverstream.shp", spacing = 50)
par(mar = c(1,1,1,1))
plot(silverstreammask)

}

}

\keyword{datasets}
