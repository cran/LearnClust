% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chebyshevDistance.R
\name{chebyshevDistance}
\alias{chebyshevDistance}
\title{To calculate the Chebyshev distance.}
\usage{
chebyshevDistance(x, y)
}
\arguments{
\item{x}{is a numeric vector or a matrix. It represents the values of a cluster.}

\item{y}{is a numeric vector or a matrix. It represents the values of a cluster.}
}
\value{
Chebyshev distance value.
}
\description{
To calculate the Chebyshev distance of two clusters.
}
\details{
This function is part of the hierarchical clusterization method. The function calculates the
Chebyshev distance value from \code{x} and \code{y}.
}
\examples{

x <- c(1,2)
y <- c(1,3)

cluster1 <- matrix(x,ncol=2)
cluster2 <- matrix(y,ncol=2)

chebyshevDistance(x,y)

chebyshevDistance(cluster1,cluster2)

}
\author{
Roberto Alcántara \email{roberto.alcantara@edu.uah.es}

Juan José Cuadrado \email{jjcg@uah.es}

Universidad de Alcalá de Henares
}
