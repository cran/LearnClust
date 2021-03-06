% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/euclideanDistance.details.R
\name{edistance.details}
\alias{edistance.details}
\title{To show the Euclidean distance formula.}
\usage{
edistance.details(x, y)
}
\arguments{
\item{x}{is a numeric vectoror a matrix. It represents the values of a cluster.}

\item{y}{is a numeric vectoror a matrix. It represents the values of a cluster.}
}
\value{
Euclidean distance value and formula.
}
\description{
To show the Euclidean distance formula and to calculate the Euclidean distance of two clusters.
}
\details{
This function is part of the hierarchical clusterization method. The function calculates the
Euclidean distance value from \code{x} and \code{y}.
}
\examples{

x <- c(1,2)
y <- c(1,3)

cluster1 <- matrix(x,ncol=2)
cluster2 <- matrix(y,ncol=2)

edistance(x,y)

edistance(cluster1,cluster2)

}
\author{
Roberto Alcántara \email{roberto.alcantara@edu.uah.es}

Juan José Cuadrado \email{jjcg@uah.es}

Universidad de Alcalá de Henares
}
