% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdDivisive.R
\name{mdDivisive}
\alias{mdDivisive}
\title{Matrix distance by distance and approach type.}
\usage{
mdDivisive(list, distance, approach, components)
}
\arguments{
\item{list}{is a clusters list.}

\item{distance}{is a string. The distance type to be used.}

\item{approach}{is a string. The approach type to be used.}

\item{components}{is a clusters list. It contains every clusters with only one element. It is used to check if complementary condition is 'TRUE'.}
}
\value{
Matrix distance.
}
\description{
To calculate the matrix distance by using \code{distance} and \code{approach} types.
}
\details{
This function is part of the divisive hierarchical clusterization method. The function calculates the
matrix distance by using the distance and approach types given.

The \code{list} parameter will be a list with the clusters as rows and columns.

The function avoids distances equal 0 and undefined clusters.

It also avoids distances between clusters that are not complementary because they can't be chosen to divide all the clusters.
}
\examples{

data <- c(1,2,1,3,1,4,1,5,1,6)

clusters <- toList(data)

components <- toList(data)

mdDivisive(clusters, 'EUC', 'MAX', components)

mdDivisive(clusters, 'MAN', 'MIN', components)

}
\author{
Roberto Alcántara \email{roberto.alcantara@edu.uah.es}

Juan José Cuadrado \email{jjcg@uah.es}

Universidad de Alcalá de Henares
}
