% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/usefulClusters.R
\name{usefulClusters}
\alias{usefulClusters}
\title{To delete clusters grouped.}
\usage{
usefulClusters(list)
}
\arguments{
\item{list}{is a list of clusters.}
}
\value{
A list of clusters.
}
\description{
To delete the clusters already used to create a new one.
}
\details{
This function is part of the hierarchical clusterization method. The function updates the cluster
list with the clusters used after to calculate de matrix distance.
}
\examples{

data <- c(1:10)

list <- toList(data)

usefulClusters(list)

}
\author{
Roberto Alcántara \email{roberto.alcantara@edu.uah.es}

Juan José Cuadrado \email{jjcg@uah.es}

Universidad de Alcalá de Henares
}
