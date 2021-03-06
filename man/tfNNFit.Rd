% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tfNNFit.R
\name{tfNNFit}
\alias{tfNNFit}
\title{returns the weights of a neural network model trained on client data}
\usage{
tfNNFit(x, model, cl.labels, compile.args, fit.args, weights.args = NULL)
}
\arguments{
\item{x}{string of a dataframe name}

\item{model}{json string of tensorflow/keras neural network model}

\item{cl.labels}{string with name of class variable}

\item{compile.args}{list with arguments passed to compile function}

\item{fit.args}{list with arguments passed to fit function}
}
\description{
compiles and fits a keras/tensorflow neural network on client
}
