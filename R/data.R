#' Beer Dataset from Near Infrared Spectroscopy
#'
#'The beer dataset contains 60 samples published by Norgaard et al. Recorded with a 30mm quartz cell
#'on the undiluted degassed beer and measured
#'from 1100 to 2250 nm (576 data points) in steps of 2 nm.
#'A good playing ground for regression methods starting from spectral intensities.
#'
#' @format ## `beer`
#' A data frame with 80 rows and 577 columns:
#' \describe{
#'   \item{y}{Original extract concentration}
#'   \item{xtrain}{Intesities measured on 576 different data points}
#' }
#' @source <https://www.kaggle.com/datasets/robertoschimmenti/beer-nir?resource=download>
#' @references Norgaard, L., Saudland, A., Wagner, J., Nielsen, J. P., Munck, L., & Engelsen, S. B. (2000). Interval partial least-squares regression (iPLS): a comparative chemometric study with an example from near-infrared spectroscopy. Applied Spectroscopy, 54(3), 413–419.
#' Adapted from a R dataset available as part of the OHPL package (https://search.r-project.org/CRAN/refmans/OHPL/html/00Index.html).
"beer"
