#' kbbd
#'
#' design of box-behnken
#'
#' @param k,n0,block,randomize,coding
#'
#' @return std.order, run.order, level of factor
#'
#' @examples
#' k<-3
#'
#' @export
#'
#' @importFrom
#' rsm as.coded.data
#'
kbbd<-function (k, n0 = 4, block = (k == 4 | k == 5), randomize = TRUE,coding){
  reftbl = list(NULL, NULL, list(c(1, 2), c(1, 3), c(2, 3)),
                list(c(1, 2), c(3, 4), c(1, 4), c(2, 3), c(1, 3), c(2,
                                                                    4)), list(c(1, 2), c(1, 3), c(3, 4), c(4, 5), c(2,
                                                                                                                    5), c(1, 4), c(1, 5), c(2, 3), c(2, 4), c(3, 5)),
                list(c(1, 2, 4), c(2, 3, 5), c(3, 4, 6), c(1, 4, 5),
                     c(2, 5, 6), c(1, 3, 6)), list(c(4, 5, 6), c(1, 6,
                                                                 7), c(2, 5, 7), c(1, 2, 4), c(3, 4, 7), c(1, 3, 5),
                                                   c(2, 3, 6)))
  CALL = match.call()
  yvars = NULL
  if (inherits(k, "formula")) {
    names = all.vars(k[[length(k)]])
    if (length(k) > 2)
      yvars = all.vars(k[[2]])
    k = length(names)
  }
  else names = paste("x", 1:k, sep = "")
  if ((k < 3) | (k > 7))
    stop("Box-Behnken designs are available only for k=3:7")
  clist = reftbl[[k]]
  if (length(clist[[1]]) == 2)
    tbl = expand.grid(c(-1, 1), c(-1, 1))
  else tbl = expand.grid(c(-1, 1), c(-1, 1), c(-1, 1))
  n = nrow(tbl)
  des = as.data.frame(matrix(0, nrow = n * length(clist), ncol = k))
  idx = 1:n - n
  for (i in 1:length(clist)) des[idx + i * n, clist[[i]]] = tbl
  if (is.character(block)) {
    blkname = block
    block = TRUE
  }
  else blkname = "Block"
  blk = 0
  if (block) {
    if (k == 4)
      blk = c(rep(1:3, rep(2 * n, 3)), rep(1:3, n0))
    else if (k == 5)
      blk = c(rep(1:2, rep(5 * n, 2)), rep(1:2, n0))
    else stop("Can only block when k=4 or k=5")
    nblk = ifelse(k == 4, 3, 2)
  }
  else nblk = 1
  des = rbind(des, matrix(0, nrow = n0 * nblk, ncol = k))
  names(des) = names
  if (block) {
    des = cbind(factor(blk), des)
    names(des)[1] = blkname
    des = des[order(blk), ]
  }
  row.names(des) = 1:nrow(des)
  if (!is.null(yvars))
    for (v in yvars) des[[v]] = NA
  if (missing(coding))
    coding = sapply(names, function(v) as.formula(paste(v,"~", v, ".as.is", sep = "")))
  des = as.coded.data(des, formulas = coding)
  N = nrow(des)/nblk
  rsd = list(primary = names, call = CALL)
  if (block) {
    rsd$block = blkname
  }
  attr(des, "rsdes") = rsd

  arrange.vars <- function(data, vars){
    ##stop if not a data.frame (but should work for matrices as well)
    stopifnot(is.data.frame(data))

    ##sort out inputs
    data.nms <- names(data)
    var.nr <- length(data.nms)
    var.nms <- names(vars)
    var.pos <- vars
    ##sanity checks
    stopifnot( !any(duplicated(var.nms)),
               !any(duplicated(var.pos)) )
    stopifnot( is.character(var.nms),
               is.numeric(var.pos) )
    stopifnot( all(var.nms %in% data.nms) )
    stopifnot( all(var.pos > 0),
               all(var.pos <= var.nr) )

    ##prepare output
    out.vec <- character(var.nr)
    out.vec[var.pos] <- var.nms
    out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
    stopifnot( length(out.vec)==var.nr )

    ##re-arrange vars by position
    data <- data[ , out.vec]
    return(data)
  }

  .block.indices = function(design) {
    rsd = attr(design, "rsdes")
    if (!is.null(blknm <- rsd$block)) {
      blk = design[[blknm[1]]]
      if (length(blknm) > 1) for (nm in blknm[-1])
        blk = paste(blk, design[[nm]], sep=".")
      # Even now, it's possible to have a null blk, if that var not in data yet
      if (!is.null(blk))
        i.init = split(1:nrow(design), blk)
      else
        i.init = list(1:nrow(design))
    }
    else
      i.init = list(1:nrow(design))
    i.init
  }

  .randomize <- function(design, randomize=TRUE) {
    OneToN = 1:nrow(design)
    if (is.na(match("std.order", names(design)))) {
      design$run.order = design$std.order = OneToN
      extcols = match(c("run.order","std.order"), names(design))
      oldcols = (1:ncol(design))[-extcols]


    }
    if (randomize) {
      i.init = .block.indices(design)
      for (idx in i.init) {
        design[sample(idx), ] = design[idx, ]
        design$std.order[idx] = 1:length(idx)
      }
      row.names(design) = OneToN
    }
    design
  }
  des<-as.data.frame(des)
  des = .randomize(des, randomize = randomize)
  std.order<-as.data.frame(des$std.order)
  colnames(std.order)<-c("std.order")
  run.order<-as.data.frame(des$run.order)
  colnames(run.order)<-c("run.order")
  des<-data.frame(std.order,run.order,des[1:k])
  des
}
