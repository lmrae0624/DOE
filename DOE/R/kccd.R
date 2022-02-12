#' kccd
#'
#' central composite design
#'
#' @param basis,generators,block,n0,alpha,wbreps,bbreps,randomize,inscribed,coding,oneblock
#'
#' @return std.order, run.order, level of factor
#'
#' @examples
#' basis=3
#' randomize=FALSE
#' oneblock=TRUE
#'
#' @export
#'
#' @importFrom
#' rsm djoin
#' rsm cube
#' rsm foldover
#' rsm star
#' rsm dupe
#'
kccd<-function (basis, generators, blocks = "Block", n0 = 4, alpha = "orthogonal",
          wbreps = 1, bbreps = 1, randomize = TRUE, inscribed = FALSE,
          coding, oneblock = FALSE)
{
  n0 = rep(n0, 2)[1:2]
  wbreps = rep(wbreps, 2)[1:2]
  bbreps = rep(bbreps, 2)[1:2]
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
      OneToN = design$std.order = design$run.order
      extcols = match(c("std.order","run.order"), names(design))
      oldcols = (1:ncol(design))[-extcols]
    }
    if (randomize) {
      i.init = .block.indices(design)
      for (idx in i.init) {
        design[sample(idx), ] = design[idx, ]
        design$std.order[idx] = 1:length(idx)
      }
      OneToN = row.names(design)
    }
    design
  }
  if (inscribed && is.logical(inscribed))
    inscribed = -999
  if (is.character(blocks)) {
    cp = des = cube(basis, generators, n0 = n0[1], reps = wbreps[1],coding = coding, randomize = randomize, inscribed = inscribed)
    blkname = blocks
  }
  else if (inherits(blocks, "formula")) {
    blkname = if (length(blocks) == 3)
      as.character(blocks[[2]])
    else "Block"
    blocks = blocks[[length(blocks)]]
    cp = des = cube(basis, generators, blockgen = blocks,
                    n0 = n0[1], reps = wbreps[1], coding = coding, randomize = randomize,
                    bid = 1, inscribed = inscribed)
    nfb = nrow(attr(cp, "rsdes")$blk.signs)
    if (nfb > 1)
      for (i in 2:nfb) des = djoin(des, foldover(cp, bid = i, randomize = randomize), blkname = blkname)
    cp = des
  }
  else stop("improper 'blocks' argument; must be a string or a formula")
  if (bbreps[1] > 1)
    for (i in 2:bbreps[1]) {
      des = djoin(des, dupe(cp, randomize = randomize), blkname = blkname)
    }
  sp = star(des, n0 = n0[2], alpha = alpha, reps = wbreps[2], randomize = randomize)
  for (i in 1:bbreps[2]) des = djoin(des, dupe(sp, randomize = randomize), blkname = blkname)
  if (inscribed) {
    div = attr(des, "rsdes")$divisor = attr(sp, "rsdes")$divisor
    pri = attr(des, "rsdes")$primary
    des[, pri] = des[, pri]/div
  }
  if (oneblock) {
    des[[blkname]] = NULL
    if (randomize)
      des = .randomize(des, TRUE)
  }
  des = .randomize(des, randomize = randomize)
  des<-as.data.frame(des)
  names(des)[1] <- c("std.order")
  names(des)[2] <- c("run.order")
  des
}
