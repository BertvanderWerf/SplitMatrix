rm(list=ls())

is.splitmatrix <- function(x) {
  'splitmatrix' %in% class(x) 
}

as.splitmatrix <- function(x) {
  if (!is.splitmatrix(x)) {
    class(x) <- c(class(x), 'splitmatrix')
  }
  return(x)
}

splitmatrix<- function(mat, rows=10, cols=0, transpose=FALSE) {
  tm <- function(x) {
    t(as.matrix(x))
  }
  
  makeFact <- function(fact, length) {
    if (!is.factor(fact)) {
      if (is.numeric(fact)) {
        stopifnot(fact[1]>=0)
        if (fact==0) fact <- 1
        n <- fact[1]
        ni <- round(length/n)
        fact <- rep(seq(1,n), rep(ni,n))
        nd <- length-length(fact)
        if (nd>0) {
          fact <- c(fact,rep(n, nd))
        }
        fact <- factor(fact)
      }
    }
    return(fact)
  }

  
  if (!is.splitmatrix(mat)) {
    rows <- makeFact(rows, nrow(mat))
    cols <- makeFact(cols, ncol(mat))
    nrows <- nlevels(rows)
    ncols <- nlevels(cols)
    
    tmd <- function(x) as.data.frame(t(as.matrix(x)))
    
    if (is.matrix(mat)) mat <- as.data.frame(mat)
    # yy <- sapply(split(as.data.frame(t(as.matrix(mat))), cols), tm, simplify=FALSE )
    if (transpose==TRUE) {
      out <- matrix(list(), nrow=ncols, ncol=nrows, dimnames=list(levels(cols), levels(rows)))
      res <- sapply(split(mat, rows), tmd, simplify = FALSE)
      for (j in 1:nrows) {
        out[,j] <- sapply(split(res[[j]], cols), as.matrix, simplify=FALSE)
      }
      names(out) <- outer(levels(cols), levels(rows), paste.dot)
    } else {
      out <- matrix(list(), nrow=nrows, ncol=ncols, dimnames=list(levels(rows), levels(cols)))
      res <- sapply(split(mat, rows), tmd, simplify = FALSE)
      j <- 1
      for (j in 1:nrows) {
        out[j,] <- sapply(split(res[[j]], cols), tm, simplify = FALSE)
      }      
      names(out) <- outer(levels(rows), levels(cols), paste.dot)
      
    #   out <- sapply(sapply(split(as.data.frame(t(as.matrix(mat))), cols), tm, simplify=FALSE ), 
    #               function(y) { sapply(split(as.data.frame(y), rows), as.matrix, simplify = FALSE) }, simplify=TRUE)
    }
  } else {
    dim <- base::dim(mat)
    for (j in 1:dim[2]) {
      d_j <- dim(mat[1,j][[1]])[2]
      if (dim[1]>1) {
        for (i in 2:dim[1]) {
          d_ <- dim(mat[i,j][[1]])[2]
          if (d_j!=d_) {
            stop('row dimensions do not match')
          }
        }
      }
    }
    for (i in 1:dim[1]) {
      d_i <- dim(mat[i,1][[1]])[1]
      if (dim[2]>1) {
        for (j in 2:dim[2]) {
          if (d_i!=dim(mat[i,j][[1]])[1]) {
            stop('column dimensions do not match')
          }
        }
      }
    }
    
    mdim <- c(0,0)
    for (i in 1:dim[1]) {
      mdim[1] <- mdim[1]+dim(mat[i,j][[1]])[1]
    }
    for (j in 1:dim[2]) {
      mdim[2] <- mdim[2] +dim(mat[i,j][[1]])[2]
    }

    rows <- makeFact(rows, mdim[1])
    cols <- makeFact(cols, mdim[2])

    for (j in 1:dim[2]) {
      if (1==j) col <- list()
      c_j <- mat[,j]
      col_j <- c_j[[1]]
      if (length(c_j)>1) {
        for (i in 2:length(c_j)) {
          col_j <- rbind(col_j, c_j[[i]])
        }
      }
      col_j <- splitmatrix(col_j, rows=rows, cols=1, transpose=FALSE)
      col[[j]] <- col_j
    }
    out <- matrix(list(), nlevels(rows), nlevels(cols))
    j <- 1
    for (j in 1:nlevels(rows)) {
      for (i in 1:length(col)) {
        if (i==1) {
          row_j <- col[[i]][[j]]
        } else {
          row_j <- cbind(row_j, col[[i]][[j]])
        }
        #col[[i]][[j]] <- list() # clean up memory
      }
      out[j,] <- splitmatrix(row_j, rows=1, cols=cols, transpose=transpose)
    }
  }

  class(out) <- c(class(out), 'splitmatrix')
  return(out)
}
  paste.dot <- function (x, y) { paste(x, y, sep='.')}

mult <- function(x1, y1) {
  if (is.splitmatrix(x1) & is.splitmatrix(y1)) {
    # r <- sapply(y1, function(x) { sapply(x1, function(y) { y %*% x}, simplify = FALSE)},
    #             simplify=TRUE)
    # z <- sapply(r, dim)
    # z[,1]
    # 
    r <- sapply(x1, function(x) { sapply(y1, function(y) { x %*% y}, simplify = FALSE)},
                simplify=TRUE)
    row.names(r) <- row.names(x1)
    colnames(r) <- row.names(y1)
    names(r) <- unlist(outer(row.names(r), colnames(r), paste.dot))
    class(r) <- c(class(r), 'splitmatrix')
  } else if (is.splitmatrix(x1)) {
    r <- sapply(x1, function(x) { x %*% y1}, simplify = FALSE)
    row.names(r) <- row.names(x1)
    class(r) <- c(class(r), 'splitmatrix')
  } else if (is.splitmatrix(x2)) {
    r <- sapply(y1, function(y) { x1 %*% y}, simplify = FALSE)
    colnames(r) <- row.names(y1)
    class(r) <- c(class(r), 'splitmatrix')
  } else {
    r <- x1 %*% x2
  }
  return(r)
}

print.sp <- function(mat) {
  for (i in 1:pdim(mat)[1]) {
    for (j in 1:pdim(mat)[2]) {
      if (j==1) {
        R <- mat[i,j][[1]]
      } else {
        R <- cbind(R, l=NA)
        R <- rbind(R, mat[i,j][[1]])
      }
    }
    row.names(R) <- paste(row.names(mat)[i], row.names(R), sep='.')
    if (i==1) {
      base::print(R)
    } else {
      cat('\n')
      base::print(R)
    }
  }
}

x <- outer(1:60, 1:6, '*')
y <- splitmatrix(mat=x, rows=10, cols=2)
matfunction(y, dim1)
matfunction(y, dim2)
z <- splitmatrix(mat=y, rows=2, cols=3)
z[1,2][[1]]

f <- factor(c(rep(1, 10), rep(2, 20), rep(3, 30)))
levels(f) <- c('A','B','C')
g <- f
levels(g) <- tolower(levels(f))

x1 <- splitmatrix(x, f)
matfunction(x1, dim1)
matfunction(x1, dim2)

y1 <- splitmatrix(x, g, transpose=TRUE)
matfunction(y1, dim1)
matfunction(y1, dim2)

printdim <- function(x1) {
  for (i in 1:dim(x1)[1]) {
    l <- ''
    for (j in 1:dim(x1)[2]) {
      item <- paste0('(',paste(dim(x1[i,j][[1]]), collapse='-'), ')')
      if (j==1) {
        line <- item
      } else {
        line <- paste(line, item)
      }
    }
    print(line)
  }
}
printdim(x1)
printdim(y1)
if (dim[x1])
if ((dim(x1)[2]>1) & (dim(y1)[1]>1)) {
  stopifnot(dim(x1)[2]==dim(y1)[1])
}


# y <- y1[1,1][[1]]
# x <- x1[1,1][[1]]
mult <- function (x, y) {
  if (!is.list(x) & is.matrix(x)) {
    x <- splitmatrix(x, 1, 1)
  }
  if (!is.list(y) & is.matrix(y)) {
    y <- splitmatrix(y, 1, 1)
  }
  out <- sapply(y, function(yj) { sapply(x, function(xi) { xi %*% yj } , simplify=FALSE) }, simplify=TRUE)
  out <- as.splitmatrix(out)
  names(out) <- outer(names(x), names(y), paste.dot)
  return(out)
}
z1 <- mult(x1, y1)
printdim(z1)

r1 <- splitmatrix(r, 2, 2)

mat <- r
diag.splitmatrix <- function(mat) {
  out <- as.splitmatrix(matrix(sapply(mat, base::diag), nrow=dim(mat)[1], ncol=dim(mat)[2],
                           dimnames=list(row.names(mat),  colnames(mat))))
  names(out) <- names(mat)
  unlist(base::diag(out)) 
}
diag.splitmatrix(r)

dim1 <- function(x) dim(x)[1]
dim2 <- function(x) dim(x)[2]

matfunction <- function(mat, fun, simplify=TRUE) {
  out <- as.splitmatrix(matrix(sapply(mat, fun, simplify = FALSE), nrow=dim(mat)[1], ncol=dim(mat)[2],
                           dimnames=list(row.names(mat),  colnames(mat))))
  names(out) <- names(mat)
  
  if (simplify==TRUE) {
    if (length(out[1,1][[1]])==1) {
      out <- matrix(sapply(out, c), 
                    nrow=dim(out)[1], ncol=dim(out)[2], dimnames=list(row.names(out), colnames(out)))
    }
  }
  return(out)
}
sapply(q, c, simplify = T)
as.matrix(q)

m <- matfunction(r, length)
m[1,1]
m <- matfunction(r, base::dim)
m <- matfunction(r, sum)

m <- q
m <- as.data.frame(m)
max(1:10)

rowFunction <- function(m, fun, na.rm=TRUE) {
  names<- row.names(m)
  out <- sapply(as.data.frame(m), function(x) { fun(x, na.rm=na.rm)} )
  out <- t(t(out))
  row.names(out) <- names
  return(out)
}

rowMax <- function(m, na.rm=TRUE) {
  rowFunction(m, max, na.rm=na.rm)
}

colFunction <- function(m, fun, na.rm=TRUE) {
  if (is.matrix(m)) names <- colnames(m)
  else if (is.data.frame(m)) names <- names(m)
  else names <- NULL
  out <- sapply(as.data.frame(t(as.matrix(m))), function(x) { fun(x, na.rm=na.rm)} )
  out <- t(out)
  colnames(out) <- names
  return(out)
}



q <-matfunction(r, dim1)
rowMax(q)
rowFunction(q, fun=max)

col.new <- rowMax(q)
row.new <- colMax(q)
colMax(x)
rowMax(z1)
class(z1)


dim(x)
matfunction(r, dim2)
max(m[[1]])

matfunction(z1, dim1)
matfunction(z1, dim2)

base::diag(s)
diag.splitmatrix <- function(x) {
  unlist(base::diag(s))
}
diag(r)
