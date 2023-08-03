
#Coursera R programming

##Assignment week 2 lexical scoping

## make matrix inverse

mmakeCacheMatrix <- function(x=matrix()){
  Inver=NULL
  set <- function(y){
    x<<-y
    Inver<<-NULL
  }
  get <- function(){x}
  setInverse <- function(inverse){Inver <<- inverse}
  getInverse <- function(){Inver}
  list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
}

#cacheSolve function check whether the inverse matrix is cached firstly. 
# Secondly, the cacheSolve function calculate the inverse of the matrix
cacheSolve <- function(x, ...){
  Inver <- x$getInverse()
  if(!is.null(Inver)){
    message("getting a cache data")
    return(Inver)
  }
  mat <- x$get()
  Inver <- solve(mat, ...)
  x$setInverse(Inver)
  Inver
}

Matx <- makeCacheMatrix(matrix(1:4,nrow =2,ncol = 2))
Matx$get()
Matx$getInverse()
cacheSolve(Matx)
