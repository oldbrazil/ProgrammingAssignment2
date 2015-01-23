# creates a special purpose object (extended matrix) from an R matrix
# this "extended matrix" can hold a second matrix in a scoped cache variable called "mym"
makeCacheMatrix<-function(m=matrix()) {
  mym <- NULL
  set <- function (y) {
      m<<-y
      mym <<- NULL
  }
  get <- function () m
  setinverse <- function (invmatrix) mym <<-invmatrix
  getinverse <- function () mym
  list (set=set, get=get,setinverse=setinverse,getinverse=getinverse)
  
}

# cacheSolve returns the inverse of a matrix
# it needs a special purpose object, an "extended matrix" created by function makeCacheMatrix
# cacheSolve will compute the inverse only if the extended matrix does not hold it internally
cacheSolve <- function(matrixEx,...) {
  inv <- matrixEx$getinverse()
  if(!is.null(inv)){
    message ("[getting cached data!]")
    return (inv)
  }
  originalRealMatrix  <- matrixEx$get()
  inv <- solve(originalRealMatrix,...)
  matrixEx$setinverse(inv)
  inv
}

#Extra fct to test if the process works woth above defined function
testCachingOfInv <- function () {
  message ("creating sample matrix ...")
  somedata<-rbind(1:3,0:2,c(0,0,5))
  m<-matrix(data=somedata,nrow=3)
  print(m)
  message ("creating special object ...")
  mEx <- makeCacheMatrix (m)
  message ("retrieving inverse (handling cache) 1 ...")
  print (cacheSolve(mEx))
  message ("retrieving inverse (handling cache) 2 ...")
  print (cacheSolve(mEx))
  print("")
  message ("creating another sample matrix ...")
  somedata<-rbind(1:3,0:2,c(0,0,17))
  m<-matrix(data=somedata,nrow=3)
  print(m)
  message ("creating special object ...")
  mEx <- makeCacheMatrix (m)
  message ("retrieving inverse (handling cache) 1 ...")
  print (cacheSolve(mEx))
  message ("retrieving inverse (handling cache) 2 ...")
  print (cacheSolve(mEx))
}
