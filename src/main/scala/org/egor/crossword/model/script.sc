val fill: Array[Array[Int]] = Array.fill(3,3)(0)

fill(0) = Array(1,2,3)
fill(1) = Array(4,5,6)

for(i <- 0 until 3){
  for(j<-0 until 1){
    print (fill(i)(j) + " ")
  }
}