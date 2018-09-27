class Utilities {

  def swap(a: Array[Int], i: Int, j: Int) {
    if (i == j) {
      return
    } else {
      var temp: Int = a(i)
      a(i) = a(j)
      a(j) = temp
      return a
    }
  }

  def createArray(n: Int): Array[Int] = {
    return scala.util.Random.shuffle(1 to n).toArray
  }

  def partition(arr: Array[Int], low: Int, high: Int): Int = {

    var pivot = arr(high)
    var middle = low

    for (i <- low to high) {
      if (arr(i) < pivot) {
        var temp = arr(i)
        arr(i) = arr(middle)
        arr(middle) = temp
        middle += 1
      }
    }
    var temp = arr(high)
    arr(high) = arr(middle)
    arr(middle) = temp
    return middle
  }
  
  def bubbleSort(arr: Array[Int], low: Int, high: Int):Array[Int] = {
    // var arr_copy = arr
    for(i <- low to high){
      for(j <- i + 1 to high){
        if(arr(i) > arr(j)){
          var temp = arr(i)
          arr(i) = arr(j)
          arr(j) = temp
        }
      }
    }
    return arr
  }
  
  def quickSort(arr: Array[Int], low: Int, high: Int) : Array[Int] = {
    if((high - low) < 10){
      return bubbleSort(arr, low, high)
    } else {
      var middle = partition(arr, low, high)
      if(low < middle){
        quickSort(arr, low, middle - 1)
      }
      if(middle < high){
        quickSort(arr, middle + 1, high)
      }
    }
    return arr
  }

}


var ut = new Utilities
var arr = ut.createArray(10)
println("before")
arr.foreach(a => println(a))
println("----------------")
println("after")
// var part = ut.partition(arr, 0, 9)
// println(part)
var sorted_arr = ut.quickSort(arr, 0, 9)
sorted_arr.foreach(a => println(a))
