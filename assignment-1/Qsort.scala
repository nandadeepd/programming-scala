/*
* @Author: Nandadeep Davuluru, davuluru@pdx.edu
* @Date:   2018-09-27 15:10:21
* @Last Modified by:   nandadeepd
* @Last Modified time: 2018-10-03 17:05:50
*/
class Utilities {

  def swap(arr: Array[Int], i: Int, j: Int): Array[Int] = {
    // make a new array with updated elements and return that.
    var new_arr = arr
    var temp = new_arr(i)
    new_arr(i) = new_arr(j)
    new_arr(j) = temp
    new_arr
  }

  def createArray(n: Int): Array[Int] = {
    scala.util.Random.shuffle(1 to n).toArray
  }

  def partition(arr: Array[Int], low: Int, high: Int): Int = {

    var pivot = arr(high)
    var middle = low
    var swapped_arr = arr
    for (i <- low to high) {
      if (arr(i) < pivot) {
        swapped_arr = swap(arr, i, middle)
        middle += 1
      }
    }
    swapped_arr = swap(swapped_arr, high, middle)
    middle
  }

  def bubbleSort(arr: Array[Int], low: Int, high: Int): Array[Int] = {
    var swapped_arr = arr
    for (i <- low to high) {
      for (j <- i + 1 to high) {
        if (arr(i) > arr(j)) {
          swapped_arr = swap(arr, i, j)
        }
      }
    }
    swapped_arr
  }

  def quickSort(arr: Array[Int], low: Int, high: Int): Array[Int] = {
    if ((high - low) < 10) {
      return bubbleSort(arr, low, high)
    } else {
      var middle = partition(arr, low, high)
      if (low < middle) {
        quickSort(arr, low, middle - 1)
      }
      if (middle < high) {
        quickSort(arr, middle + 1, high)
      }
    }
    arr
  }

  def printArray(arr: Array[Int], n: Int) {

    println(arr.mkString(" "))

  }

  def verifyArray(arr: Array[Int], n: Int) {

    for(i <- 0 to n - 2) {
      if(arr(i) > arr(i + 1)) {
        println("failed")
      }
    }
    println("verified")
  }
}

object Qsort {

  // wrapping the qsort in an object. 

  def main(args: Array[String]) {
    // args.foreach(a => print(a))

    var nums: Int = args(0).toInt
    var ut = new Utilities
    var numbers = ut.createArray(nums)
    println("before sorting: ")
    println(numbers.mkString(" "))
    var sorted_numbers = ut.quickSort(numbers, 0, nums - 1)
    println("after sorting: ")
    println(sorted_numbers.mkString(" "))

    ut.verifyArray(sorted_numbers, nums)
    

  }
}
 

