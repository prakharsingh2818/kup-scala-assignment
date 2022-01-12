package edu.knoldus

import scala.annotation.tailrec

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def search(arr: Array[Int], lower: Int, upper: Int): Boolean = {
      val midIndex = (upper + lower) / 2
      if (upper >= lower) {
        if (arr(midIndex) == elem) {true}
        else if (elem < arr(midIndex)) {search(arr, lower, midIndex - 1)}
        else {search(arr, midIndex + 1, upper)}
      }
      else {false}
    }
    search(array.sortBy(a => a),0 , (array.length - 1))
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    @tailrec
    def search(ints: Array[Int], accumulator: Boolean): Boolean =
      if(ints.length >= 1 && !accumulator) search(ints.tail, ints.headOption.get == elem)
      else accumulator

    search(array, false)
  }

}
