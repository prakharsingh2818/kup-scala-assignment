package edu.knoldus

import scala.annotation.tailrec

class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {

    def sort(list: List[Int]): List[Int] = {
      if (list.isEmpty) {
        Nil
      } else {
        insert(list.head, sort(list.tail))
      }
    }
    def insert(x: Int, list: List[Int]): List[Int] = {
      if (list.isEmpty || x <= list.head) {
        x :: list
      } else {
        list.head :: insert(x, list.tail)
      }
    }
    sort(array.toList).toArray
  }

  def selectionSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def sort(ints: List[Int], accumulator: List[Int] = List()): List[Int] = {
      if (ints.isEmpty) {accumulator}
      else {
        val minimumElements = ints.filter(_ == ints.min)
        val updatedList = ints.filter(_ != ints.min)
        sort(updatedList, accumulator :++ minimumElements)
      }
    }

    sort(array.toList).toArray
  }


  def bubbleSort(array: Array[Int]): Array[Int] = {
    @tailrec
    def elementSort(list: List[Int], counter: Int = 0): List[Int] = {
      if(counter > list.size) {list}
      else {elementSort(sort(list), counter + 1)}
    }
    @tailrec
    def sort(list: List[Int], currentIndex: Int = 0): List[Int] = {
      if(currentIndex <= list.size - 2) {
        if (list(currentIndex) > list(currentIndex + 1)) {
          sort(swapWithNext(list, currentIndex), currentIndex + 1)
        }
        else {sort(list, currentIndex + 1)}
      }
      else {list}
    }
    def swapWithNext(list:List[Int],currentIndex:Int):List[Int] ={
      (list.dropRight(list.length - currentIndex) :+ list(currentIndex + 1) :+ list(currentIndex)) ::: list.drop((currentIndex + 1) + 1)
    }
    elementSort(array.toList).toArray
  }

}
