import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.collection.{MapView, immutable}

object Exercise2 {
  def main(args: Array[String]): Unit = {
    //println(merge(Array(1, 3, 5, 6, 0, 0, 0), 4, Array(3, 5, 8), 3))
    //println(removeElement(Array(1, 3, 5, 6, 0, 0, 0), 0))
    //println(removeElement(Array(1, 3, 5, 6, 0, 0, 0), 0))
    //println(removeDuplicates(Array(1, 3, 5, 6, 0, 0, 0)))
    //println(removeDuplicates2(Array(1, 3, 5, 5, 6, 6, 6, 0, 0, 0)))
    //println(majorityElement(Array(1, 3, 5, 5, 6, 6, 6, 0, 0, 0)))
    //println(rotate(nums = Array(1,2,3,4,5,6,7), k = 3))
    //println(maxProfit(Array(1, 2)))
    //println(maxProfit(Array(1, 2)))
    print(jump(Array(2,3,1)))
  }

  //88. Merge Sorted Array
  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    (nums1.take(m) ++ nums2.take(n)).sorted.zipWithIndex.foreach(n => nums1.update(n._2, n._1))
  }

  //27. Remove Element
  def removeElement(nums: Array[Int], `val`: Int): Int = {
    val newArray = nums.filter(elem => elem != `val`)
    newArray.copyToArray(nums)
    newArray.length
  }

  //26. Remove Duplicates from Sorted Array
  def removeDuplicates(nums: Array[Int]): Int = {
    val distinctNums = nums.distinct
    distinctNums.copyToArray(nums)
    distinctNums.length
  }

  //80. Remove Duplicates from Sorted Array II
  def removeDuplicates2(nums: Array[Int]): Int = {
    val mapNums: Map[Int, Int] = nums.groupBy(identity).view.mapValues(_.length).toMap
    val updatedMap: Map[Int, Int] = mapNums.map {
      case (key, value) if value >= 3 => (key, 2)
      case (key, value) => (key, value)
    }
    val mapNumsToArray: Array[Int] = updatedMap.map((number, frequency) => Array.fill(frequency)(number)).reduce((a, b) => a.concat(b))

    mapNumsToArray.copyToArray(nums)
    mapNumsToArray.length
  }

  //169. Majority Element
  def majorityElement(nums: Array[Int]): Int = {
    val mapNums: Map[Int, Int] = nums.groupBy(identity).view.mapValues(_.length).toMap
    val highestNumFrequency: Int = mapNums.map { (number, frequency) => frequency }.toSeq.max
    val highestNum: Int = mapNums.find((number, frequency) => frequency == highestNumFrequency).get._1
    highestNum
  }

  //189. Rotate Array
  def rotate(nums: Array[Int], k: Int): Unit = {

    @tailrec
    def auxRotate(auxNums: Array[Int], k: Int): Array[Int] = {
      if (k == 0) auxNums
      else auxRotate(auxNums.last +: auxNums.dropRight(1), k - 1)
    }

    val simplexK: Int = k % nums.length
    auxRotate(nums, simplexK).copyToArray(nums)

  }

  //121. Best Time to Buy and Sell Stock
  def maxProfit(prices: Array[Int]): Int = {
    @tailrec
    def auxMaxProfit(subPrices: Seq[Int], maxProfit: Int = 0, index: Int = 0): Int = {
      if (subPrices.length <= 1) maxProfit
      else if (index >= subPrices.length) auxMaxProfit(subPrices.tail, maxProfit)
      else {
        val refValue: Int = subPrices.head
        val defSubPrices: Seq[Int] = subPrices.tail.map(_ - refValue)
        val maxDiffSubPrices: Int = defSubPrices.max
        if (maxProfit >= maxDiffSubPrices) auxMaxProfit(subPrices, maxProfit, index + 1)
        else auxMaxProfit(subPrices, maxDiffSubPrices, index + 1)
      }
    }


    @tailrec
    def deleteLastValues(pricesToDelete: Seq[Int]): Seq[Int] = {
      if (pricesToDelete.length <= 1) pricesToDelete
      else if (pricesToDelete.last > pricesToDelete.dropRight(1).last)
        pricesToDelete
      else deleteLastValues(pricesToDelete.dropRight(1))
    }

    @tailrec
    def deleteFirstValues(pricesToDelete: Seq[Int]): Seq[Int] = {
      if (pricesToDelete.length <= 1) pricesToDelete
      else if (pricesToDelete.head < pricesToDelete.tail.head)
        pricesToDelete
      else deleteFirstValues(pricesToDelete.tail)
    }

    val cleanPrices: Seq[Int] = deleteLastValues(deleteFirstValues(prices)).foldLeft(Seq.empty[Int]) { (result, num) =>
      if (result.isEmpty || num != result.last) result :+ num
      else result
    }

    auxMaxProfit(cleanPrices)
  }

  //55. Jump Game - https://leetcode.com/problems/jump-game/description/?envType=study-plan-v2&envId=top-interview-150
  def canJump(nums: Array[Int]): Boolean = {

    @tailrec
    def auxCanJump(auxNums: Array[Int], fuel: Int = 0, bolVal: Boolean = true): Boolean = {
      if (fuel < 0) false
      else if (auxNums.length == 1) true
      else if (auxNums.head > fuel) auxCanJump(auxNums.tail, fuel = auxNums.head)
      else auxCanJump(auxNums.tail, fuel - 1)
    }

    auxCanJump(nums)
  }

  def jump(nums: Array[Int]): Int = {
    def auxJump(auxNums: Array[Int], jumps: Int = 0): Int = {

      if (auxNums.length <= 1) jumps
      else if (auxNums.head >= auxNums.tail.length) jumps + 1
      else {
        val firstElem: Int = auxNums.head

        val longestJump: Array[Int] = auxNums.tail.take(firstElem)

        val longestJumpWithIndex: Seq[(Int, Int)] = longestJump.zipWithIndex.toSeq

        val normalizedElementsWithIndex: Seq[(Int, Int)] = longestJumpWithIndex.map { case (elem, ind) => (ind, elem + ind + 1)}

        val maxIndex = normalizedElementsWithIndex.maxBy(_._2)._1

        auxJump(auxNums.slice(maxIndex + 1, auxNums.length), jumps + 1)
      }
    }

    auxJump(nums)
  }

}
