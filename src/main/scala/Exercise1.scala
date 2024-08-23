import scala.annotation.tailrec

//https://www.simplilearn.com/coding-interview-questions-article
object Exercise1 {
  def main(args: Array[String]): Unit = {
    //println(reverseString("Scala"))
    //println(checkPalindrome("ScalalacS"))
    //println(numberOfOccurrences("ScalalacS", 'a'))
    //println(checkTwoAnagrams("abcd", "DCBAF~~~~"))
    //println(calculateVowelsAndConsonants("abcd"))
    //println(calculateMatchingElements(Seq(1, 2, 3, 4, 5, 1, 2, 3)))
    //println(bubbleSortAlgorithm(Seq(1, 2, 3, 4, 5, 3)))
    //println("insertionSortAlgorithm: " + insertionSortAlgorithm(Seq(2, 1, 3, 4, 10, 1)))
    //println("reverseAlgorithm: " + reverseAlgorithm(Seq(2, 1, 3, 4, 10, 1)))
    //println("swapAlgorithm: " + swapAlgorithm(2, 4))
    //println("fibonacciAlgorithm: " + fibonacciAlgorithm(numberOfSteps = 8))
    //println("factorialAlgorithm: " + factorialAlgorithm(8))
    //println("binarySearchAlgorithm: " + binarySearchAlgorithm(Seq(1, 2, 3, 4, 9, 10), 8))
    //println("secondLargestAlgorithm: " + secondLargestAlgorithm(Seq(1, 2, 3, 4, 9, 10)))
    //println("removeCharacterAlgorithm: " + removeCharacterAlgorithm("abcdefqh", 'b'))
    //println("checkPrimeNumber: " + checkPrimeNumber(1))
    //println("canConstruct: " + canConstruct("abcdeff", "abzxyy"))
    //println("isIsomorphic: " + isIsomorphic("abcdeff", "abzxyy"))
    //println("isIsomorphic: " + groupAnagrams(Array("eat","tea","tan","ate","nat","bat")))
    println("containsNearbyDuplicate: " + containsNearbyDuplicate(nums = Array(1, 2, 3, 1), k = 3))

  }

  //How do you reverse a string in Java?
  def reverseString(name: String): String = {
    name.reverse
  }

  //How do you determine if a string is a palindrome?
  def checkPalindrome(name: String): Boolean = {
    name == name.reverse
  }

  //Find the number of occurrences of a character in a String?
  def numberOfOccurrences(name: String, character: Char): Int = {
    name.toLowerCase.count(char => char == character.toLower)
  }

  //How to find out if the given two strings are anagrams or not?
  def checkTwoAnagrams(firstName: String, secondName: String): Boolean = {
    val firstNameNormalized: String = firstName.toLowerCase.replaceAll("[^a-z]", "")
    val secondNameNormalized: String = secondName.toLowerCase.replaceAll("[^a-z]", "")

    firstNameNormalized.sorted == secondNameNormalized.sorted
  }

  //How do you calculate the number of vowels and consonants in a String?
  def calculateVowelsAndConsonants(name: String): (Int, Int) = {
    val nameNormalized: String = name.toLowerCase.replaceAll("[^a-z]", "")
    val numberOfVowels: Int = nameNormalized.count(character => "aeiou".contains(character))
    val numberOfConsonants: Int = nameNormalized.length - numberOfVowels

    (numberOfVowels, numberOfConsonants)
  }


  //How do you get the matching elements in an integer array?
  def calculateMatchingElements(elemArray: Seq[Int]): Seq[Int] = {
    elemArray
      .groupBy(identity) // Group by element
      .collect { case (elem, occurrences) if occurrences.size > 1 => elem } // Collect elements that appear more than once
      .toSeq // Convert to sequence

  }

  private def checkSortSeq(seqSorted: Seq[Int]): Boolean = {
    seqSorted.sliding(2, 1).forall(x => x.head <= x.tail.head)
  }

  // How would you implement the bubble sort algorithm?
  @tailrec
  def bubbleSortAlgorithm(seqToSort: Seq[Int]): Seq[Int] = {
    @tailrec
    def slidingBubbleSort(subSeqToSortLeft: Seq[Int] = Seq.empty, subSeqToSortRight: Seq[Int]): Seq[Int] = {
      if (subSeqToSortRight.length == 1) subSeqToSortLeft ++ subSeqToSortRight
      else {
        val compareTuple: Seq[Int] = subSeqToSortRight.take(2).sorted
        slidingBubbleSort(subSeqToSortLeft :+ compareTuple.head, compareTuple.tail.head +: subSeqToSortRight.drop(2))
      }
    }

    if (checkSortSeq(seqToSort)) seqToSort
    else
      bubbleSortAlgorithm(slidingBubbleSort(subSeqToSortRight = seqToSort))
  }


  //How would you implement the insertion sort algorithm ? Seq(2, 1, 2, 3, 4, 5, 1, 2, 3))
  @tailrec
  def insertionSortAlgorithm(seqToSort: Seq[Int]): Seq[Int] = {
    @tailrec
    def slidingInsertionSort(sortedPart: Seq[Int], unsortedPart: Seq[Int]): Seq[Int] = {
      if (unsortedPart.isEmpty) sortedPart
      else {
        val element: Int = unsortedPart.head
        val (left, right) = sortedPart.span(_ < element)

        slidingInsertionSort(left ++ (element +: right), unsortedPart.tail)
      }
    }

    if (checkSortSeq(seqToSort)) seqToSort
    else
      insertionSortAlgorithm(slidingInsertionSort(Seq.empty, seqToSort))
  }

  // How do you reverse an array?
  def reverseAlgorithm(seqToSort: Seq[Int]): Seq[Int] = {
    @tailrec
    def reverseAux(sortedPart: Seq[Int], unsortedPart: Seq[Int]): Seq[Int] = {
      if (unsortedPart.isEmpty) sortedPart
      else reverseAux(sortedPart :+ unsortedPart.last, unsortedPart.dropRight(1))
    }

    reverseAux(Seq.empty, seqToSort)
  }

  //How would you swap two numbers without using a third variable?
  def swapAlgorithm(a: Int, b: Int): (Int, Int) = {
    var aux_a: Int = a
    var aux_b: Int = b

    aux_b = aux_a + aux_b
    aux_a = aux_b - aux_a
    aux_b = aux_b - aux_a

    (aux_a, aux_b)
  }

  //Print a Fibonacci series using recursion
  @tailrec
  def fibonacciAlgorithm(totalArray: Seq[Int] = Seq(0, 1), numberOfSteps: Int): Seq[Int] = {
    if (totalArray.length == numberOfSteps) totalArray
    else {
      val lastTwoSum: Int = totalArray.takeRight(2).sum
      fibonacciAlgorithm(totalArray :+ lastTwoSum, numberOfSteps)
    }
  }

  //How do you find the factorial of an integer?
  def factorialAlgorithm(value: Int): Int = {
    val seqValues: Seq[Int] = for i <- value to 2 by -1 yield i
    seqValues.product
  }

  //How would you implement Binary Search?
  @tailrec
  def binarySearchAlgorithm(entireSeq: Seq[Int], findNumber: Int): Boolean = {

    if (entireSeq.isEmpty || (entireSeq.length == 1 && entireSeq.head != findNumber)) false
    else {
      val halfLengthSeq: Int = (entireSeq.length) / 2
      val midValue: Int = entireSeq(halfLengthSeq)

      if (midValue == findNumber) true
      else if (midValue > findNumber) binarySearchAlgorithm(entireSeq.take(halfLengthSeq), findNumber)
      else binarySearchAlgorithm(entireSeq.drop(halfLengthSeq), findNumber)
    }
  }

  //35. How would you find the second largest number in an array?
  def secondLargestAlgorithm(entireSeq: Seq[Int]): Int = {
    @tailrec
    def auxiliarSecondLargest(entireSeq: Seq[Int], highestNumber: Int, secondHighestNumber: Int): Int = {
      if (entireSeq.isEmpty) secondHighestNumber
      else if (entireSeq.head > highestNumber) auxiliarSecondLargest(entireSeq.tail, entireSeq.head, highestNumber)
      else if (entireSeq.head > secondHighestNumber) auxiliarSecondLargest(entireSeq.tail, highestNumber, entireSeq.head)
      else auxiliarSecondLargest(entireSeq.tail, highestNumber, secondHighestNumber)
    }

    val firstNumber: Int = entireSeq.head
    val secondNumber: Int = entireSeq.tail.head

    if (firstNumber > secondNumber) auxiliarSecondLargest(entireSeq.drop(2), firstNumber, secondNumber)
    else auxiliarSecondLargest(entireSeq.drop(2), secondNumber, firstNumber)

  }

  //36. How do you remove all occurrences of a given character from the input string?
  @tailrec
  def removeCharacterAlgorithm(stringtoRemove: String, characterToRemove: Char, newString: String = ""): String = {
    if (stringtoRemove.isEmpty) newString
    else if (stringtoRemove.head == characterToRemove) removeCharacterAlgorithm(stringtoRemove.tail, characterToRemove, newString)
    else removeCharacterAlgorithm(stringtoRemove.tail, characterToRemove, newString :+ stringtoRemove.head)
  }

  //39. How do you check if the given number is prime?
  def checkPrimeNumber(number: Int): Boolean = {
    //if we receive a non prime number, we know that is dividible by a number equal to sqrt(number) or less
    val sqrtNumber: Int = math.sqrt(number).toInt


    @tailrec
    def auxCheckPrimeNumber(number: Int, numberToDivide: Int): Boolean = {
      if (number < 2) false
      else if (numberToDivide == 1) true
      else if (number % 2 == 0) false
      else if (number % numberToDivide == 0) false
      else auxCheckPrimeNumber(number, numberToDivide - 2)
    }

    if (sqrtNumber % 2 == 0)
      auxCheckPrimeNumber(number, sqrtNumber - 1)
    else
      auxCheckPrimeNumber(number, sqrtNumber)

  }

  //383. Ransom Note: Given two strings ransomNote and magazine, return true if ransomNote can be constructed by using the letters from magazine and false otherwise.
  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    val ransomNoteDict: Map[Char, Int] = ransomNote.groupBy(identity).view.mapValues(_.length).toMap
    val magazineDict: Map[Char, Int] = magazine.groupBy(identity).view.mapValues(_.length).toMap


    ransomNoteDict.forall { case (char, charCount) => magazineDict.getOrElse(char, 0) >= charCount }

  }

  //205. Isomorphic Strings: Given two strings s and t, determine if they are isomorphic.
  def isIsomorphic(s: String, t: String): Boolean = {
    val mapped = s.zip(t).toMap
    s.flatMap(mapped.get).mkString == t
  }
}

//290. Word Pattern: Given a pattern and a string s, find if s follows the same pattern.
def wordPattern(pattern: String, s: String): Boolean = {
  val sSplit: Array[String] = s.split(" ")
  val patternArray: Array[Char] = pattern.toCharArray

  if (sSplit.length == patternArray.length) {

    val patternMap: Map[Char, String] = sSplit.zip(patternArray).toMap.map(_.swap)

    patternArray.map(patternMap.getOrElse(_, "")).mkString(" ") == s

  } else false

}

//242. Valid Anagram: Given two strings s and t, return true if t is an anagram of s, and false otherwise.
def isAnagram(s: String, t: String): Boolean = {
  s.sorted == t.sorted
}

//49. Group Anagrams: Given an array of strings strs, group the anagrams together. You can return the answer in any order.
def groupAnagrams(strs: Array[String]): List[List[String]] = {
  strs.groupBy(str => str.sorted).values.map(_.toList).toList
}


//1. Two Sum: Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
def twoSum(nums: Array[Int], target: Int): Array[Int] = {

  val map: Map[Int, Int] = nums.zipWithIndex.toMap

  nums.indices.collectFirst {
    case i if map.get(target - nums(i)).exists(_ != i) => Array(i, map(target - nums(i)))
  }.getOrElse(Array(-1, -1))
}

//202. Happy Number: Write an algorithm to determine if a number n is happy.
def isHappy(n: Int): Boolean = {

  def auxIsHappy(nAux: Int, listOfSums: List[Int]): Boolean = {

    if (nAux == 1) true
    else {
      val digits: Seq[Int] = nAux.toString.map(_.asDigit)
      val squareDigits: Int = digits.map(x => x * x).sum

      if (listOfSums.contains(squareDigits)) false
      else {
        auxIsHappy(squareDigits, listOfSums :+ squareDigits)
      }
    }
  }

  auxIsHappy(n, List())

}

import Math.abs

//202. Contains Duplicate II: Given an integer array nums and an integer k, return true if there are two distinct
// indices i and j in the array such that nums[i] == nums[j] and abs(i - j) <= k.
def containsNearbyDuplicate(nums: Array[Int], k: Int): Boolean = {
  val positionsArray: Array[Array[Int]] = nums.zipWithIndex.groupBy(_._1).view.mapValues(_.map(_._2)).values.toArray.filter(_.length >= 2)
  positionsArray.exists(_.sliding(2).exists { case Array(a, b) => Math.abs(b - a) <= k })

}
//128: Longest Consecutive Sequence: Given an unsorted array of integers nums, return the length of the longest consecutive elements sequence.
def longestConsecutive(nums: Array[Int]): Int = {


  def auxLongestConsecutive(numsAux: Array[Int], currentLength: Int = 1, sequencesLength: Array[Int] = Array(0)): Int = {
    if (numsAux.length == 0) 0
    else if (numsAux.length <= 1) (sequencesLength :+ currentLength).max
    else if (numsAux(1) - 1 == numsAux(0)) auxLongestConsecutive(numsAux.tail, currentLength + 1, sequencesLength)
    else auxLongestConsecutive(numsAux.tail, 1, sequencesLength :+ currentLength)
  }

  val numsSorted: Array[Int] = nums.distinct.sorted
  auxLongestConsecutive(numsSorted)

}





