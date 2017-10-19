
import scala.io._
object Sieve_of_Eratosthenes extends App {
  println("Enter a number: ")
  //a number is input upto which we want to find the prime numbers
  val num = StdIn.readInt()
  val myList = List.range(2, num + 1) // list up to num numbers including num will be generated

  println("elements of the list are: ")
  println(myList)
  val outputList = getPrimeNumbers(myList, num, 0) // this func takes a list,
  // number unitll which the prime numbers are required and
  //initial value for accumulator as parameters

  println("Do you want to perform the Junit test?")
  println("Test can only be true if num is 30")
  println("enter 1 to proceed")
  val choice = StdIn.readInt()
  if (choice == 1) {
    lab5Test(outputList) // here we call the juint func to test if our output is correct
  }

  def getPrimeNumbers(myList: List[Int], n: Int, acc: Int): List[Int] = {

    if (acc < myList.length) { // here we check if accumulator is out of bound of the list index
      val a = myList(acc)
      val newList = removeElements(myList, a, acc) // a new list is returned after removal
      return getPrimeNumbers(newList, n, acc + 1) // newly obtained list is returned tail recursivly

    } else { // if acc is out of bound then we simply print the result
      println("prime numbers are: ")
      println(myList)
      return (myList)
    }
    // removeElements func takes the list, the number whose multiples are to be removed
    // and the acc value as parameter
    //and returns a list after removal of multiples of p

  }
  def removeElements(myList: List[Int], p: Int, acc: Int): List[Int] = {
    if (acc < myList.length) { //if acc is out of bound func returns

      // if value at myList[acc] is multiple of p
      //and acc is not p then we proceed else we return

      if (myList(acc) % p == 0 && myList(acc) != p) {
        val myNewList = myList.filter(_ != myList(acc)) // here we filter the multiples of p
        removeElements(myNewList, p, acc + 1) // tail recursive call to the func itself
      } else {
        removeElements(myList, p, acc + 1)
      }
    } else return myList
  }

  // this func is used for unit testing. it compares the output list with the given list
  //and prints passed if the resuls match
  def lab5Test(outputlist: List[Int]): Unit = {
    val primeNumsBtw2And30 = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    if (primeNumsBtw2And30 == outputlist) {
      print("test Passed")
    } else print("test failed")
  }

}