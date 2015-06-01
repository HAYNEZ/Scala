object Lists {

  def main(args: Array[String]) {
    //    val list0 = List(5,2,1)
      
    val list1 = List(1,2,5)
    println(CombList(10,list1))
    //    print("hello")
    //    println(list0)
    //    val lol = List(Nil)
   
    //    println(lol::list1)
    //    
    //   
    //    val insertSorted = insertSort(list0)
    //    insertSorted.foreach{println}
    //    
    //    val zeichen = zählZeichen("test hallo")
    //    zeichen.foreach(println)
    //    
    //    val revList = revFast(list0)
    //    revList.foreach{println}
    //    
    //    println(time(revSlow(list0)))
    //    println(time(revFast(list0)))

    //    
    //    println(time(mergeSort(list0)))
    //    println(time(insertSort(list0)))

    //    println{length(list0)}
    //        println("Hello")
    //    val test = uniqlist(list0)
    //    test.foreach(println)
    //     println("Hello")
    //     if( balanced(("()()()()").toList)){
    //     print("balanced!")}
    //    else {
    //      print("Unbalanced!")
    //    }

//    val x = List(List(1,2,3))
//    val y = List(List(4,5,6))
//    print(mergeLists(x,y))
//    
//    print(filter(list1, (x: Int) => x == 1))
  }

  def filter(l: List[Int], bleibtdrin: Int => Boolean): List[Int] = {
    if (l.tail == Nil) Nil
    else if (bleibtdrin(l.head)) l.head :: filter(l.tail, bleibtdrin)
    else filter(l.tail, bleibtdrin)
  }

  def zählZeichen(s: String): List[(Char, Int)] = {

    def hochzählen(c: Char, l: List[(Char, Int)]): List[(Char, Int)] = {
      if (l == Nil) List((c, 1))
      else if (l.head._1 == c) (c, l.head._2 + 1) :: l.tail
      else l.head :: hochzählen(c, l.tail)
    }

    if (s.isEmpty) Nil
    else hochzählen(s.head, zählZeichen(s.tail))

  }

  def insertSort(l: List[Int]): List[Int] =
    if (l == Nil) Nil
    else insert(l.head, insertSort(l.tail))
  def insert(i: Int, l: List[Int]): List[Int] = {
    if (l == Nil) List(i)
    else if (i >= l.head) i :: l
    else l.head :: insert(i, l.tail)
  }

  def merge(a: List[Int], b: List[Int]): List[Int] = {

    if (a == Nil) b
    else if (b == Nil) a
    else if (a.head >= b.head) a.head :: merge(a.tail, b)
    else b.head :: merge(a, b.tail)
  }

  def mergeSort(l: List[Int]): List[Int] = {
    if (l == Nil) Nil
    else if (l.tail == Nil) List(l.head)
    else {
      val splitted = split(l)

      //merge(mergeSort(derp._1),mergeSort(derp._2)
      merge(mergeSort(splitted._1), mergeSort(splitted._2))
    }

  }

  def split(l: List[Int]): (List[Int], List[Int]) = {
    if (l == Nil) (Nil, Nil)
    else if (l.tail == Nil) (List(l.head), Nil)
    else {
      val dude = split(l.tail.tail)
      ((l.head :: dude._1), (l.tail.head :: dude._2))
    }
  }

  //  
  //    val l: List[Int] = List(1, 2, 3, 4, 5, 6)
  //    val i: Int = 3
  //    val durp = zählZeichenS("Hallo")
  //    durp.foreach(println)
  //    println("hallo")
  //
  //    println(time {
  //      revSlow(l)
  //    })
  //
  //    println(time {
  //      revFast(l)
  //    })

  def append(l: List[Int], i: Int): List[Int] = {
    if (l == Nil) List(i)
    else l.head :: append(l.tail, i)
  }

  def revSlow(l: List[Int]): List[Int] = {
    if (l == Nil) Nil
    else append(revSlow(l.tail), l.head)
  }

  def revFast(l: List[Int]): List[Int] = {
    def rev1(alt: List[Int], neu: List[Int]): List[Int] = {
      if (alt == Nil) neu
      else rev1(alt.tail, alt.head :: neu)
    }

    rev1(l, Nil)
  }

  def time(f: => Unit) = {
    val s = System.nanoTime
    f
    System.nanoTime - s
  }

  //  def zählZeichen(s: String): List[(Char, Int)] = {
  //    val b = s.toList.distinct
  //    def zählZeichen1(s: String, b: List[Char]): List[(Char, Int)] = {
  //      if (b == Nil) Nil
  //      else
  //        ((b.head), (s.count(_ == b.head))) :: zählZeichen1(s, b.tail)
  //
  //    }
  //    zählZeichen1(s, b.tail)
  //
  //  }

  def addCount(c: Char, l: List[(Char, Int)]): List[(Char, Int)] = {
    if (l == Nil) List((c, 1))
    else if (l.head._1 == c) (c, l.head._2 + 1) :: l.tail
    else l.head :: addCount(c, l.tail)
  }

  def length(l: List[Int]): Int = {
    def count(l: List[Int], i: Int): Int = {
      if (l.tail == Nil) 1
      else 1 + count(l.tail, i)
    }
    count(l, 0)
  }

  def take(l: List[Int], n: Int): List[Int] = {
    if (l == Nil) Nil
    else if (n == 0) Nil
    else l.head :: take(l.tail, n - 1)
  }

  def drop(l: List[Int], n: Int): List[Int] = {
    if (l == Nil) Nil
    else if (n > 0) drop(l.tail, n - 1)
    else l.head :: drop(l.tail, 0)
  }

  def insert(l: List[Int], i: Int, x: Int): List[Int] = {
    if (l == Nil) Nil
    else if (i == 1) x :: l.head :: insert(l.tail, i - 1, 0)
    else l.head :: insert(l.tail, i - 1, x)
  }

  def remove(l: List[Int], i: Int): List[Int] = {
    if (l == Nil) Nil
    else if (i == 1) remove(l.tail, i - 1)
    else l.head :: remove(l.tail, i - 1)
  }
  //        def revFast(l: List[Int]): List[Int] = {
  //    def rev1(alt: List[Int], neu: List[Int]): List[Int] = {
  def uniqlist(l: List[Int]): List[Int] = {
    def uniqlist1(l: List[Int], i: Int): Boolean = {
      if (l == Nil) false
      else if (i == l.head) true
      else uniqlist1(l.tail, i)
    }
    if (l == Nil) Nil
    else if (uniqlist1(l.tail, l.head)) uniqlist(l.tail)
    else l.head :: uniqlist(l.tail)
  }

  def balanced(s: List[Char]): Boolean = {
    def balanced1(s: List[Char], i: Int): Boolean = {
      if (s.isEmpty) {
        if (i == 0) true
        else false
      } else if (s.head == '(') balanced1(s.tail, i + 1)
      else if (s.head == ')') {
        if (i > 0) balanced1(s.tail, i - 1)
        else false
      } else balanced1(s.tail, i)
    }
    balanced1(s, 0);
  }

  def countChange(money: Int, coins: List[Int]): Int = {

    def loop(money: Int, lcoins: List[Int], count: Int): Int = {
      // if there are no more coins or if we run out of money ... return 0 
      if (lcoins.isEmpty || money < 0) 0
      else {
        if (money == 0) count + 1
        /* if the recursive subtraction leads to 0 money left - a prefect division hence return count +1 */
        else

          /* keep iterating ... sum over money and the rest of the coins and money - the first item and the full set of coins left*/
          loop(money, lcoins.tail, count) + loop(money - lcoins.head, lcoins, count)
      }
    }

    loop(money, coins, 0)

  }

//  def countChangeComb(money: Int, coins: List[Int]): Int = {
//
//    def loop(money: Int, lcoins: List[Int], count: Int, combination: List[Int]): Int = {
//      // if there are no more coins or if we run out of money ... return 0 
//      if (lcoins.isEmpty || money < 0) 0
//      else {
//        if (money == 0) {
//          println(combinations)
//          count + 1
//
//        } /* if the recursive subtraction leads to 0 money left - a prefect division hence return count +1 */ else
//
//          /* keep iterating ... sum over money and the rest of the coins and money - the first item and the full set of coins left*/
//          loop(money, lcoins.tail, count, combinations) + loop(money - lcoins.head, lcoins, count, lcoins.head :: combination)
//      }
//    }
//
//    loop(money, coins, 0, Nil)
//
//  }
//
//  
//  
//  
  
  
  
  
  
  
    def mergeLists(a: List[List[Int]], b: List[List[Int]]): List[List[Int]] = {

    if (a == Nil) b
    else if (b == Nil) a
    else   a.head :: mergeLists(a.tail, b)
  }
  
  
  
  
  def CombList(money: Int, coins: List[Int]): List[List[Int]] = {

    def loop(money: Int, lcoins: List[Int], comb1: List[Int], combinations: List[List[Int]]): List[List[Int]] = {
      // if there are no more coins or if we run out of money ... return 0 
      if (lcoins.isEmpty || money < 0) combinations
      else {
        if (money == 0) {
          comb1 :: combinations

        } /* if the recursive subtraction leads to 0 money left - a prefect division hence return count +1 */
        else

          /* keep iterating ... sum over money and the rest of the coins and money - the first item and the full set of coins left*/
          mergeLists(loop(money, lcoins.tail, comb1, combinations), loop(money - lcoins.head, lcoins, lcoins.head :: comb1, combinations))
      }
    }

      loop(money, coins, Nil, Nil)

  }

  
  
  
  
  
}
