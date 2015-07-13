

/**
 * @author Ben
 */
class help {
  def main(args: Array[String]){
    val l: List[Int] = List(1,2,3)
    val i: Int = 3
    
    l.foreach{println}
  }
  
  def append(l: List[Int], i: Int){
      if (l == Nil) l(i)
      else append(l.tail, i)
  }
  
  
  
}