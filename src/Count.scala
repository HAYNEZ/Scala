

/**
 * @author Ben
 */
class Count {
  
  def main(args: Array[String]){
    val durp = addCount('h',Nil)

      }
  
  
  def revFast(l: List[Int]): List[Int] ={
    def rev1 (alt: List[Int], neu: List[Int]):List[Int]= {
      if (alt == Nil) neu 
      else rev1(alt.tail, alt.head::neu)
      }
  
    rev1(l,Nil)
  }

  
  
  
  def zählZeichen (s: String): List[(Char, Int)]= {
   val b = s.toList.distinct
   def zählZeichen1 (s: String,b: List[Char]): List[(Char, Int)]= {
   if (b == Nil)  Nil
   else
    ((b.head),(s.count(_== b.head)))::zählZeichen1(s,b.tail)
   }
    zählZeichen1(s,b.tail)
    
  }
  
  def addCount (c: Char,l: List[(Char, Int)]): List[(Char, Int)]= {
  if (l == Nil) List((c,1))
  else if  (l.head._1 == c) (c,l.head._2 + 1)::l.tail
  else l.head::addCount(c,l.tail) 
}  
}