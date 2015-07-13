import java.net._
import java.io.PrintWriter
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.File
import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

object Webserver {
  val defaultport:Int = 8099
  var readArray = new Array[String](2);
  def main (args: Array[String]){
    val port = try {
      args(0).toInt
    }
    catch {
      case _:Throwable => defaultport
    }
    server(port)
  }
  
  def server(port:Int){
      server (new ServerSocket(port))
    } 
    
  
  
    def server(ss: ServerSocket){
      while(true){
        
        
        
      
         
 
         
         
      val sock = ss.accept()    
     
      
      val in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
      var readLine = in.readLine();
      if (readLine != null)
      readArray = readLine.split(" ");
      var request = readArray(1)  
      new HTTPConnection(ss.accept,request)
      } 
 
      
      
      
      
      
      
    }
    
}
  
  class HTTPConnection(s:Socket,r:String) extends Thread {
    start
    override def run{
      respond(s,r)
    }
    
    def respond(s:Socket,r:String){
      try{
        new PrintWriter(s.getOutputStream(),true).println(response(r))  
      } finally {
        s.close
      }
    }
	

	def response(response:String, code:String="200 OK") = {
<html>
<head>
<style>
</style>
</head>
<body>
<img src="file://D:/w3schools_green.jpg" alt="W3Schools.com">
</img>

</body>
</html>
  }
  }