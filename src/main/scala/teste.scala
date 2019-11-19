import org.scalajs.dom
import dom.document
import dom.html
import dom.window
import dom.svg
import dom.raw
import scala.scalajs.js.annotation._
import scala.math._


object teste {

 def main(args: Array[String]): Unit = {     
}

/*Converte cada caractere da mensagem para seu valor correspondente 
  em ASCII*/
def listaInteiros(list: Seq[Char]): List[Int] = {
  list match {
    case head+:tail => (head).toInt +: listaInteiros(tail)
    case Nil => List()
  }
}

//Converte cada elemento da lista para binário
def list2Binary(list: List[Int]): List[String] = {
  list match {
    case head+:tail => (head).toBinaryString +: list2Binary(tail)
    case Nil => List()
  }  
}

/*De acordo com valor da lista de inteiros, faz a conversão necessária para que 
  qualquer caractere tenha uma representação adequada em relação ao byte.*/
def listaBinario(list: List[Int]): List[String] = {
  list match {
    case head+:tail => {
      if(head >= 0 && head <= 127) range1(list2Binary(list).head) +: listaBinario(tail)
      else if(head >= 128 && head <= 2047) range2(list2Binary(list).head) +: listaBinario(tail)
      else range3(list2Binary(list).head) +: listaBinario(tail)
    }
    case Nil => List()
  }
} 

def range1(s: String): String = {
  s match {
    case c:String => if(c.length == 7) "0" + c;
              else range1("0" + c)
    case empty => ""        
  }
}

def range2(s: String): String = {
  //Método auxiliar para complementar a string com 0.
  def caso0(s: String): String = {
    s match {
    case c:String => if(c.length == 11) c
              else if(c.length < 11) caso0("0"+c)
              else caso0(c.tail)
    case empty => ""
    }
  }
  val x: String = caso0(s)
  "110" + x.dropRight(6) + "10" + x.drop(5)
}

def range3(s: String): String = {
  def caso0(s: String): String = {
    s match {
      case c:String => if(c.length == 16) s
                       else if(c.length < 16) caso0("0"+c)
                       else caso0(c.tail)
    }
  }
  val x: String = caso0(s)
  val y: String = x.dropRight(12)
  val z: String = x.drop(4)
  "1110" + y + "10" + z.dropRight(6) + "10" + z.drop(6)
  
}

//Concatena os binários para apenas uma única string
def concatenarElementos(list: List[String]): String = {
  list match {
    case head+:tail => head + concatenarElementos(tail)
    case Nil => ""
  }
}

//Faz com que a quantidade de bytes seja divisível por 3
def bit2Byte(s: String): String = {
  if((s.length/8) % 3 == 0) s
  else if((s.length/8) % 3 == 1) s + "0000"
  else s + "00"
}

//Agrupa de 6 em 6 números de acordo com a string passada
def agrupar6(s: String):List[String] = {
   if(s.isEmpty) List()
   else s.take(6) +: agrupar6(s.drop(6))
}

/*Transforma a lista de string em uma lista de decimais
  para a futura conversão*/
def listBin2Int(list: List[String]):List[Int] = {
  list match {
    case head+:tail => Integer.parseInt(head, 2) +: listBin2Int(tail)
    case Nil => Nil
  }
}

//A base da criptografia.
def mapa(): List[Char] = {
      def mapa1(c: Char): List[Char] = {
        if((c).toInt < 91) c +: mapa1((c+1).toChar)
        else List()
      }
      def mapa2(c: Char): List[Char] = {
        if((c).toInt < 123) c +: mapa2((c+1).toChar)
        else List()
      }
      def mapa3(c: Char): List[Char] = {
        if((c).toInt < 58) c +: mapa3((c+1).toChar)
        else List('+','/')
      }
      mapa1('A') ::: mapa2('a') ::: mapa3('0') 
}

val x: List[Char] = mapa() 

def criptografe(list: Seq[Char]): String = {
  def mensagemCriptografada(list: List[Int]): String = {
      list match {
        case head+:tail => x(head) + mensagemCriptografada(tail)
        case Nil => ""
      }
  }
  val a: List[Int] = listaInteiros(list)
  val b: List[String] = listaBinario(a)
  val c: String = concatenarElementos(b)
  val d: String = bit2Byte(c)
  val e: List[String] = agrupar6(d)
  val f: List[Int] = listBin2Int(e)
  val tam: Double = (d.length / 8)
  if(tam % 3 == 0) mensagemCriptografada(f)
  else if(tam % 3 == 1) mensagemCriptografada(f) + "=="
  else mensagemCriptografada(f) + "="
}

  @JSExportTopLevel("descript")
    def descriptografar() = {
       try {
          val p = document.getElementById("myText2").asInstanceOf[html.TextArea].value;
          document.getElementById("myText").asInstanceOf[html.TextArea].value = descriptografe(p)
          document.getElementById("myText2").asInstanceOf[html.TextArea].value = ""; 
      } catch {
         case e: Exception  =>{
           window.alert("A mensagem não pode ser criptografada por não atender a uma mensagem válida.")
         }
       }
      
  }


    @JSExportTopLevel("cript")
    def criptografar()= {
            val h = document.getElementById("myText").asInstanceOf[html.TextArea].value;
            document.getElementById("myText2").asInstanceOf[html.TextArea].value = criptografe(h)
            document.getElementById("myText").asInstanceOf[html.TextArea].value = "";      
  }

/**********************************************************************************************************************************************************/

/*Utilizado para verificar o em quantos bytes a
  string de binarios foi deslocada.*/
def doisUltimos(s: String): Int = {
  val x: Int = s.length
  val p: String = s.drop(x - 2)
  if((p.head).toString == "=") 2
  else if((p.tail.head).toString == "=") 1
  else 0
}

/*Converte cada caractere para um inteiro de acordo com
  a sua posição no mapa.*/
def ocorrencia(list: Seq[Char], mapa: List[Char]): Int = {
  (list, mapa) match {
    case (Nil,_) => 0 
    case(head+:tail, x) => if(head == x.head) 0
                           else 1 + ocorrencia(head+:tail,x.tail)
  }
}

/*Verifica no mapa a key correspondente e transforma cada caractere
  da mensagem na key.*/
def dlistaInt(list: Seq[Char]): List[Int] = {
  list match {
    case head+:tail => if(head.toString == "=") List() else ocorrencia(list, x) +: dlistaInt(tail)
    case Nil => List()
  }
}


/*Utilizado para deixar cadaa string da lista 
  como um múltiplo de 6*/
def completar6(list: List[String]): List[String] = {
  list match {
    case head+:tail => if(head.length == 6) head +: completar6(tail) else completar6(("0" + head)+:tail)
    case Nil => List()
  }
}

def removerUltimos0(list: String, v: Int): String = {
  if(v == 0) list
  else if(v == 1) list.dropRight(2)
  else list.dropRight(4)
}

/*A cada 6 elementos, adiciona cada string 
  correspondente a uma lista */
def quebraString(s: String): List[String] = {
  if(s.length == 0) List()
  else{
    if(s.head.toString == "0") s.take(8) +: quebraString(s.drop(8))
    else if (s.head.toString == "1" && s.tail.head.toString == "1" && s.tail.tail.head.toString == "0") (s.take(8).drop(3) ++ s.take(16).drop(10)) +: quebraString(s.drop(16))
    else (s.take(8).drop(4) + s.take(16).drop(10) + s.take(24).drop(18)) +: quebraString(s.drop(24))
  }
}

def descriptografe(s: String): String = {
  def mensagem(list: List[Int]): String = {
    list match {
      case head+:tail => (head.toChar).toString ++ mensagem(tail)
      case Nil => ""
    }
  }
  val v: Int = doisUltimos(s)
  val a: List[Int] = dlistaInt(s)
  val b: List[String] = list2Binary(a)
  val c: List[String] = completar6(b)
  val d: String = concatenarElementos(c)
  val e: String = removerUltimos0(d,v)
  val f: List[String] = quebraString(e)
  val g: List[Int] = listBin2Int(f)
  mensagem(g)
 }

}


