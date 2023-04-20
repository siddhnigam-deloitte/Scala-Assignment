import com.typesafe.config.ConfigFactory

import java.io.{File, FileNotFoundException}
import javax.swing.filechooser.FileNameExtensionFilter
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Assignment {

  try {

    val applicationConf = ConfigFactory.load("application.conf")
    val filepath = applicationConf.getString("app.filepath")
    def open(path:String):File=new File(filepath)
    def read(file: File)=Source.fromFile(file).getLines().take(10000)

    val readfile=read(open(filepath))


    def checkpoint1(year1:Int ,year2:Int, director:String):Unit={
      for(k<-readfile.drop(1)) {
          var arr: Array[String] = k.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)(?<![ ])(?![ ])")
          var year = arr(3).toInt

          if (year >= year1 && year <= year2 && director == arr(9)) {
            println(arr(1))
          }
        }
      }

   //checkpoint1(1900,1950,"D.W. Griffith")
    class movieandreview(val movie:String,val review:Int)
    {
      def getmovie():Unit={
        println(this.movie+","+this.review)
      }
    }
    def checkpoint2(userReview:Int):Unit= {
      var ind = 0
      val res = ListBuffer[movieandreview]()
      for (k <- readfile.drop(1)) {
        var arr: Array[String] = k.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)(?<![ ])(?![ ])")

        if (arr.length > 20 && arr(20) != "") {

          var reviewno = arr(20).toInt
          if (reviewno >= userReview && arr(8) == "English") {
            res += new movieandreview(arr(1), reviewno)
          }
        }
      }


      val finalres = res.sortBy(x => -x.review)
      for (x <- finalres) {
        x.getmovie()
      }
    }

    //checkpoint2(500)

    class movieandbudget(val movie:String,val budget:String){
      def getintbudget():Int= {
//        if (this.budget == ""||this.budget==null) {return 0}

          val s: String = ""
          for (i <- this.budget) {
            if (i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9' || i == '0') {
              s.appended(i)
            }
          }
          if(s=="") 0 else s.toInt
        }
    }

    def checkpoint3(year:Int,country:String):Unit={
      val list=ListBuffer[Array[String]]()
      val objectlist=ListBuffer[movieandbudget]()
      for(k<-readfile.drop(1)) {
          var arr: Array[String] = k.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)(?<![ ])(?![ ])")
          list += arr
          }

        val finallist=list.filter(_(3).toInt==year).filter(_(7)==country)

        for(i<-finallist) { objectlist += new movieandbudget(i(1),i(16)) }
        val res=objectlist.sortBy(x => -x.getintbudget())
        for(x<-res.take(5)) {println(x.movie+","+x.budget)}
      }

     //checkpoint3(1912,"USA")


    def checkpoint4(country:String, votes:Int):Unit={
      val list=ListBuffer[Array[String]]()
      for(k<-readfile.drop(1)) {
        var arr: Array[String] = k.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)(?<![ ])(?![ ])")
        list += arr
      }
      val finalist=list.filter(_(7)==country).filter(_(15).toInt>=votes)
      val res=finalist.sortBy(x => -x(6).toInt)

      for(j<-res)
        {
          println(j(1)+","+j(6))
        }

    }
    //checkpoint4("USA",100)


    def getintbudget(budget:String):Int= {
      //        if (this.budget == ""||this.budget==null) {return 0}

      val s: String = ""
      for (i <- budget) {
        if (i == '1' || i == '2' || i == '3' || i == '4' || i == '5' || i == '6' || i == '7' || i == '8' || i == '9' || i == '0') {
          s.appended(i)
        }
      }
      if(s=="") 0 else s.toInt
    }

    def checkpoint5(budget1:String,budget2:String,country:String): Unit =
    {
      var x=""
      val lang_count=collection.mutable.HashMap[String,Int]()
      val list=ListBuffer[Array[String]]()
      val language=collection.mutable.Set[String]()
      for(k<-readfile.drop(1)) {
        var arr: Array[String] = k.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)(?<![ ])(?![ ])")
        if(arr.length>16) list += arr
        if(arr(7).contains(country)) {
          if(arr(8).contains(","))
            {
              var lang:Array[String]=arr(8).split(",")
              for (l<-lang) {
                if(l.charAt(0)=='"')  {x=l.substring(1).trim}
                else if(l.charAt(l.length-1)=='"'){x=l.substring(0,l.length-1).trim}
                else {x=l.trim}
                language+=x
              }
            }
            else { language += arr(8)}
        }
      }
      for(i<-language)
        {
          var temp=list
          var count=temp.filter(_(8).equalsIgnoreCase(i)).filter(x=> getintbudget(x(16))>=getintbudget(budget1) && getintbudget(x(16))<=getintbudget(budget2)).size
          lang_count += (i->count)
        }

        val sortlist=lang_count.toList.sortBy(-_._2)
        val sortedMap = mutable.LinkedHashMap(sortlist:_*)
      for ((k, v) <- sortedMap) {
        if(k!="")
        println(k+":"+v)
      }
    }

    checkpoint5("$100","$25000","Romania")
  }

  catch {
    case x: FileNotFoundException=>{
      println(x)
    }
    case y:NumberFormatException=>{
      println(y)
    }
  }


  def main(args: Array[String]): Unit = {
  }
}
