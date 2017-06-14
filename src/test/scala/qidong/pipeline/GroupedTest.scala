package qidong.pipeline
import org.scalatest.FunSuite
class GroupedTest extends FunSuite {
  import qidong.pipeline.ops._
  import fixture._
  
  test("should support grouping missions"){
    val ms = intM =>: intEM =>: intDM
    val ms1 = intM =>: (intEM =>: intDM).name("group")
    val ms2 = (intM =>: intEM).name("group") =>: intDM
  }
  
  test("should support arbitarily nested grouping"){
    val ms = ((intM =>: intEM).name("group1") =>: intDM).name("group2")
    val ms1 = (intM =>: (intEM =>: intDM).name("group")).name("group3")
    val ms2 = (intM =>: (intM =>: intEM).name("group") =>: intDM).name("group4") =>: intDM    
    val ms3 = ((intM =>: (intM =>: intEM).name("group")).name("group5") =>: intDM).name("group4") =>: intDM    
  }
}



