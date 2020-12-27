object Main:

  def testVec = refactoring.vector.test()

  /*
  def testMyMac() = 
    import macrop.MyFirstMacro._
    //myMac("hello world inside macro at compile time")

    
    inline val v = "an identifier, will not show up, or should"
    myMac2(v)
    

    inline def v2 = "inlined identifier, with text"
    myMac2(v2)

  */

  case class Person(name: String, age: Int)

  def testSimpleMacro() = 
    import macrop.SimpleMacro._
    //val mytree =
    transparent inline def myTree = "hello world struct".toUpperCase()
    transparent inline def result = getMyTree(myTree)
    printTheTree(result)
    println(result)

    val onlySubString = getMyTree2("hello again".toUpperCase())
    pprint.pprintln(onlySubString)


    val clName = macrop.classNameOf[Person]
    pprint.pprintln(clName)

  def testQuiller() =
    macrop.quill.QuillerSpec.test()

  def main(args: Array[String]): Unit =
    // testSimpleMacro() // test2
    testQuiller()
    